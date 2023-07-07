******* AR137PD 50.1.11.1.24.1.13 <1155907203>
      ******************************************************************
      * WORKER BENEFIT PLAN MODIFICATIONS
      * 04/05/27 HERWECK - ONLY PROCESS EMPLOYERS, SKIP DIRECT BILL
      * 04/05/27 HERWECK - LIMIT FINANCE CHARGE TO BALANCES OVER $2.00
      * 10/27/10 M. HUNTER - ACS - REAPPLIED ABOVE CUSTOMIZATIONS AFTER
      *                            9.0 APPS UPGRADE.
      * 08/22/11 M. HUNTER - ACS - REAPPLIED ABOVE CUSTOMIZATIONS AFTER
      *                            9.0.1 APPS UPGRADE.
      ******************************************************************      
      ******************************************************************
      *  JT-NUMBER  *   TAG   *        DESCRIPTION                     *
      ******************************************************************
      * JT-1095971  *  095971 * ISSUES WITH CREDIT MEMO HANDLING       *
      *----------------------------------------------------------------*
      * JT-1116658  *  116658 * ISSUE W/SUPPRESS=Y FOR CREDIT INVOICES *
      *----------------------------------------------------------------*
      * JT-1167804  *  167804 * ISSUE W/SUPPRESS=N FOR CUSTOMER WITH   *
      *                       * FINANCE AMNT BELOW CUSTOMER MIN CHARGE *
      * JT-1257937  *         * AR137 FAILS & IS CREATING CORRUPT DATA *
      * JT-1258200  *  258200 * ISSUE W/SUPPRESS=N FOR CUSTOMER WITH   *
      *                       * FINANCE AMNT BELOW CUSTOMER MIN CHARGE *
      * JT-1336027  *  336027 * INTEREST BY DATE CREATES INVOICE FOR   *
      *                       * WRONG AMOUNT WITH SUPPRESS ZEROS = Y   *
      * JT-1433224  *  433224 * AR137 CALCULATING FINANCE CHARGES PRIOR*
      *                       * TO ASSESSMENT DATE                     *
      ******************************************************************
       050-EDIT-PARAMETERS             SECTION 10.
      ******************************************************************
       050-START.

           MOVE PRM-COMPANY            TO DB-COMPANY.
           PERFORM 840-FIND-ACOSET1.
           IF (ARCOMP-NOTFOUND)
               MOVE PRM-COMPANY        TO CRT-ERR-VAR1
               MOVE 50                 TO CRT-ERROR-NBR
               MOVE WS-TRUE            TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

           IF  (ACO-INT-BY-DATE-FL      = "Y")
           AND (PRM-FUTURE-APPS         = "N")
               MOVE 76                 TO CRT-ERROR-NBR
               MOVE WS-TRUE            TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

           MOVE PRM-COMPANY            TO IFCOWS-COMPANY.
           MOVE "AR"                   TO IFCOWS-SYSTEM.
           PERFORM 615-EDIT-COMPANY-60.
           IF (ERROR-FOUND)            
               MOVE WS-TRUE            TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

           IF (PRM-AS-OF-DATE = ZEROES)
               MOVE IFCOWS-CURR-PER-END-DT TO PRM-AS-OF-DATE.

           IF (PRM-AS-OF-DATE NOT > IFCOWS-PRIOR2-PER-END-DT)
               MOVE PRM-AS-OF-DATE     TO CRT-ERR-VAR1
               MOVE 55                 TO CRT-ERROR-NBR
               MOVE WS-TRUE            TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

           IF (ACO-VERIFY-GLDATE        = "Y")
               MOVE PRM-COMPANY         TO IFSCWS-COMPANY
               MOVE "AR"                TO IFSCWS-SYSTEM
               MOVE PRM-AS-OF-DATE      TO IFSCWS-POSTING-DATE 
               PERFORM 620-EDIT-SYSTEM-CODE-60
               IF (ERROR-FOUND)
                   MOVE WS-TRUE         TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END.

J14371**** JT-114371 SELECTING BY PROCESS LEVEL
           IF  (PRM-UPDATE-OPTION   = "R")
J14371     AND (PRM-SEL-PROC-LVL    = SPACES)
               IF (PRM-PROCESS-LEVEL NOT = SPACES)
                   MOVE 56                 TO CRT-ERROR-NBR
                   MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END
               END-IF.

J14371     IF  (PRM-SEL-PROC-LVL    NOT = SPACES)
           AND (PRM-PROCESS-LEVEL   NOT = SPACES)
               MOVE PRM-SEL-PROC-LVL   TO CRT-ERR-VAR1
               MOVE 105                   TO CRT-ERROR-NBR
               MOVE WS-TRUE               TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

J14371     IF  (PRM-SEL-PROC-LVL    NOT = SPACES)
           AND (PRM-PMT-CR-OPTION       = SPACES)
               MOVE PRM-SEL-PROC-LVL      TO CRT-ERR-VAR1
               MOVE 106                   TO CRT-ERROR-NBR
               MOVE WS-TRUE               TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.

J14371     IF (PRM-PMT-CR-OPTION   NOT = SPACES)
               IF  (PRM-UPDATE-OPTION      = "U")
               AND (PRM-PROCESS-LEVEL  NOT = SPACES)
                   MOVE PRM-PROCESS-LEVEL TO CRT-ERR-VAR1
                   MOVE 107               TO CRT-ERROR-NBR
                   MOVE WS-TRUE           TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END
               END-IF
               IF (PRM-SEL-PROC-LVL        = SPACES)
                   MOVE PRM-PMT-CR-OPTION TO CRT-ERR-VAR1
                   MOVE 108               TO CRT-ERROR-NBR
                   MOVE WS-TRUE           TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END
               END-IF
           END-IF.

           IF (PRM-UPDATE-OPTION    = "R")
               IF (PRM-FIN-GL-CODE NOT = SPACES)
                   MOVE 57                 TO CRT-ERROR-NBR
                   MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END.

           IF (PRM-PROCESS-LEVEL = SPACES)
                IF  (PRM-UPDATE-OPTION = "U")
J14371          AND (PRM-SEL-PROC-LVL  = SPACES)
                   MOVE 58                 TO CRT-ERROR-NBR
                   MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END.

           IF (PRM-PROCESS-LEVEL NOT = SPACES)
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE PRM-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-APVSET1
               IF (ARPROCLEVL-NOTFOUND)
                   MOVE PRM-PROCESS-LEVEL  TO CRT-ERR-VAR1
                   MOVE 59                 TO CRT-ERROR-NBR
                   MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 050-END
               ELSE
                   IF (APV-AR-CODE = SPACES)
                       MOVE PRM-PROCESS-LEVEL  TO CRT-ERR-VAR1
                       MOVE 60                 TO CRT-ERROR-NBR
                       MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-END.

           IF (PRM-UPDATE-OPTION = "U")
               IF (PRM-FIN-GL-CODE = SPACES)
                   IF (APV-FIN-GL-CODE = SPACES)
                       MOVE PRM-PROCESS-LEVEL  TO CRT-ERR-VAR1
                       MOVE 61                 TO CRT-ERROR-NBR
                       MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-END.

           IF (PRM-UPDATE-OPTION = "U")
               IF (PRM-FIN-GL-CODE = SPACES)
                   MOVE APV-FIN-GL-CODE        TO PRM-FIN-GL-CODE.

           IF (PRM-UPDATE-OPTION = "U")
               IF (PRM-FIN-GL-CODE NOT = SPACES)
                   MOVE PRM-COMPANY            TO DB-COMPANY
                   MOVE PRM-FIN-GL-CODE        TO DB-AR-CODE
                   PERFORM 840-FIND-RCDSET1
                   IF (ARCODE-NOTFOUND)
                       MOVE PRM-FIN-GL-CODE    TO CRT-ERR-VAR1
                       MOVE 62                 TO CRT-ERROR-NBR
                       MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                       PERFORM 780-PRINT-ERROR-MSG
                       GO TO 050-END.

           IF  (PRM-FIN-CYCLE (1)          = SPACES)
           AND (PRM-FIN-CYCLE (2)          = SPACES)
           AND (PRM-FIN-CYCLE (3)          = SPACES)
           AND (PRM-FIN-CYCLE (4)          = SPACES)
           AND (PRM-FIN-CYCLE (5)          = SPACES)
           AND (PRM-FIN-CYCLE (6)          = SPACES)
           AND (PRM-FIN-CYCLE (7)          = SPACES)
           AND (PRM-FIN-CYCLE (8)          = SPACES)
           AND (PRM-FIN-CYCLE (9)          = SPACES)
           AND (PRM-FIN-CYCLE (10)         = SPACES)
                MOVE 64                 TO CRT-ERROR-NBR
                MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                PERFORM 780-PRINT-ERROR-MSG
                GO TO 050-END.

           PERFORM 051-FIN-CYCLE
               VARYING I1 FROM 1 BY 1
               UNTIL  (I1 > 10)
               OR     (ERROR-FOUND).

           IF (ERROR-FOUND)
               GO TO 050-END. 

           MOVE PRM-COMPANY         TO EDJBHDWS-COMPANY.
           MOVE PRM-JRNL-BOOK-NBR   TO EDJBHDWS-JRNL-BOOK-NBR.
           MOVE "AR"                TO EDJBHDWS-SYSTEM.
           INITIALIZE EDJBHDWS-OPERATOR.
           PERFORM 635-EDIT-JBOOKHDR-70.
           IF (ERROR-FOUND)
               MOVE WS-TRUE             TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               MOVE PRM-JRNL-BOOK-NBR-FN TO CRT-FIELD-NBR
               GO TO 050-END.
            
      **** DATA AND LANGUAGE TRANSLATION
           MOVE PRM-COMPANY         TO DB-COMPANY.
           MOVE PRM-TEXT-CODE       TO DB-TEXT-CODE.
           INITIALIZE                  DB-LANGUAGE-CODE.
           PERFORM 850-FIND-NLT-ANTSET1.
           IF (ARPAYNOTTX-NOTFOUND)
           OR (ANT-COMPANY   NOT = PRM-COMPANY)
           OR (ANT-TEXT-CODE NOT = PRM-TEXT-CODE)
               MOVE 52              TO CRT-ERROR-NBR
               MOVE WS-TRUE         TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
               GO TO 050-END.
      **** END

342466     IF  (PRM-DUE-DATE           NOT = ZEROS)
342466     AND (PRM-DUE-DATE           < WS-SYSTEM-DATE-YMD)
               MOVE PRM-DUE-DATE       TO CRT-ERR-VAR1
               MOVE 68                 TO CRT-ERROR-NBR
               MOVE WS-TRUE            TO WS-PARAMETER-ERROR
               PERFORM 780-PRINT-ERROR-MSG
342466         GO TO 050-END.

       050-END.

      ******************************************************************
       051-FIN-CYCLE                   SECTION 10.
      ******************************************************************

       051-START.

           IF (PRM-FIN-CYCLE (I1)          NOT = SPACES)
               MOVE "F"                    TO DB-CYCLE-TYPE
               MOVE PRM-FIN-CYCLE (I1)     TO DB-CYCLE
               PERFORM 840-FIND-ACYSET1
               IF (ARCYCLE-NOTFOUND)
                   MOVE PRM-FIN-CYCLE (I1) TO CRT-ERR-VAR1
                   MOVE 63                 TO CRT-ERROR-NBR
                   MOVE WS-TRUE            TO WS-PARAMETER-ERROR
                   PERFORM 780-PRINT-ERROR-MSG
                   GO TO 051-END.
               
       051-END.

      ******************************************************************
       100-PROGRAM-CONTROL             SECTION 10.
      ******************************************************************
       100-START.

           PERFORM 1000-OPEN-WORKFLOW-DB.
           PERFORM 600-BEGIN-BROADCAST.
           MOVE "Y"                    TO AR137WS-DO-BASE-BROADCAST.
           MOVE PRM-PRINT-FILE         TO AR137WS-PRINT-FILE-1.

           MOVE 51                     TO CRT-MSG-NBR.
           PERFORM 780-DISPLAY-MSG.

           IF (ACO-DTL-FIN-CHRG        = "Y")
               MOVE "RN"                   TO IFSCATWS-SOURCE-CODE
               PERFORM 625-GET-SC-CATEGORIES-70.

           PERFORM 840-FIND-CKPSET1.
           IF  (CKPOINT-FOUND)
           AND (CKP-RESTART-INFO       NOT = SPACES)
               MOVE CKP-RESTART-INFO   TO WS-RESTART-INFO 
               MOVE WS-TRUE            TO PROGRAM-RESTARTING
           ELSE   
               MOVE WS-FALSE           TO PROGRAM-RESTARTING.

           OPEN OUTPUT FINANCE-FILE.
           OPEN OUTPUT ERROR-FILE.

           IF (PROGRAM-RESTARTING      = WS-TRUE)
               MOVE WS-RESTART-FILENAMEA TO WS-AR137W1-NAME
               MOVE WS-RESTART-FILENAMEB TO WS-AR137W2-NAME
               MOVE WS-RESTART-FILENAMEC TO WS-AR137W3-NAME
               MOVE WS-RESTART-FILENAMED TO WS-AR137W4-NAME
               MOVE WS-RESTART-END-PROG  TO AR137WS-END-PROG
               MOVE WS-RESTART-FANB      TO AR137WS-FANB
               IF (WS-RESTART-PHASE    = 1)
                   MOVE WS-AR137W1-NAME      TO WS-TMP-FILE
                   PERFORM 901-REMOVE-TMP-FILE
                   MOVE WS-AR137W2-NAME      TO WS-TMP-FILE
                   PERFORM 901-REMOVE-TMP-FILE
                   MOVE WS-AR137W3-NAME      TO WS-TMP-FILE
                   PERFORM 901-REMOVE-TMP-FILE
                   MOVE WS-AR137W4-NAME      TO WS-TMP-FILE
                   PERFORM 901-REMOVE-TMP-FILE
               END-IF
               IF (WS-RESTART-PHASE    > 1)
                   GO TO 100-REPORTING.

           PERFORM 900-BUILD-TMP-FILE-NAME.
           MOVE WS-TMP-FILE         TO WS-AR137W1-NAME.
           PERFORM 900-BUILD-TMP-FILE-NAME.
           MOVE WS-TMP-FILE         TO WS-AR137W2-NAME.
           PERFORM 900-BUILD-TMP-FILE-NAME.
           MOVE WS-TMP-FILE         TO WS-AR137W3-NAME.
           PERFORM 900-BUILD-TMP-FILE-NAME.
           MOVE WS-TMP-FILE         TO WS-AR137W4-NAME.

           OPEN I-O AR137W1-FILE.
           OPEN I-O AR137W2-FILE.
           OPEN I-O AR137W3-FILE.
           OPEN I-O AR137W4-FILE.

           IF (PROGRAM-RESTARTING      = WS-FALSE)
               INITIALIZE   RPT-PAGE-COUNT (AR137-R3)
                            AR137WS-ERROR-CNT
               MOVE PRM-COMPANY                TO G3-COMPANY
               MOVE ACO-NAME                   TO G3-NAME
               MOVE ACO-CURRENCY-CD            TO G3-CURRENCY-CD
               MOVE PRM-AS-OF-DATE             TO G3-AS-OF-DATE
               MOVE GN3-ERROR-REPORT           TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           MOVE PRM-AS-OF-DATE             TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           MOVE WSDR-JULIAN-DAYS           TO AR137WS-AS-OF-DAYS.

           PERFORM 910-AUDIT-BEGIN.
           PERFORM 840-MODIFY-CKPSET1.
           MOVE 1                      TO WS-RESTART-PHASE.
           MOVE WS-AR137W1-NAME        TO WS-RESTART-FILENAMEA.
           MOVE WS-AR137W2-NAME        TO WS-RESTART-FILENAMEB.
           MOVE WS-AR137W3-NAME        TO WS-RESTART-FILENAMEC.
           MOVE WS-AR137W4-NAME        TO WS-RESTART-FILENAMED.
           MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
           PERFORM 820-STORE-CKPOINT.
           PERFORM 920-AUDIT-END.

           PERFORM 1000-SEL-REPORT.

           CLOSE AR137W1-FILE.
           CLOSE AR137W2-FILE.
           CLOSE AR137W3-FILE.
           CLOSE AR137W4-FILE.

           PERFORM 910-AUDIT-BEGIN.
           PERFORM 840-MODIFY-CKPSET1.
           MOVE 2                      TO WS-RESTART-PHASE.
           MOVE AR137WS-END-PROG       TO WS-RESTART-END-PROG.
           MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
           PERFORM 820-STORE-CKPOINT.
           PERFORM 920-AUDIT-END.

       100-REPORTING.

           OPEN I-O AR137W1-FILE.
           OPEN I-O AR137W2-FILE.
           OPEN I-O AR137W3-FILE.

           IF (AR137WS-END-PROG             = "N")
               PERFORM 910-AUDIT-BEGIN
               PERFORM 2000-DO-REPORT
               PERFORM 920-AUDIT-END.

           IF (PRM-UPDATE-OPTION            = "Y")
               PERFORM 910-AUDIT-BEGIN
               MOVE "CUST"                  TO MXTSLWS-OBJ-TYPE
               PERFORM 7000-TIMESTAMP-MX-LIST
               PERFORM 920-AUDIT-END
           END-IF.

           CLOSE AR137W1-FILE.
           CLOSE AR137W2-FILE.
           CLOSE AR137W3-FILE.

           MOVE WS-AR137W1-NAME      TO WS-TMP-FILE.
           PERFORM 901-REMOVE-TMP-FILE.
           MOVE WS-AR137W2-NAME      TO WS-TMP-FILE.
           PERFORM 901-REMOVE-TMP-FILE.
           MOVE WS-AR137W3-NAME      TO WS-TMP-FILE.
           PERFORM 901-REMOVE-TMP-FILE.
           MOVE WS-AR137W4-NAME      TO WS-TMP-FILE.
           PERFORM 901-REMOVE-TMP-FILE.
           PERFORM 605-END-BROADCAST.

           IF (AR137WS-ERROR-CNT  = 0)
               MOVE T23-NO-DATA          TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
           END-IF.

           MOVE T33-REPORT-MESSAGE       TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           CLOSE FINANCE-FILE.
           CLOSE ERROR-FILE.

       100-END.

      ******************************************************************
       1000-SEL-REPORT                 SECTION 10.
      ******************************************************************
       1000-START.

           MOVE 500                    TO AR137WS-ELEMENT-TYPE.
           MOVE PRM-COMPANY            TO AR137WS-ELEMENT-VALUE.
           PERFORM 610-BEG-ELEMENT-TAG.

           PERFORM 1005-CREATE-ACM-FILTER
           THRU    1005-END.

           INITIALIZE AR137WS-FIN-CHRG-CD.

           MOVE "N"                    TO AR137WS-END-PROG.
           MOVE PRM-COMPANY            TO DB-COMPANY.
           MOVE ACMSET1-COMPANY        TO WS-DB-BEG-RNG.
           PERFORM 850-FILTER-BEGRNG-ACMSET1.
           PERFORM 1010-SEL-ARO-CUSTOMER
           THRU    1010-END
               UNTIL (ARCUSTOMER-NOTFOUND)
               OR    (ACM-COMPANY      NOT = PRM-COMPANY)
               OR    (AR137WS-END-PROG NOT = "N").

           MOVE 500                    TO AR137WS-ELEMENT-TYPE.
           MOVE PRM-COMPANY            TO AR137WS-ELEMENT-VALUE.
           PERFORM 615-END-ELEMENT-TAG.

           GO TO 1000-END.

      ******************************************************************
       1005-CREATE-ACM-FILTER.
      ******************************************************************

           INITIALIZE                  FILTER-STRING.
           MOVE WS-FALSE               TO AR137WS-MULTI-FIN-CYCLES.

           MOVE 1                      TO AR137WS-I1.

           STRING "(" 
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           STRING FIL-ACM-LATE-PAY-FL
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           STRING "(" 
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (1)  NOT = SPACES)
              MOVE WS-TRUE              TO AR137WS-MULTI-FIN-CYCLES
              STRING FIL-ACM-FIN-CYCLE-1  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (2)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-2  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (3)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-3  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (4)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-4  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (5)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-5  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (6)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-6  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (7)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-7  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (8)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-8  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (9)  NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-9  
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           IF (PRM-FIN-CYCLE (10) NOT = SPACES)
              IF (AR137WS-MULTI-FIN-CYCLES = WS-TRUE)
                  STRING " OR "
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1
              ELSE
                  MOVE WS-TRUE          TO AR137WS-MULTI-FIN-CYCLES
              END-IF
              STRING FIL-ACM-FIN-CYCLE-10 
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           STRING "))" 
                  DELIMITED BY SIZE
                  INTO                  FILTER-STRING
                  POINTER               AR137WS-I1.

           PERFORM 890-CREATE-FILTER.

           IF (PRM-FIN-CYCLE (1)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (1)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (2)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (2)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (3)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (3)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (4)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (4)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (5)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (5)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (6)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (6)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (7)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (7)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (8)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (8)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (9)  NOT = SPACES)
               MOVE PRM-FIN-CYCLE (9)  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

           IF (PRM-FIN-CYCLE (10) NOT = SPACES)
               MOVE PRM-FIN-CYCLE (10) TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

       1005-END.

      ******************************************************************
       1010-SEL-ARO-CUSTOMER.
      ******************************************************************

           INITIALIZE                    AR137WS-W1-REC-CNT.

      **** DATA AND LANGUAGE TRANSLATION
           MOVE PRM-COMPANY           TO DB-COMPANY.
           MOVE PRM-TEXT-CODE         TO DB-TEXT-CODE.
           MOVE ACM-LANGUAGE-CODE     TO DB-LANGUAGE-CODE.
           PERFORM 840-FIND-ANTSET1.
           IF (ARPAYNOTTX-NOTFOUND)
               MOVE ACM-CUSTOMER       TO D3-ERR-CUSTOMER
               MOVE PRM-TEXT-CODE      TO D3-FIELD-NAME
               MOVE 204                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               MOVE D31-MESSAGE-ERRORS TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                 TO AR137WS-ERROR-CNT
               GO TO 1010-NEXT-ARCUSTOMER.
      **** END
               
           MOVE ACM-CUSTOMER           TO AR137WS-ARO-CUSTOMER.

           INITIALIZE AR137WS-88
                      AR137WS-87
                      AR137WS-PNMT
                      AR137WS-FNET
                      AR137WS-ORIG-AMT
                      AR137WS-CRED
                      AR137WS-PRLSV
                      AR137WS-PRLSV-SV.

           INITIALIZE AR137WS-CHRG-TOT-AMT
                      AR137WS-PNMT-CUST
                      AR137WS-NBR-LINES-CUST
                      AR137WS-AVG-CHRG-RATE-CUST
                      AR137WS-NBR-OF-DAYS-CUST
                      AR137WS-AMT-BY-DATE-CUST
                      AR137WS-CHRG-EXIST
474744                AR137WS-CUST-AMT.

           MOVE "N"                        TO AR137WS-PRL-SV.
           IF (ACM-FIN-CHRG-CD             NOT = AR137WS-FIN-CHRG-CD)
               MOVE ACM-FIN-CHRG-CD        TO DB-FIN-CHRG-CD
               MOVE ACM-FIN-CHRG-CD        TO AR137WS-FIN-CHRG-CD
               PERFORM 840-FIND-ARFSET1
               IF (ARFINANCE-NOTFOUND)
                   MOVE ACM-CUSTOMER       TO D3-ERR-CUSTOMER
                   MOVE ACM-FIN-CHRG-CD    TO D3-FIELD-NAME
                   MOVE 203                TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE        TO D3-FIELD-DATA-ERROR
                   MOVE D31-MESSAGE-ERRORS TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
                   ADD  1                  TO AR137WS-ERROR-CNT
                   GO TO 1010-NEXT-ARCUSTOMER.

342466     MOVE ARF-COUNTRY-CODE           TO AR137WS-COUNTRY-CODE. 
           MOVE WS-FALSE               TO AR137WS-EDIT-CURR.
           INITIALIZE                     AR137WS-INVOICE-SV.
      
           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE ACM-CUSTOMER               TO DB-CUSTOMER.

      **** CALCULATE TOTAL OF UNAPPLIED PAYMENTS AND CREDIT MEMOS
      **** FOR EACH ARCUSTOMER

           IF (ACO-INT-BY-DATE-FL              = "N")
               INITIALIZE                     AR137WS-CRED
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE ACM-CUSTOMER           TO DB-CUSTOMER
J14371         IF (PRM-PMT-CR-OPTION  NOT = "C")
                   IF (PRM-FUTURE-APPS         = "N")
                       MOVE APMSET6-CUSTOMER   TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-APMSET6
                   ELSE 
                       MOVE APMSET8-CUSTOMER   TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-APMSET8
                   END-IF
                   PERFORM 1011-UNAPPLIED-PAYMENTS
                   THRU    1011-END
                       UNTIL (ARPAYMENT-NOTFOUND)
J14371         END-IF
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE ACM-CUSTOMER           TO AR137WS-CUSTOMER
                                              DB-CUSTOMER
               MOVE "C"                    TO DB-TRANS-TYPE
J14371         IF (PRM-PMT-CR-OPTION  NOT = "P")
                   IF (PRM-FUTURE-APPS    = "N")
                       MOVE AROSET3-TRANS-TYPE TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-AROSET3
                   ELSE
                       MOVE AROSET6-TRANS-TYPE TO WS-DB-BEG-RNG
                       PERFORM 850-FIND-BEGRNG-AROSET6
                   END-IF
                   PERFORM 1012-CREDIT-MEMO
                   THRU    1012-END
                       UNTIL (AROITEMS-NOTFOUND)
                       OR    (ARO-COMPANY      NOT = PRM-COMPANY)
                       OR    (ARO-CUSTOMER     NOT = AR137WS-CUSTOMER)
                       OR    (ARO-TRANS-TYPE   NOT = "C").
                      
           PERFORM 1013-CREATE-ARO-FILTER
           THRU    1013-END.

           IF (PRM-FUTURE-APPS     = "N")
               MOVE AROSET3-CUSTOMER   TO WS-DB-BEG-RNG
               PERFORM 850-FILTER-BEGRNG-AROSET3
           ELSE
               MOVE AROSET6-CUSTOMER   TO WS-DB-BEG-RNG
               PERFORM 850-FILTER-BEGRNG-AROSET6.

           MOVE WS-FALSE               TO AR137WS-ARFINRATE-FOUND.
           PERFORM 1015-SEL-AROITEMS
           THRU    1015-END
               UNTIL (AROITEMS-NOTFOUND)
               OR    (ARO-CUSTOMER NOT = AR137WS-ARO-CUSTOMER)
               OR    (AR137WS-END-PROG NOT = "N").

      **** CURRENCY CODE VALIDATION ERROR
           IF (AR137WS-END-PROG        = "Y")
               GO TO 1010-END.

           IF  (AR137WS-CRED       NOT = ZEROES)
           AND (ACM-FIN-CALC-TYPE      = "D")    
      **** REMOVE INVOICES FROM WORKFILE IF PAYMENTS/CREDITS EXIST
               MOVE PRM-COMPANY              TO WF-COMPANY   
               MOVE AR137WS-ARO-CUSTOMER     TO WF-CUSTOMER
               MOVE ZEROES                   TO WF-DUE-DATE
                                                WF-SEQ-NBR
                                                WF-PAYMENT-SEQ
               MOVE SPACES                   TO WF-TRANS-TYPE
                                                WF-TRANS-NBR
               MOVE WS-FALSE                 TO AR137WS-FILE
               PERFORM 8500-READ-NLT-AR137W1
               PERFORM 1016-PAYMENT-CREDITS
               THRU    1016-END
                   UNTIL (AR137-NOTFOUND)
                   OR    (AR137WS-CRED    = ZEROES)
                   OR    (WF-COMPANY  NOT = PRM-COMPANY)
                   OR    (WF-CUSTOMER NOT = AR137WS-ARO-CUSTOMER)
           END-IF.

           MOVE ARZSET2-CUSTOMER       TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-ARZSET2.
           PERFORM 1017-SEL-ARITEMAUD
           THRU    1017-END
               UNTIL (ARITEMAUD-NOTFOUND)
               OR    (ARZ-COMPANY      NOT = PRM-COMPANY)
               OR    (ARZ-CUSTOMER     NOT = AR137WS-ARO-CUSTOMER)
               OR    (AR137WS-END-PROG NOT = "N").

           IF  (AR137WS-CRED       NOT = ZEROES)
           AND (ACM-FIN-CALC-TYPE      = "D")    
      **** REMOVE INVOICES FROM WORKFILE IF PAYMENTS/CREDITS EXIST
               MOVE PRM-COMPANY              TO WF-COMPANY   
               MOVE AR137WS-ARO-CUSTOMER     TO WF-CUSTOMER
               MOVE ZEROES                   TO WF-DUE-DATE
                                                WF-SEQ-NBR
                                                WF-PAYMENT-SEQ
               MOVE SPACES                   TO WF-TRANS-TYPE
                                                WF-TRANS-NBR
               MOVE WS-FALSE                 TO AR137WS-FILE
               PERFORM 8500-READ-NLT-AR137W1
               PERFORM 1016-PAYMENT-CREDITS
               THRU    1016-END
                   UNTIL (AR137-NOTFOUND)
                   OR    (AR137WS-CRED    = ZEROES)
                   OR    (WF-COMPANY  NOT = PRM-COMPANY)
                   OR    (WF-CUSTOMER NOT = AR137WS-ARO-CUSTOMER)
           END-IF.

           IF  (ACO-INT-BY-DATE-FL         = "Y")
           AND (ACM-LATE-PAY-FL            = "B")
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE ACM-CUSTOMER           TO DB-CUSTOMER
               IF (PRM-FUTURE-APPS         = "N")
                   MOVE APMSET6-CUSTOMER   TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-APMSET6
               ELSE 
                   MOVE APMSET8-CUSTOMER   TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-APMSET8
               END-IF
               IF (ARPAYMENT-FOUND)
                   PERFORM 1115-OPEN-PAYMENTS
                   THRU    1115-END
                       UNTIL (ARPAYMENT-NOTFOUND)
               END-IF
           END-IF.

           IF (AR137WS-END-PROG            = "Y")
               GO TO 1010-END.

           IF  (ACO-INT-BY-DATE-FL         = "Y")
           AND (ACM-LATE-PAY-FL            = "L")
               IF (AR137WS-W1-REC-CNT     > ZEROS)
020435*---JT-1020435
020435             IF (PRM-SUPPRESS-ZEROS = "Y")
020435                 CONTINUE
020435             ELSE
020435                 PERFORM 1046-FINANCE-NET    
020435                 THRU    1046-END
020435                 GO TO 1010-NEXT-ARCUSTOMER
020435             END-IF
               ELSE
                   GO TO 1010-NEXT-ARCUSTOMER
               END-IF.

258200     IF (ACM-LATE-PAY-FL  = "B")
258200        IF (PRM-SUPPRESS-ZEROS = "Y")
258200            CONTINUE
258200        ELSE
258200            PERFORM 1046-FINANCE-NET
258200            THRU    1046-END
258200            GO TO 1010-NEXT-ARCUSTOMER
258200        END-IF
258200     END-IF.

      **** PRINT FINANCE CHARGE CODE ERROR AND READ NEXT ARCUSTOMER
      **** ELSE COMPUTE NET FINANCE CHARGE FOR ARCUSTOMER.
           IF (ACM-FIN-CALC-TYPE                = "N")
               SUBTRACT AR137WS-CRED FROM AR137WS-PNMT
               IF (AR137WS-PNMT             NOT >  ZEROES)
                   INITIALIZE                      AR137WS-FNET
                   GO TO 1010-NEXT-ARCUSTOMER
               ELSE
                   MOVE ACM-FIN-CHRG-CD         TO DB-FIN-CHRG-CD
                   MOVE PRM-AS-OF-DATE          TO DB-EFF-DATE
                   PERFORM 850-FIND-NLT-AFISET2
                   IF (ARFINRATE-NOTFOUND)
                   OR (AFI-FIN-CHRG-CD      NOT =  ACM-FIN-CHRG-CD)
                       MOVE ACM-CUSTOMER        TO D3-ERR-CUSTOMER
                       MOVE ACM-FIN-CHRG-CD     TO D3-FIELD-NAME
                       MOVE 205                 TO CRT-MSG-NBR
                       PERFORM 790-GET-MSG
                       MOVE CRT-MESSAGE         TO D3-FIELD-DATA-ERROR
                       MOVE D31-MESSAGE-ERRORS  TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
                       ADD  1                   TO AR137WS-ERROR-CNT
                       GO TO 1010-NEXT-ARCUSTOMER
                   ELSE
RAY   *--- 04/05/27 HERWECK LIMIT FINANCE CHARGE TO BALANCES OVER $2.00
RAY                    IF (AR137WS-PNMT > 2.00)
RAY   *-----------------------------------------------------------------
                       COMPUTE AR137WS-FNET ROUNDED = AR137WS-PNMT
                                                    * AFI-CHRG-RATE
                   END-IF
               END-IF
           END-IF.

           PERFORM 1046-FINANCE-NET
           THRU    1046-END.
               
474744     IF (AR137WS-CUST-AMT              = ZERO)
474744     OR (AR137WS-CUST-AMT              < ACM-FIN-MIN-CHRG)
474744         MOVE PRM-COMPANY              TO WF-COMPANY   
474744         MOVE AR137WS-ARO-CUSTOMER     TO WF-CUSTOMER
474744         MOVE ZERO                     TO WF-PRINT-FL
787215         IF (ACO-CURRENCY-CD NOT = ACM-CURRENCY-CD)
787215            MOVE IFCCWS-CURRENCY-ND    TO WF-ORIG-ND
787215         ELSE
787215            MOVE ACO-BASE-ND           TO WF-ORIG-ND
787215         END-IF
474744         INITIALIZE WF-DUE-DATE
474744                    WF-SEQ-NBR
474744                    WF-TRANS-TYPE
474744                    WF-TRANS-NBR
474744                    WF-PAYMENT-SEQ
474744         WRITE AR137W1-REC.

       1010-NEXT-ARCUSTOMER.

           PERFORM 860-FIND-NXTRNG-ACMSET1.

       1010-END.

      ******************************************************************
       1011-UNAPPLIED-PAYMENTS.
      ******************************************************************

J14371     IF  (PRM-PMT-CR-OPTION       = "B" OR "P")
           AND (APM-PROCESS-LEVEL   NOT = PRM-SEL-PROC-LVL)
               GO TO 1011-FIND-NEXT-ARPAYMENT.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE APM-BATCH-NBR              TO DB-BATCH-NBR.
           PERFORM 840-FIND-APHSET1.

           IF (APH-DEPOSIT-DATE            > PRM-AS-OF-DATE)
               GO TO 1011-FIND-NEXT-ARPAYMENT.

           IF  (APM-CANCEL-DATE        NOT = ZEROS)
           AND (APM-CANCEL-DATE        NOT > PRM-AS-OF-DATE)
               GO TO 1011-FIND-NEXT-ARPAYMENT.

           IF  (APM-TRANSFER-DATE      NOT = ZEROS)
           AND (APM-CUSTOMER               = APM-TRNS-CUST)
           AND (APM-TRANSFER-DATE          > PRM-AS-OF-DATE)
               GO TO 1011-FIND-NEXT-ARPAYMENT.

           IF  (APM-TRANSFER-DATE      NOT = ZEROS)
           AND (APM-CUSTOMER           NOT = APM-TRNS-CUST)
J19179     AND (APM-TRANSFER-AMT           = APM-ORIG-AMT)
J19179*    AND (APM-TRANSFER-AMT       NOT = APM-TRAN-AMT)
               IF (APM-TRANSFER-DATE   NOT > PRM-AS-OF-DATE)
                   GO TO 1011-FIND-NEXT-ARPAYMENT.

           MOVE APM-TRAN-AMT           TO AROPWS-OPEN-AMT.
           MOVE PRM-AS-OF-DATE         TO AROPWS-POST-DATE.
           PERFORM 720-OPEN-PAYMENT.
           MOVE AROPWS-OPEN-AMT        TO AR137WS-OPENC.
           ADD AR137WS-OPENC           TO AR137WS-CRED.

       1011-FIND-NEXT-ARPAYMENT.

           IF (PRM-FUTURE-APPS     = "N")
               PERFORM 860-FIND-NXTRNG-APMSET6
           ELSE
               PERFORM 860-FIND-NXTRNG-APMSET8.

       1011-END.

      ******************************************************************
       1012-CREDIT-MEMO.
      ******************************************************************

J14371     IF  (PRM-PMT-CR-OPTION       = "B" OR "C")
           AND (ARO-PROCESS-LEVEL   NOT = PRM-SEL-PROC-LVL)
               GO TO 1012-FIND-NEXT-ARCRMEMO.

           MOVE ARO-ALT-TYPE           TO DB-TRANS-TYPE.
           MOVE ARO-INVOICE            TO DB-INVOICE.

           IF (ARO-GL-DATE > PRM-AS-OF-DATE)
               GO TO 1012-FIND-NEXT-ARCRMEMO.

J19179     IF (ARO-DUE-DATE > PRM-AS-OF-DATE)
J19179         GO TO 1012-FIND-NEXT-ARCRMEMO
J19179     END-IF.
J19179
           IF (ARO-CANCEL-FLAG             = "Y")
               MOVE ARO-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
               PERFORM 840-FIND-ARZSET1
               IF  (ARITEMAUD-FOUND)
               AND (ARZ-CANCEL-DATE        > PRM-AS-OF-DATE)
                    GO TO 1012-FIND-NEXT-ARCRMEMO.

           MOVE ARO-TRAN-AMT               TO AROPWS-OPEN-AMT.
           MOVE PRM-AS-OF-DATE             TO AROPWS-POST-DATE.
           IF (ARO-APPLIED-SEQ            NOT = ZEROES)
               PERFORM 710-OPEN-CREDIT.

           MOVE AROPWS-OPEN-AMT            TO AR137WS-OPENC.
           ADD AR137WS-OPENC               TO AR137WS-CRED.

       1012-FIND-NEXT-ARCRMEMO.

           IF (PRM-FUTURE-APPS    = "N")
               PERFORM 860-FIND-NXTRNG-AROSET3
           ELSE
               PERFORM 860-FIND-NXTRNG-AROSET6.

       1012-END.

      ******************************************************************
       1013-CREATE-ARO-FILTER.
      ******************************************************************

           INITIALIZE                  FILTER-STRING.
           MOVE 1                      TO AR137WS-FILTER-LENGTH.

000630     STRING "("                     DELIMITED BY SIZE
000640         INTO FILTER-STRING
               POINTER AR137WS-FILTER-LENGTH.
           STRING "(ARO-BANK-INST-TYPE !=?)" DELIMITED BY SIZE
               INTO FILTER-STRING
               POINTER AR137WS-FILTER-LENGTH.
           STRING "AND (ARO-DUE-DATE !=?)" DELIMITED BY SIZE
               INTO FILTER-STRING
               POINTER AR137WS-FILTER-LENGTH.
           IF (ACO-INT-BY-DATE-FL       = "N")
               STRING "AND (ARO-TRANS-TYPE !=?)" DELIMITED BY SIZE
               INTO FILTER-STRING
               POINTER AR137WS-FILTER-LENGTH.
           IF (PRM-PROCESS-LEVEL    NOT = SPACES)
               STRING "AND (ARO-PROCESS-LEVEL =?)" DELIMITED BY SIZE
               INTO FILTER-STRING
               POINTER AR137WS-FILTER-LENGTH.
000880     STRING ")"                     DELIMITED BY SIZE
000640         INTO FILTER-STRING
               POINTER AR137WS-FILTER-LENGTH.

           PERFORM 890-CREATE-FILTER.

           MOVE "D"                 TO ALPHANUM-FILTER-VALUE.
           PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
           MOVE PRM-AS-OF-DATE          TO DATETIME-FILTER-VALUE.
           PERFORM 890-SET-DATETIME-FILTER-VALUE.
           IF (ACO-INT-BY-DATE-FL       = "N")
               MOVE "C"             TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.
           IF (PRM-PROCESS-LEVEL     NOT = SPACES)
               MOVE PRM-PROCESS-LEVEL   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE.

       1013-END.

      ******************************************************************
       1015-SEL-AROITEMS.    
      ******************************************************************

J14371     IF  (PRM-SEL-PROC-LVL   NOT = SPACES)
           AND (ARO-PROCESS-LEVEL  NOT = PRM-SEL-PROC-LVL)
               GO TO 1015-FIND-NEXT-AROITEMS.

           IF  (ARO-LATE-CH-FL  = "N")
           AND (ACM-LATE-PAY-FL = "L")
               GO TO 1015-FIND-NEXT-AROITEMS.
 
           IF  (PRM-INCLUDE-INT          = "N")
           AND (ARO-ORIG-CODE            = "F")
               GO TO 1015-FIND-NEXT-AROITEMS.

           IF (ARO-CANCEL-FLAG             = "Y")
               MOVE ARO-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
               PERFORM 840-FIND-ARZSET1
               IF  (ARITEMAUD-FOUND)
               AND (ARZ-CANCEL-DATE        > PRM-AS-OF-DATE)
                    GO TO 1015-FIND-NEXT-AROITEMS.

           IF (ARO-INVOICE NOT = AR137WS-INVOICE-SV)
               INITIALIZE                 WS-CHARGE-BY-DATE-INFO
               MOVE ARO-INVOICE        TO AR137WS-INVOICE-SV.

RAY   *--- 04/05/27 HERWECK - ONLY PROCESS EMPLOYERS, SKIP DIRECT BILL -
           IF ((ARO-CUSTOMER(3:2) = "ER")
           AND (ARO-CUSTOMER(5:5) NUMERIC))
               CONTINUE
           ELSE
102104         GO TO 1015-FIND-NEXT-AROITEMS.
      *--- END WBP MOD -------------------------------------------------

      **** "7" = FULLY APPLIED TRANSACTIONS
      **** THE FOLLOWING CODE IS CHECKING FOR WRITE-OFFS

           INITIALIZE   AR137WS-ADJ-AMT.

           IF  (ARO-STATUS                 = "7")
           AND (ARO-TRANS-TYPE         NOT = "C")
               MOVE ARO-COMPANY            TO DB-COMPANY
               MOVE ARO-TRANS-TYPE         TO DB-TRANS-TYPE
               MOVE ARO-INVOICE            TO DB-INVOICE  
               MOVE ARO-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
               MOVE ARASET1-PAYMENT-SEQ    TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-ARASET1
               PERFORM
                   UNTIL (ARAPPLIED-NOTFOUND)
433224             IF (ARO-LAST-FC-DATE > ARA-GL-DATE)
433224                 GO TO 1015-FIND-NEXT-AROITEMS
433224             END-IF
                   IF (ARAPPLIED-FOUND)
                       ADD ARA-ADJ-AMT     TO AR137WS-ADJ-AMT
                   END-IF
                   PERFORM 860-FIND-NXTRNG-ARASET1
               END-PERFORM
               IF (ARO-TRAN-AMT            = AR137WS-ADJ-AMT)
                   GO TO 1015-FIND-NEXT-AROITEMS
               END-IF
           END-IF.

           IF  (ARO-STATUS                 = "7")
           AND (ARO-TRANS-TYPE             = "C")
               MOVE ARO-COMPANY            TO DB-COMPANY
               MOVE ARO-TRANS-TYPE         TO DB-TRANS-TYPE
               MOVE ARO-INVOICE            TO DB-INVOICE
               MOVE ARO-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
               MOVE ARASET1-PAYMENT-SEQ    TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-ARASET1
               IF (ARAPPLIED-NOTFOUND)
                   GO TO 1015-FIND-NEXT-AROITEMS
               END-IF
           END-IF.

      **** "8" = CLOSED TRANSACTIONS (WITH FINANCE CHARGES)
           IF  (ARO-STATUS           = 8)
           AND (ARO-LAST-FC-DATE NOT = ZEROES)
               GO TO 1015-FIND-NEXT-AROITEMS.

      **** "8" = CLOSED TRANSACTIONS (WITHOUT FINANCE CHARGES)
           IF  (ARO-STATUS       = 8)
           AND (ARO-LAST-FC-DATE = ZEROES)
           AND (ACM-LATE-PAY-FL  = "F")
               GO TO 1015-FIND-NEXT-AROITEMS.

      **** NOTE:  IF ACM-LATE-PAY-FL = "L" (LATE CHARGES)
      ****           ACM-LATE-PAY-FL = "B" (LATE AND FINANCE CHARGES)
      ****        THEN
      ****           ACM-FIN-CALC-TYPE ***MUST*** = "D" (DETAIL METHOD)

      **** CALCULATE DUE DATE AND JULIAN ADJUSTED DUE DATE FOR
      **** FINANCE CHARGES
      **** "F" = ACCESS FINANCE CHARGES ONLY
      **** "B" = ACCESS FINANCE CHARGES AND LATE PAYMENT CHARGES
      **** 
           IF (ACM-LATE-PAY-FL = "F" OR "B")
               IF (ARO-LAST-FC-DATE            = ZEROES)
                   MOVE ARO-DUE-DATE           TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   IF (ACO-INT-BY-DATE-FL = "N")
                       ADD ACM-FIN-GRAC-DAYS   TO WSDR-JULIAN-DAYS
                   END-IF
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-FIN
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO AR137WS-DUDT-FIN
               ELSE
                   IF (ARO-LAST-FC-DATE        > ARO-DUE-DATE)
                       MOVE ARO-LAST-FC-DATE   TO AR137WS-DUDT-FIN
                       MOVE ARO-LAST-FC-DATE   TO WSDR-FR-DATE
                   ELSE
                       MOVE ARO-DUE-DATE       TO AR137WS-DUDT-FIN
                       MOVE ARO-DUE-DATE       TO WSDR-FR-DATE
                   END-IF
                   PERFORM 900-DATE-TO-JULIAN
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-FIN
               END-IF
           END-IF.

      **** CALCULATE DUE DATE AND JULIAN ADJUSTED LATE DATE FOR
      **** LATE CHARGES
      **** "L" = ACCESS LATE PAYMENT CHARGES ONLY
      **** "B" = ACCESS LATE PAYMENT CHARGES AND FINANCE CHARGES
      ****
           IF (ACO-INT-BY-DATE-FL    = "N")
               IF (ACM-LATE-PAY-FL   = "L" OR "B")
                   MOVE ARO-DUE-DATE           TO WSDR-FR-DATE
                   IF (ACM-LATE-PAY-FL         = "B")
                       IF (ARO-LAST-FC-DATE    > ARO-DUE-DATE)
                           MOVE ARO-LAST-FC-DATE TO WSDR-FR-DATE
                       END-IF
                   END-IF
                   PERFORM 900-DATE-TO-JULIAN
                   IF (ARO-LAST-FC-DATE = ZEROES)
                       ADD ACM-FIN-GRAC-DAYS   TO WSDR-JULIAN-DAYS
                   END-IF
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-LATE
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO AR137WS-DUDT-LATE
               END-IF
           ELSE
               IF (ACM-LATE-PAY-FL = "L")
                   MOVE ARO-DUE-DATE           TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-LATE
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO AR137WS-DUDT-LATE
               END-IF
           END-IF.

           IF (PRM-UPDATE-OPTION         = "U")
               IF (AR137WS-PRL-SV        = "N")
                   IF (AR137WS-PRLSV-SV  = SPACES)
                       MOVE ARO-PROCESS-LEVEL TO AR137WS-PRLSV-SV
                   ELSE
                       IF (AR137WS-PRLSV-SV NOT = ARO-PROCESS-LEVEL)
                           MOVE "Y"      TO AR137WS-PRL-SV.

           IF  (ACO-CURRENCY-CD      NOT = ACM-CURRENCY-CD)
           AND (AR137WS-EDIT-CURR        = WS-FALSE)
               PERFORM 1050-EDIT-CURRENCY
               THRU    1050-END
               IF (ERROR-FOUND)
                   GO TO 1015-END.

      **** COMPUTE ONLY LATE CHARGES (ACM-LATE-PAY-FL = "L")
           IF (ACM-LATE-PAY-FL             = "L")
               MOVE "N"                 TO AR137WS-ARO-TOTAL-FIN-CHRG
               GO TO 1015-LATE-PAY-ONLY.

      **** COMPUTE LATE CHARGE AND FINANCE CHARGE
      **** (ACM-LATE-PAY-FL = "F" OR "B")
      ****  
      **** COMPUTE LATE CHARGES (ACM-LATE-PAY-FL = "B")
      ****
           INITIALIZE AR137WS-APAM.
           
           IF (ACO-INT-BY-DATE-FL  = "N")
               MOVE "Y"                            TO AR137WS-RECF
               IF (ARO-APPLIED-SEQ         NOT = ZEROES)
                   MOVE "N"                    TO AR137WS-WORKB-DONE
                   MOVE ARO-COMPANY            TO DB-COMPANY
                   MOVE ARO-TRANS-TYPE         TO DB-TRANS-TYPE
                   MOVE ARO-INVOICE            TO DB-INVOICE
                   MOVE ARO-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
                   MOVE ARO-BATCH-NBR          TO DB-BATCH-NBR
                   MOVE ARASET1-BATCH-NBR      TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-ARASET1
                   PERFORM 1020-CALC-ARO-OPEN-AMT
                   THRU    1020-END
                       UNTIL (ARAPPLIED-NOTFOUND)
                       OR    (AR137WS-RECF     NOT = "Y")
                       OR    (AR137WS-END-PROG NOT = "N")
               END-IF
               IF (AR137WS-END-PROG = "Y")
                   GO TO 1015-END
               END-IF
               IF (AR137WS-RECF     = "N")
                   GO TO 1015-FIND-NEXT-AROITEMS
               END-IF
           END-IF.

           IF (ACO-INT-BY-DATE-FL        = "N")
               COMPUTE AR137WS-DPF       = AR137WS-AS-OF-DAYS
                                         - AR137WS-LDAT-FIN
               IF (AR137WS-DPF           <= ZEROES)
                   GO TO 1015-FIND-NEXT-AROITEMS
               END-IF
           END-IF.

      **** DISPUTES
           IF (ACM-DISPUTES-FIN          = "N")
               IF (ARO-DISPUTE-SEQ       NOT = ZEROES)
                   MOVE PRM-COMPANY          TO DB-COMPANY
                   MOVE ARO-CUSTOMER         TO DB-CUSTOMER
                   MOVE ARO-TRANS-TYPE       TO DB-TRANS-TYPE
                   MOVE ARO-INVOICE          TO DB-INVOICE
                   MOVE ARO-PAYMENT-SEQ      TO DB-PAYMENT-SEQ
                   MOVE ADPSET1-PAYMENT-SEQ  TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-ADPSET1
                   PERFORM 1030-DISPUTES
                   THRU    1030-END
                       UNTIL (ARDISPUTE-NOTFOUND).

           IF (AR137WS-APAM              = ARO-TRAN-AMT)
               GO TO 1015-FIND-NEXT-AROITEMS.

           COMPUTE AR137WS-CPNMT ROUNDED = ARO-TRAN-AMT
                                         - AR137WS-APAM.

           IF (ACO-INT-BY-DATE-FL            = "N")
               GO TO 1015-CONT.

           MOVE "N"                          TO AR137WS-LATE-CHRG.

      ****  COMPUTE FINANCE CHARGES (ACM-LATE-PAY-FL = "F" OR "B")
      ****
           IF (AR137WS-LATE-CHRG       = "Y")
               COMPUTE AR137WS-DPF     = AR137WS-AS-OF-DAYS
                                       - AR137WS-LDAT-FIN
                                       - 1
           ELSE
      *** SUBTRACT GRACE DAYS FOR INVOICE SELECTION CRITERIA FOR FINANCE
      *** CHARGE CALCULATION
               IF (ARO-LAST-FC-DATE    = ZEROES)
                   COMPUTE AR137WS-DPF = AR137WS-AS-OF-DAYS
                                       - AR137WS-LDAT-FIN
                                       - ACM-FIN-GRAC-DAYS
               ELSE
                   COMPUTE AR137WS-DPF = AR137WS-AS-OF-DAYS
                                       - AR137WS-LDAT-FIN
               END-IF
           END-IF.

           IF (AR137WS-DPF             <= ZEROES)
               GO TO 1015-FIND-NEXT-AROITEMS
           END-IF.

      **** ADD GRACE DAYS BACK IN FOR FINANCE CHARGE CALCULATION
           IF  (AR137WS-LATE-CHRG  NOT = "Y")
           AND (ARO-LAST-FC-DATE       = ZEROES)
               COMPUTE AR137WS-DPF     = AR137WS-DPF
                                       + ACM-FIN-GRAC-DAYS
           END-IF.

           IF  (ACM-LATE-PAY-FL        = "B")
           AND (AR137WS-LATE-CHRG      = "Y")
               GO TO 1015-FIND-NEXT-AROITEMS
           END-IF.
         
           MOVE "N"                    TO AR137WS-APPLD-EXIST
                                          AR137WS-ARO-TOTAL-FIN-CHRG.

           IF (ACM-LATE-PAY-FL         = "B")
               MOVE ARO-COMPANY        TO DB-COMPANY
               MOVE ARO-TRANS-TYPE     TO DB-TRANS-TYPE
               MOVE ARO-INVOICE        TO DB-INVOICE
               MOVE ARO-PAYMENT-SEQ    TO DB-PAYMENT-SEQ
               MOVE ARO-BATCH-NBR      TO DB-BATCH-NBR
               MOVE ARASET1-BATCH-NBR  TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-ARASET1
               IF  (ARAPPLIED-NOTFOUND)
                   MOVE "Y"            TO AR137WS-ARO-TOTAL-FIN-CHRG
                   MOVE AR137WS-CPNMT  TO AR137WS-OPEN-AMT
                   PERFORM 1020-CALC-ARO-OPEN-AMT
                   THRU    1020-END
               ELSE
                   INITIALIZE             WS-CHARGE-FC-INFO
                                          I2
                                          AR137WS-FIRST-APPLD
                                          AR137WS-TABLE-CREATED
                                          AR137WS-HOLD-TRANS-TYPE
                                          AR137WS-HOLD-INVOICE
                                          AR137WS-HOLD-PAYMENT-SEQ
                                          AR137WS-HOLD-COMPANY
                                          AR137WS-HOLD-BATCH-NBR
                                          AR137WS-HOLD-APP-SEQ
                                          AR137WS-NO-RECS
                   MOVE 1              TO I3
                                          AR137WS-TABLE-SORT
                   MOVE 0              TO AR137WS-TABLE-CONTROL
                   PERFORM 1019-CREATE-FC-TABLE
                   THRU    1019-END
                       UNTIL (ARAPPLIED-NOTFOUND)
               END-IF
           END-IF.

           IF  (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-TABLE-CREATED       NOT = "Y")
           AND (AR137WS-NO-RECS                 = "N")
               ADD 1                        TO I2
               IF (ARO-LAST-FC-DATE         = ZEROES)
                   MOVE ARO-DUE-DATE        TO AR137WS-FC-FROM-DATE (I2)
               ELSE
                   MOVE ARO-LAST-FC-DATE    TO AR137WS-FC-FROM-DATE (I2)
               END-IF
               MOVE PRM-AS-OF-DATE          TO AR137WS-FC-TO-DATE (I2)
               MOVE AR137WS-OPEN-AMT        TO AR137WS-FC-OPEN-AMT (I2)
               MOVE AR137WS-HOLD-TRANS-TYPE TO 
                                              AR137WS-FC-TRANS-TYPE (I2)
               MOVE AR137WS-HOLD-INVOICE    TO AR137WS-FC-INVOICE (I2)
               MOVE AR137WS-HOLD-PAYMENT-SEQ TO
                                             AR137WS-FC-PAYMENT-SEQ (I2)
               MOVE AR137WS-HOLD-COMPANY    TO AR137WS-FC-COMPANY (I2)
               MOVE AR137WS-HOLD-BATCH-NBR  TO AR137WS-FC-BATCH-NBR (I2)
               MOVE AR137WS-HOLD-APP-SEQ    TO AR137WS-FC-APP-SEQ (I2)
               MOVE "Y"                     TO AR137WS-FC-LAST-REC (I2)
               MOVE I2                      TO AR137WS-MAX-FC-TABLE
           END-IF.

           IF  (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-TABLE-CREATED           = "Y")
               IF (AR137WS-OPEN-AMT         NOT = ZEROES)
                   MOVE PRM-AS-OF-DATE      TO AR137WS-FC-TO-DATE (I2)
               END-IF
               MOVE "Y"                     TO AR137WS-FC-LAST-REC (I2)
               MOVE I2                      TO AR137WS-MAX-FC-TABLE
           END-IF.

           IF (AR137WS-OPEN-AMT                 = ZEROES)
               MOVE "Y"                         TO AR137WS-LATE-CHRG.

           IF (AR137WS-MAX-FC-TABLE             > 1)
               MOVE 1                           TO I2
               MOVE 2                           TO I3
               PERFORM 1018-SORT-FC-DATA
               THRU    1018-END
                   UNTIL  (AR137WS-SORT).
           
           IF  (AR137WS-ARO-TOTAL-FIN-CHRG NOT = "Y")
           AND (AR137WS-NO-RECS                = "N")
               MOVE "N"                        TO AR137WS-WORKB-DONE
               PERFORM 1020-CALC-ARO-OPEN-AMT
               THRU    1020-END
                   VARYING I2 FROM 1 BY 1 UNTIL
                                           (I2 > AR137WS-MAX-FC-TABLE).

           IF (ACO-INT-BY-DATE-FL              = "Y")
               GO TO 1015-FIND-NEXT-AROITEMS.

       1015-CONT.

      **** NET METHOD PROCESSING FOR FINANCE CHARGES ****
      **** NOTE: IF ACM-FIN-CALC-TYPE = "N" (NET METHOD) THEN
      ****       ACM-LATE-PAY-FL MUST BE = "F" (FINANCE CHARGES ONLY)

           IF (ACM-FIN-CALC-TYPE         = "N")
               MOVE AR137WS-PRL-SV       TO AR137WS-PRL
               MOVE AR137WS-PRLSV-SV     TO AR137WS-PRLSV
               ADD AR137WS-CPNMT         TO AR137WS-PNMT
               GO TO 1015-FIND-NEXT-AROITEMS.

      ****  DETAIL METHOD PROCESSING FOR FINANCE CHARGES ****

           COMPUTE AR137WS-FINC = AR137WS-CPNMT
                                * AR137WS-DPF.

           COMPUTE AR137WS-FINC ROUNDED = AR137WS-FINC
                                        / 30.

           IF (AR137WS-ARFINRATE-FOUND = WS-FALSE)
               MOVE ACM-FIN-CHRG-CD         TO DB-FIN-CHRG-CD
               MOVE PRM-AS-OF-DATE          TO DB-EFF-DATE
               PERFORM 850-FIND-NLT-AFISET2
               IF (ARFINRATE-NOTFOUND)
               OR (AFI-FIN-CHRG-CD          NOT = ACM-FIN-CHRG-CD)
                   MOVE ACM-CUSTOMER        TO D3-ERR-CUSTOMER
                   MOVE ACM-FIN-CHRG-CD     TO D3-FIELD-NAME
                   MOVE 205                 TO CRT-MSG-NBR
                   PERFORM 790-GET-MSG
                   MOVE CRT-MESSAGE         TO D3-FIELD-DATA-ERROR
                   MOVE D31-MESSAGE-ERRORS  TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
                   ADD  1                   TO AR137WS-ERROR-CNT
                   GO TO 1015-FIND-NEXT-AROITEMS
               ELSE
                   MOVE WS-TRUE             TO AR137WS-ARFINRATE-FOUND
                   MOVE AFI-CHRG-RATE       TO IFCAWS-BASERATE.

           IF (ACO-LATE-CH-RND-FL = "Y")
               COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FINC
                                                * AFI-CHRG-RATE
               MOVE IFCAWS-BASE-AMT0    TO AR137WS-FINC
           ELSE
               IF (ACO-BASE-ND         = 2)
                   COMPUTE IFCAWS-BASE-AMT2 ROUNDED = AR137WS-FINC
                                                    * AFI-CHRG-RATE
                   MOVE IFCAWS-BASE-AMT2    TO AR137WS-FINC
                ELSE
                   COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FINC
                                                    * AFI-CHRG-RATE
                   MOVE IFCAWS-BASE-AMT0    TO AR137WS-FINC.

           INITIALIZE IFCAWS-BASE-AMT2
                      IFCAWS-BASE-AMT0.

      ****
      ****  ACCUMUATE DETAIL FINANCE CHARGES AND OPEN AMOUNTS **********
      ****
           ADD AR137WS-FINC                TO AR137WS-FNET.
           ADD AR137WS-CPNMT               TO AR137WS-PNMT.

474744*     IF (WF-FINANCE-AMOUNT           = ZEROS)
474744*         GO TO 1015-FIND-NEXT-AROITEMS.

           IF (AR137WS-FINC = ZEROES)
               GO TO 1015-FIND-NEXT-AROITEMS.

           MOVE "D"                        TO WF-TYPE.
           ADD 1                           TO AR137WS-SEQ-NBR.
           IF (AR137WS-SEQ-NBR  = ZEROES)
               ADD 1                       TO AR137WS-SEQ-NBR
           END-IF.
           MOVE AR137WS-SEQ-NBR            TO WF-SEQ-NBR.
           MOVE AR137WS-CPNMT              TO WF-OPEN-AMOUNT.
           MOVE AR137WS-DPF                TO WF-DPF.
           INITIALIZE                         WF-DPL.
           MOVE AR137WS-DUDT-FIN           TO WF-DUE-DATE.
           MOVE ARO-DUE-DATE               TO WF-ARO-DUE-DATE.
           MOVE ARO-TRANS-TYPE             TO WF-TRANS-TYPE.
           MOVE ARO-INVOICE                TO WF-TRANS-NBR.
           MOVE ARO-CUSTOMER               TO WF-CUSTOMER.
           MOVE ARO-PROCESS-LEVEL          TO WF-PROCESS-LEVEL.
           MOVE AFI-CHRG-RATE              TO WF-CHRG-RATE.
           MOVE AR137WS-FINC               TO WF-FINANCE-AMOUNT.
474744     MOVE 1                          TO WF-PRINT-FL.
474744     ADD WF-FINANCE-AMOUNT           TO AR137WS-CUST-AMT.
           MOVE ARO-TRANS-DATE             TO WF-TRANS-DATE.
           MOVE PRM-COMPANY                TO WF-COMPANY.
           MOVE ARO-PAYMENT-SEQ            TO WF-PAYMENT-SEQ.
           MOVE AR137WS-CURR-RATE          TO WF-ORIG-RATE.
           MOVE IFCRWS-MULT-DIV            TO WF-CURR-MUDV.
           MOVE IFCCWS-CURRENCY-ND         TO WF-ORIG-ND.
           MOVE ACM-CURRENCY-CD            TO WF-ORIG-CURRENCY.
342466     MOVE AR137WS-COUNTRY-CODE       TO WF-COUNTRY-CODE.

           IF (ACO-CURRENCY-CD             NOT = ACM-CURRENCY-CD)
               MOVE ARO-COMPANY            TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD        TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD        TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                   TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE         TO IFCAWS-EFFECT-DATE
               MOVE ZEROS                  TO IFCAWS-BASE-AMOUNT
               MOVE WF-FINANCE-AMOUNT      TO IFCAWS-TRAN-AMOUNT
               MOVE AR137WS-CURR-RATE      TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV      TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND     TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND            TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               MOVE IFCAWS-BASE-AMOUNT     TO WF-ORIG-AMT
           ELSE
               MOVE ACO-CURRENCY-CD        TO WF-ORIG-CURRENCY
               MOVE 1                      TO WF-ORIG-RATE
               MOVE "M"                    TO WF-CURR-MUDV
               MOVE ACO-BASE-ND            TO WF-ORIG-ND
               MOVE WF-FINANCE-AMOUNT      TO WF-ORIG-AMT.

      **** INITIALIZE FIELDS NOT LOADED FOR WORKFILE 
      ****
           INITIALIZE                         WF-LATE-AMOUNT.
           INITIALIZE                         WF-MIN-CHG-FL.
           INITIALIZE                         WF-NO-OF-DAYS.
           INITIALIZE                         WF-DPL-EFF.
           INITIALIZE                         WF-DEPOSIT-DATE.
           INITIALIZE                         WF-MEMO-CHG-FL.
           INITIALIZE                         WF-DPF-EFF.
           INITIALIZE                         WF-BATCH-NBR.

           WRITE AR137W1-REC.

           MOVE AR137WS-PRL-SV          TO AR137WS-PRL.
           MOVE AR137WS-PRLSV-SV        TO AR137WS-PRLSV.

           ADD WF-ORIG-AMT              TO AR137WS-ORIG-AMT.

           GO TO 1015-FIND-NEXT-AROITEMS.

      ****  
      **** PROCESS LATE PAYMENT ONLY (ACM-LATE-PAY-FL = "L")
      ****

       1015-LATE-PAY-ONLY.

           INITIALIZE AR137WS-APAM
                      AR137WS-CALC-ORIG-AMT
                      AR137WS-CALC-FNET
                      AR137WS-LATE-FULLY-PAID.

      **** IF PARTIAL PAYMENTS ARE MADE AND ACO-INT-BY-DATE-FL = Y
      **** DO NOT ASSESS LATE CHARGES UNTIL IT IS FULLY APPLIED
           COMPUTE AR137WS-LATE-OPEN-AMT  ROUNDED
                                          =  ARO-ORIG-AMT
                                          - (ARO-ORIG-APP-AMT
                                          +  ARO-ORIG-ADJ-AMT).
           IF (AR137WS-LATE-OPEN-AMT  = ZEROES)
               MOVE WS-TRUE               TO AR137WS-LATE-FULLY-PAID
           ELSE
               MOVE WS-FALSE              TO AR137WS-LATE-FULLY-PAID.
      ****

           IF  (ACM-LATE-PAY-FL           = "L")
               IF (ARO-APPLIED-SEQ    NOT = ZEROES)
                   MOVE "N"               TO AR137WS-WORKB-DONE
                   MOVE ARO-COMPANY       TO DB-COMPANY
                   MOVE ARO-TRANS-TYPE    TO DB-TRANS-TYPE
                   MOVE ARO-INVOICE       TO DB-INVOICE
                   MOVE ARO-PAYMENT-SEQ   TO DB-PAYMENT-SEQ
                   MOVE ARO-BATCH-NBR     TO DB-BATCH-NBR
                   MOVE ARASET1-BATCH-NBR TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-ARASET1
                   PERFORM 1020-CALC-ARO-OPEN-AMT
                   THRU    1020-END
                       UNTIL (ARAPPLIED-NOTFOUND)
                       OR    (AR137WS-END-PROG NOT = "N")
                   IF (AR137WS-END-PROG = "Y")
                       GO TO 1015-END
                   ELSE
                       PERFORM 1026-FIND-ARAPPLIED
                       THRU    1026-END
                       GO TO 1015-FIND-NEXT-AROITEMS
                   END-IF
               ELSE
                   GO TO 1015-FIND-NEXT-AROITEMS
               END-IF
           ELSE
               GO TO 1015-FIND-NEXT-AROITEMS
           END-IF.

       1015-FIND-NEXT-AROITEMS.

           IF (ACM-LATE-PAY-FL        = "L")
               CLOSE AR137W4-FILE
               OPEN OUTPUT AR137W4-FILE
               CLOSE AR137W4-FILE
               OPEN I-O               AR137W4-FILE
               MOVE WS-FALSE          TO AR137CWS-FILE
               INITIALIZE                AR137WS-CALC-ORIG-AMT
                                         AR137WS-CALC-FNET
                                         AR137WS-LATE-SEQ-NBR.

           IF (PRM-FUTURE-APPS  = "N")
               PERFORM 860-FIND-NXTRNG-AROSET3
           ELSE
               PERFORM 860-FIND-NXTRNG-AROSET6.

       1015-END.

      ******************************************************************
       1016-PAYMENT-CREDITS.
      ******************************************************************

           IF (AR137WS-CRED            NOT < WF-OPEN-AMOUNT)
               IF (WF-LATE-AMOUNT NOT = ZEROES)
                   NEXT SENTENCE
               ELSE
                   SUBTRACT WF-FINANCE-AMOUNT FROM AR137WS-FNET
                   SUBTRACT WF-OPEN-AMOUNT    FROM AR137WS-PNMT
                   SUBTRACT WF-OPEN-AMOUNT    FROM AR137WS-CRED
                   PERFORM 8300-DELETE-AR137W1
               END-IF
           ELSE
               SUBTRACT WF-FINANCE-AMOUNT FROM AR137WS-FNET
474744         SUBTRACT WF-FINANCE-AMOUNT FROM AR137WS-CUST-AMT
               SUBTRACT AR137WS-CRED      FROM AR137WS-PNMT
               SUBTRACT AR137WS-CRED      FROM WF-OPEN-AMOUNT
               INITIALIZE AR137WS-CRED
               COMPUTE AR137WS-FINC = WF-OPEN-AMOUNT
                                    * WF-DPF

               COMPUTE AR137WS-FINC ROUNDED = AR137WS-FINC
                                            / 30
               IF (ACO-LATE-CH-RND-FL = "Y")
                   COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FINC
                                                    * AFI-CHRG-RATE
                   MOVE IFCAWS-BASE-AMT0    TO AR137WS-FINC
               ELSE
                   IF (ACO-BASE-ND         = 2)
                       COMPUTE IFCAWS-BASE-AMT2 ROUNDED = AR137WS-FINC
                                                        * AFI-CHRG-RATE
                       MOVE IFCAWS-BASE-AMT2    TO AR137WS-FINC
                    ELSE
                       COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FINC
                                                        * AFI-CHRG-RATE
                       MOVE IFCAWS-BASE-AMT0    TO AR137WS-FINC
                    END-IF
               END-IF
               INITIALIZE IFCAWS-BASE-AMT2
                          IFCAWS-BASE-AMT0
               ADD AR137WS-FINC                 TO AR137WS-FNET
               MOVE AR137WS-FINC                TO WF-FINANCE-AMOUNT
474744         ADD WF-FINANCE-AMOUNT           TO AR137WS-CUST-AMT
474744         MOVE 1                          TO WF-PRINT-FL
               IF (ACO-CURRENCY-CD             NOT = ACM-CURRENCY-CD)
167804             MOVE PRM-COMPANY            TO IFCAWS-COMPANY
                   MOVE ACO-CURRENCY-CD        TO IFCAWS-FR-CURR-CODE
                   MOVE ACM-CURRENCY-CD        TO IFCAWS-TO-CURR-CODE
                   MOVE "AR"                   TO IFCAWS-SYSTEM
                   MOVE PRM-AS-OF-DATE         TO IFCAWS-EFFECT-DATE
                   MOVE ZEROS                  TO IFCAWS-BASE-AMOUNT
                   MOVE WF-FINANCE-AMOUNT      TO IFCAWS-TRAN-AMOUNT
                   MOVE AR137WS-CURR-RATE      TO IFCAWS-BASERATE
                   MOVE AR137WS-CURR-MUDV      TO IFCAWS-MULT-DIV
                   MOVE IFCCWS-CURRENCY-ND     TO IFCAWS-BASE-ND
                   MOVE ACO-BASE-ND            TO IFCAWS-TRAN-ND
                   PERFORM 690-CALCULATE-AMOUNT-60
                   MOVE IFCAWS-BASE-AMOUNT     TO WF-ORIG-AMT
               ELSE
                   MOVE WF-FINANCE-AMOUNT      TO WF-ORIG-AMT
               END-IF
               PERFORM 8100-RECREATE-AR137W1
           END-IF.

           PERFORM 8600-READ-NEXT-AR137W1.

       1016-END.

      ******************************************************************
       1017-SEL-ARITEMAUD.
      ******************************************************************

           IF (ARZ-CANCEL-DATE        < PRM-AS-OF-DATE)
           OR (ARZ-CANCEL-DATE        = PRM-AS-OF-DATE)
               GO TO 1017-FIND-NEXT-ARITEMAUD.

           IF (ARZ-DUE-DATE           > PRM-AS-OF-DATE)
               GO TO 1017-FIND-NEXT-ARITEMAUD.

           MOVE ARZ-COMPANY            TO DB-COMPANY.
           MOVE ARZ-TRANS-TYPE         TO DB-TRANS-TYPE.
           MOVE ARZ-INVOICE            TO DB-INVOICE.
           MOVE ARZ-PAYMENT-SEQ        TO DB-PAYMENT-SEQ.
           PERFORM 840-FIND-AROSET1.

      **** "D" = BOE PAYMENT TRANSACTIONS
      **** "B" = BANK SERVICE TRANSACTIONS
      **** "C" = CASH PAYMENT TRANSACTIONS
      ****
           IF (ARO-BANK-INST-TYPE = "D")
               GO TO 1017-FIND-NEXT-ARITEMAUD.

      **** CALCULATE OPEN PAYMENTS AND CREDIT MEMOS

      **** "C" = CREDIT MEMOS
           IF (ARO-TRANS-TYPE = "C")
               PERFORM 1037-CREDIT-MEMO
               THRU    1037-END
               GO TO 1017-FIND-NEXT-ARITEMAUD.

           IF (PRM-INCLUDE-INT            = "N")
               IF (ARO-ORIG-CODE = "F")
                   GO TO 1017-FIND-NEXT-ARITEMAUD.

           IF (ARO-DUE-DATE > PRM-AS-OF-DATE)
               GO TO 1017-FIND-NEXT-ARITEMAUD.

      **** "7" = FULLY APPLIED TRANSACTIONS
           IF  (ARO-STATUS = 7)
                GO TO 1017-FIND-NEXT-ARITEMAUD.

      **** "8" = CLOSED TRANSACTIONS (WITH FINANCE CHARGES)
           IF  (ARO-STATUS = 8)
           AND (ARO-LAST-FC-DATE NOT = ZEROES)
               GO TO 1017-FIND-NEXT-ARITEMAUD.

      **** "8" = CLOSED TRANSACTIONS (WITHOUT FINANCE CHARGES)
           IF  (ARO-STATUS = 8)
           AND (ARO-LAST-FC-DATE = ZEROES)
           AND (ACM-LATE-PAY-FL = "F")
               GO TO 1017-FIND-NEXT-ARITEMAUD.

      **** NOTE:  IF ACM-LATE-PAY-FL = "L" (LATE CHARGES)
      ****           ACM-LATE-PAY-FL = "B" (LATE AND FINANCE CHARGES)
      ****        THEN
      ****           ACM-FIN-CALC-TYPE ***MUST*** = "D" (DETAIL METHOD)

      **** CALCULATE DUE DATE AND JULIAN ADJUSTED DUE DATE FOR
      **** FINANCE CHARGES
      **** "F" = ACCESS FINANCE CHARGES ONLY
      **** "B" = ACCESS FINANCE CHARGES AND LATE PAYMENT CHARGES
      **** 
           IF (ACM-LATE-PAY-FL = "F" OR "B")
               IF (ARO-LAST-FC-DATE            = ZEROES)
                   MOVE ARO-DUE-DATE           TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   IF (ACO-INT-BY-DATE-FL = "N")
                       ADD ACM-FIN-GRAC-DAYS   TO WSDR-JULIAN-DAYS
                   END-IF
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-FIN
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO AR137WS-DUDT-FIN
               ELSE
                   IF (ARO-LAST-FC-DATE        > ARO-DUE-DATE)
                       MOVE ARO-LAST-FC-DATE   TO AR137WS-DUDT-FIN
                       MOVE ARO-LAST-FC-DATE   TO WSDR-FR-DATE
                   ELSE
                       MOVE ARO-DUE-DATE       TO AR137WS-DUDT-FIN
                       MOVE ARO-DUE-DATE       TO WSDR-FR-DATE
                   END-IF
                   PERFORM 900-DATE-TO-JULIAN
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-FIN
               END-IF
           END-IF.

      **** CALCULATE DUE DATE AND JULIAN ADJUSTED LATE DATE FOR
      **** LATE CHARGES
      **** "L" = ACCESS LATE PAYMENT CHARGES ONLY
      **** "B" = ACCESS LATE PAYMENT CHARGES AND FINANCE CHARGES
      ****
           IF (ACO-INT-BY-DATE-FL  = "N")
               IF (ACM-LATE-PAY-FL = "L" OR "B")
                   MOVE ARO-DUE-DATE           TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   IF (ARO-LAST-FC-DATE = ZEROES)
                       ADD ACM-FIN-GRAC-DAYS   TO WSDR-JULIAN-DAYS
                   END-IF
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-LATE
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO AR137WS-DUDT-LATE
               END-IF
           ELSE
               IF (ACM-LATE-PAY-FL = "L")
                   MOVE ARO-DUE-DATE           TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-LATE
                   PERFORM 900-JULIAN-TO-DATE
                   MOVE WSDR-FR-DATE           TO AR137WS-DUDT-LATE
               END-IF
           END-IF.

           IF (PRM-UPDATE-OPTION         = "U")
               IF (AR137WS-PRL-SV        = "N")
                   IF (AR137WS-PRLSV-SV  = SPACES)
                       MOVE ARO-PROCESS-LEVEL TO AR137WS-PRLSV-SV
                   ELSE
                       IF (AR137WS-PRLSV-SV NOT = ARO-PROCESS-LEVEL)
                           MOVE "Y"      TO AR137WS-PRL-SV.

           IF  (ACO-CURRENCY-CD      NOT = ACM-CURRENCY-CD)
           AND (AR137WS-EDIT-CURR        = WS-FALSE)
               PERFORM 1050-EDIT-CURRENCY
               THRU    1050-END
               IF (ERROR-FOUND)
                   GO TO 1017-END
               END-IF
           END-IF.

      ****
      ****  CALCULATE LATE CHARGES ONLY (ACM-LATE-PAY-FL = "L")
      ****
           IF (ACM-LATE-PAY-FL           = "L")
               GO TO 1017-LATE-PAY-ONLY.

      ****
      **** CALCULATE LATE CHARGES AND FINANCE CHARGES
      ****
      **** (ACM-LATE-PAY-FL = "F" OR "B")
      ****
      **** COMPUTE LATE CHARGES (ACM-LATE-PAY-FL = "B")
      ****
           INITIALIZE AR137WS-APAM.
           
           IF (ACO-INT-BY-DATE-FL  = "N")
               MOVE "Y"                            TO AR137WS-RECF
               IF (ARO-APPLIED-SEQ         NOT = ZEROES)
                   MOVE ARO-COMPANY            TO DB-COMPANY
                   MOVE ARO-TRANS-TYPE         TO DB-TRANS-TYPE
                   MOVE ARO-INVOICE            TO DB-INVOICE
                   MOVE ARO-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
                   MOVE ARO-BATCH-NBR          TO DB-BATCH-NBR
                   MOVE ARASET1-BATCH-NBR      TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-ARASET1
                   PERFORM 1020-CALC-ARO-OPEN-AMT
                   THRU    1020-END
                       UNTIL (ARAPPLIED-NOTFOUND)
                       OR    (AR137WS-RECF     NOT = "Y")
                       OR    (AR137WS-END-PROG NOT = "N")
               END-IF
               IF (AR137WS-END-PROG = "Y")
                   GO TO 1017-END
               END-IF
               IF (AR137WS-RECF     = "N")
                   GO TO 1017-FIND-NEXT-ARITEMAUD
               END-IF
           END-IF.

           IF (ACO-INT-BY-DATE-FL        = "N")
               COMPUTE AR137WS-DPF       = AR137WS-AS-OF-DAYS
                                         - AR137WS-LDAT-FIN
               IF (AR137WS-DPF           <= ZEROES)
                   GO TO 1017-FIND-NEXT-ARITEMAUD
               END-IF
           END-IF.

      **** DISPUTES
           IF (ACM-DISPUTES-FIN          = "N")
               IF (ARO-DISPUTE-SEQ       NOT = ZEROES)
                   MOVE PRM-COMPANY          TO DB-COMPANY
                   MOVE ARZ-CUSTOMER         TO DB-CUSTOMER
                   MOVE ARZ-TRANS-TYPE       TO DB-TRANS-TYPE
                   MOVE ARZ-INVOICE          TO DB-INVOICE
                   MOVE ARZ-PAYMENT-SEQ      TO DB-PAYMENT-SEQ
                   MOVE ADPSET1-PAYMENT-SEQ  TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-ADPSET1
                   PERFORM 1030-DISPUTES
                   THRU    1030-END
                       UNTIL (ARDISPUTE-NOTFOUND).

           IF (ACO-INT-BY-DATE-FL            = "N")
               GO TO 1017-CONT.

           MOVE "N"                          TO AR137WS-LATE-CHRG.

      ****
      ****  COMPUTE FINANCE CHARGES (ACM-LATE-PAY-FL = "F" OR "B")
      ****
           IF (AR137WS-LATE-CHRG       = "Y")
               COMPUTE AR137WS-DPF     = AR137WS-AS-OF-DAYS
                                       - AR137WS-LDAT-FIN
                                       - 1
           ELSE
      *** SUBTRACT GRACE DAYS FOR INVOICE SELECTION CRITERIA FOR FINANCE
      *** CHARGE CALCULATION
               IF (ARO-LAST-FC-DATE    = ZEROES)
                   COMPUTE AR137WS-DPF = AR137WS-AS-OF-DAYS
                                       - AR137WS-LDAT-FIN
                                       - ACM-FIN-GRAC-DAYS
               ELSE
                   COMPUTE AR137WS-DPF = AR137WS-AS-OF-DAYS
                                       - AR137WS-LDAT-FIN
               END-IF
           END-IF.

           IF (AR137WS-DPF             <= ZEROES)
               GO TO 1017-FIND-NEXT-ARITEMAUD
           END-IF.

      *** ADD GRACE DAYS BACK IN FOR FINANCE CHARGE CALCULATION
           IF  (AR137WS-LATE-CHRG  NOT = "Y")
           AND (ARO-LAST-FC-DATE       = ZEROES)
               COMPUTE AR137WS-DPF     = AR137WS-DPF
                                       + ACM-FIN-GRAC-DAYS
           END-IF.

           IF  (ACM-LATE-PAY-FL        = "B")
           AND (AR137WS-LATE-CHRG      = "Y")
               GO TO 1017-FIND-NEXT-ARITEMAUD
           END-IF.
         
           MOVE "N"                    TO AR137WS-APPLD-EXIST
                                          AR137WS-ARO-TOTAL-FIN-CHRG.

           IF (ACM-LATE-PAY-FL         = "B")
               MOVE ARO-COMPANY        TO DB-COMPANY
               MOVE ARO-TRANS-TYPE     TO DB-TRANS-TYPE
               MOVE ARO-INVOICE        TO DB-INVOICE
               MOVE ARO-PAYMENT-SEQ    TO DB-PAYMENT-SEQ
               MOVE ARO-BATCH-NBR      TO DB-BATCH-NBR
               MOVE ARASET1-BATCH-NBR  TO WS-DB-BEG-RNG
               PERFORM 850-FIND-BEGRNG-ARASET1
               IF  (ARAPPLIED-NOTFOUND)
                   MOVE "Y"            TO AR137WS-ARO-TOTAL-FIN-CHRG
                   MOVE ARO-ORIG-AMT   TO AR137WS-OPEN-AMT
                   PERFORM 1020-CALC-ARO-OPEN-AMT
                   THRU    1020-END
               ELSE
                   INITIALIZE             WS-CHARGE-FC-INFO
                                          I2
                                          AR137WS-FIRST-APPLD
                                          AR137WS-TABLE-CREATED
                                          AR137WS-HOLD-TRANS-TYPE
                                          AR137WS-HOLD-INVOICE
                                          AR137WS-HOLD-PAYMENT-SEQ
                                          AR137WS-HOLD-COMPANY
                                          AR137WS-HOLD-BATCH-NBR
                                          AR137WS-HOLD-APP-SEQ
                                          AR137WS-NO-RECS
                   MOVE 1              TO I3
                                          AR137WS-TABLE-SORT
                   MOVE 0              TO AR137WS-TABLE-CONTROL
                   PERFORM 1019-CREATE-FC-TABLE
                   THRU    1019-END
                       UNTIL (ARAPPLIED-NOTFOUND)
               END-IF
           END-IF.

           IF  (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-TABLE-CREATED       NOT = "Y")
           AND (AR137WS-NO-RECS                 = "N")
               ADD 1                        TO I2
               MOVE ARO-DUE-DATE            TO AR137WS-FC-FROM-DATE (I2)
               MOVE PRM-AS-OF-DATE          TO AR137WS-FC-TO-DATE (I2)
               MOVE AR137WS-OPEN-AMT        TO AR137WS-FC-OPEN-AMT (I2)
               MOVE AR137WS-HOLD-TRANS-TYPE TO 
                                              AR137WS-FC-TRANS-TYPE (I2)
               MOVE AR137WS-HOLD-INVOICE    TO AR137WS-FC-INVOICE (I2)
               MOVE AR137WS-HOLD-PAYMENT-SEQ TO
                                             AR137WS-FC-PAYMENT-SEQ (I2)
               MOVE AR137WS-HOLD-COMPANY    TO AR137WS-FC-COMPANY (I2)
               MOVE AR137WS-HOLD-BATCH-NBR  TO AR137WS-FC-BATCH-NBR (I2)
               MOVE AR137WS-HOLD-APP-SEQ    TO AR137WS-FC-APP-SEQ (I2)
               MOVE "Y"                     TO AR137WS-FC-LAST-REC (I2)
               MOVE I2                      TO AR137WS-MAX-FC-TABLE
           END-IF.

           IF  (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-TABLE-CREATED           = "Y")
               IF (AR137WS-OPEN-AMT         NOT = ZEROES)
                   MOVE PRM-AS-OF-DATE      TO AR137WS-FC-TO-DATE (I2)
                   MOVE "Y"                 TO AR137WS-FC-LAST-REC (I2)
               ELSE
                   MOVE "Y"                 TO AR137WS-FC-LAST-REC (I2)
               END-IF
               MOVE I2                      TO AR137WS-MAX-FC-TABLE
           END-IF.

           IF (AR137WS-OPEN-AMT                 = ZEROES)
               MOVE "Y"                         TO AR137WS-LATE-CHRG.

           IF (AR137WS-MAX-FC-TABLE             > 1)
               MOVE 1                           TO I2
               MOVE 2                           TO I3
               PERFORM 1018-SORT-FC-DATA
               THRU    1018-END
                   UNTIL  (AR137WS-SORT).
           
           IF  (AR137WS-ARO-TOTAL-FIN-CHRG NOT = "Y")
           AND (AR137WS-NO-RECS                = "N")
               MOVE "N"                        TO AR137WS-WORKB-DONE
               PERFORM 1020-CALC-ARO-OPEN-AMT
               THRU    1020-END
                   VARYING I2 FROM 1 BY 1 UNTIL
                                           (I2 > AR137WS-MAX-FC-TABLE).

           IF (ACO-INT-BY-DATE-FL              = "Y")
               GO TO 1017-FIND-NEXT-ARITEMAUD.

       1017-CONT.

           IF (AR137WS-APAM              = ARO-TRAN-AMT)
               GO TO 1017-FIND-NEXT-ARITEMAUD.

           COMPUTE AR137WS-CPNMT ROUNDED = ARO-TRAN-AMT
                                         - AR137WS-APAM.

      ****
      **** NET METHOD PROCESSING FOR FINANCE CHARGES
      **** NOTE: IF ACM-FIN-CALC-TYPE = "N" (NET METHOD) THEN
      ****       ACM-LATE-PAY-FL MUST BE = "F" (FINANCE CHARGES ONLY)
      ****
           IF (ACM-FIN-CALC-TYPE           = "N")
               MOVE AR137WS-PRL-SV         TO AR137WS-PRL
               MOVE AR137WS-PRLSV-SV       TO AR137WS-PRLSV
               ADD AR137WS-CPNMT           TO AR137WS-PNMT
               GO TO 1017-FIND-NEXT-ARITEMAUD.

      ****
      ****  DETAIL CALCULATION PROCESSING FOR FINANCE CHARGES ****
      ****

           COMPUTE AR137WS-FINC = AR137WS-CPNMT
                                * AR137WS-DPF.

           COMPUTE AR137WS-FINC ROUNDED = AR137WS-FINC
                                        / 30.

           MOVE ACM-FIN-CHRG-CD         TO DB-FIN-CHRG-CD.
           MOVE PRM-AS-OF-DATE          TO DB-EFF-DATE.
           PERFORM 850-FIND-NLT-AFISET2.
           IF (ARFINRATE-NOTFOUND)
           OR (AFI-FIN-CHRG-CD          NOT = ACM-FIN-CHRG-CD)
               MOVE ACM-CUSTOMER        TO D3-ERR-CUSTOMER
               MOVE ACM-FIN-CHRG-CD     TO D3-FIELD-NAME
               MOVE 205                 TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE         TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS  TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                   TO AR137WS-ERROR-CNT
               GO TO 1017-FIND-NEXT-ARITEMAUD
           ELSE
               MOVE AFI-CHRG-RATE       TO IFCAWS-BASERATE
           END-IF.

           IF (ACO-CURRENCY-CD          NOT = ACM-CURRENCY-CD)
               MOVE IFCCWS-CURRENCY-ND  TO IFCAWS-TRAN-ND
               MOVE ACO-COMPANY         TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD     TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD     TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE      TO IFCAWS-EFFECT-DATE
               MOVE ZEROS               TO IFCAWS-BASE-AMOUNT
               MOVE AR137WS-FINC        TO IFCAWS-TRAN-AMOUNT
               MOVE ZEROS               TO IFCAWS-BASE-AMOUNT
               MOVE AR137WS-CURR-RATE      TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV      TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND     TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND            TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               MOVE IFCAWS-BASE-AMOUNT  TO AR137WS-FINC.

      ****
      ****  ACCUMULATE DETAIL FINANCE CHARGES AND OPEN AMOUNTS
      ****
           ADD AR137WS-FINC                TO AR137WS-FNET.
           ADD AR137WS-CPNMT               TO AR137WS-PNMT.

           IF (AR137WS-FINC                = ZEROES)
               GO TO 1017-FIND-NEXT-ARITEMAUD.

           MOVE "D"                        TO WF-TYPE.
           ADD 1                           TO AR137WS-SEQ-NBR.
           IF (AR137WS-SEQ-NBR  = ZEROES)
               ADD 1                       TO AR137WS-SEQ-NBR
           END-IF.
           MOVE AR137WS-SEQ-NBR            TO WF-SEQ-NBR.
           MOVE AR137WS-CPNMT              TO WF-OPEN-AMOUNT.
           MOVE AR137WS-DPF                TO WF-DPF.
           INITIALIZE                         WF-DPL.
           MOVE AR137WS-DUDT-FIN           TO WF-DUE-DATE.
342466     MOVE AR137WS-COUNTRY-CODE       TO WF-COUNTRY-CODE.
           MOVE ARO-DUE-DATE               TO WF-ARO-DUE-DATE.
           MOVE ARZ-TRANS-TYPE             TO WF-TRANS-TYPE.
           MOVE ARZ-INVOICE                TO WF-TRANS-NBR.
           MOVE ARZ-CUSTOMER               TO WF-CUSTOMER.
           MOVE ARO-PROCESS-LEVEL          TO WF-PROCESS-LEVEL.

           MOVE AFI-CHRG-RATE              TO WF-CHRG-RATE.

           MOVE AR137WS-FINC               TO WF-FINANCE-AMOUNT.
474744     MOVE 1                          TO WF-PRINT-FL.
474744     ADD WF-FINANCE-AMOUNT           TO AR137WS-CUST-AMT.
           MOVE ARO-TRANS-DATE             TO WF-TRANS-DATE.
           MOVE PRM-COMPANY                TO WF-COMPANY.
           MOVE ARZ-PAYMENT-SEQ            TO WF-PAYMENT-SEQ.
           IF (ACO-CURRENCY-CD         NOT = ACM-CURRENCY-CD)
               MOVE ARO-COMPANY            TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD        TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD        TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                   TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE         TO IFCAWS-EFFECT-DATE
               MOVE ZEROS                  TO IFCAWS-BASE-AMOUNT
               MOVE WF-FINANCE-AMOUNT      TO IFCAWS-TRAN-AMOUNT
               MOVE AR137WS-CURR-RATE      TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV      TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND     TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND            TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               MOVE IFCAWS-BASE-AMOUNT     TO WF-ORIG-AMT
               MOVE AR137WS-CURR-RATE      TO WF-ORIG-RATE
               MOVE AR137WS-CURR-MUDV      TO WF-CURR-MUDV
               MOVE IFCCWS-CURRENCY-ND     TO WF-ORIG-ND
               MOVE ACM-CURRENCY-CD        TO WF-ORIG-CURRENCY
           ELSE
               MOVE ACO-CURRENCY-CD        TO WF-ORIG-CURRENCY
               MOVE 1                      TO WF-ORIG-RATE
               MOVE "M"                    TO WF-CURR-MUDV
               MOVE ACO-BASE-ND            TO WF-ORIG-ND
               MOVE WF-FINANCE-AMOUNT      TO WF-ORIG-AMT.

      **** INITIALIZE FIELDS NOT LOADED FOR WORKFILE 
           INITIALIZE                         WF-LATE-AMOUNT.
           INITIALIZE                         WF-MIN-CHG-FL.
           INITIALIZE                         WF-NO-OF-DAYS.
           INITIALIZE                         WF-DPL-EFF.
           INITIALIZE                         WF-DEPOSIT-DATE.
           INITIALIZE                         WF-MEMO-CHG-FL.
           INITIALIZE                         WF-DPF-EFF.
           INITIALIZE                         WF-BATCH-NBR.

           WRITE AR137W1-REC.

           MOVE AR137WS-PRL-SV          TO AR137WS-PRL.
           MOVE AR137WS-PRLSV-SV        TO AR137WS-PRLSV.

           ADD WF-ORIG-AMT              TO AR137WS-ORIG-AMT.

           GO TO 1017-FIND-NEXT-ARITEMAUD.

      ****  
      ****  LATE PAYMENT ONLY (ACM-LATE-PAY-FL = "L")
      ****

       1017-LATE-PAY-ONLY.


           INITIALIZE AR137WS-APAM.

           IF (ACM-LATE-PAY-FL            = "L")
               IF (ARO-APPLIED-SEQ    NOT = ZEROES)
                   MOVE ARZ-COMPANY       TO DB-COMPANY
                   MOVE ARZ-TRANS-TYPE    TO DB-TRANS-TYPE
                   MOVE ARZ-INVOICE       TO DB-INVOICE
                   MOVE ARZ-PAYMENT-SEQ   TO DB-PAYMENT-SEQ
                   MOVE ARO-BATCH-NBR     TO DB-BATCH-NBR
                   MOVE ARASET1-BATCH-NBR TO WS-DB-BEG-RNG
                   PERFORM 850-FIND-BEGRNG-ARASET1
                   PERFORM 1020-CALC-ARO-OPEN-AMT
                   THRU    1020-END
                       UNTIL (ARAPPLIED-NOTFOUND)
                       OR    (AR137WS-END-PROG NOT = "N")
                   IF (AR137WS-END-PROG = "Y")
                       GO TO 1017-END
                   ELSE
                       GO TO 1017-FIND-NEXT-ARITEMAUD
                   END-IF
               ELSE
                   GO TO 1017-FIND-NEXT-ARITEMAUD
               END-IF
           ELSE
               GO TO 1017-FIND-NEXT-ARITEMAUD
           END-IF.

       1017-FIND-NEXT-ARITEMAUD.

           PERFORM 860-FIND-NXTRNG-ARZSET2.

       1017-END.

      ******************************************************************
       1018-SORT-FC-DATA.
      ******************************************************************

           PERFORM
           UNTIL (I3 > AR137WS-MAX-FC-TABLE)
               IF (AR137WS-FC-FROM-DATE (I3) 
                                            < AR137WS-FC-FROM-DATE (I2))
                   MOVE AR137WS-FC-CHRG (I2) TO AR137WS-FC-CHRG-REC
                   MOVE AR137WS-FC-CHRG (I3) TO AR137WS-FC-CHRG (I2)
                   MOVE AR137WS-FC-CHRG-REC  TO AR137WS-FC-CHRG (I3)
                   MOVE 1                    TO AR137WS-TABLE-CONTROL
                   ADD 1                     TO I2
                                                I3
               ELSE
                   ADD 1                     TO I2
                                                I3
               END-IF
           END-PERFORM.

           IF (AR137WS-TABLE-CONTROL         = 0)
               MOVE 0                        TO AR137WS-TABLE-SORT
           ELSE
               INITIALIZE                       AR137WS-TABLE-CONTROL
               MOVE 1                        TO I2
               MOVE 2                        TO I3
               MOVE 1                        TO AR137WS-TABLE-SORT
           END-IF.

       1018-END.

      ******************************************************************
       1019-CREATE-FC-TABLE.
      ******************************************************************

           IF (ARA-STATUS                    = ZEROES)
               MOVE "Y"                      TO AR137WS-NO-RECS
               GO TO 1019-NEXT.

           IF  (ARA-APPLD-AMT                = ZEROES)
           AND (ARA-ADJ-AMT                  = ZEROES)
               MOVE "Y"                      TO AR137WS-NO-RECS
               GO TO 1019-NEXT.

           IF (ARA-RESULT-FL                 = "F" OR "W")
               MOVE "Y"                      TO AR137WS-NO-RECS
               GO TO 1019-NEXT.

           IF (ARA-CR-TYPE                   = SPACES)
               MOVE "Y"                      TO AR137WS-NO-RECS
               GO TO 1019-NEXT.
            
           IF (ARA-RESULT-FL                 = SPACES)
               IF (ARA-CR-TYPE               = "C")
                   NEXT SENTENCE
               ELSE
                   GO TO 1019-NEXT
                   END-IF
           END-IF.
            
           IF (ARA-DEPOSIT-DATE              > PRM-AS-OF-DATE)
           OR (ARA-GL-DATE                   > PRM-AS-OF-DATE)
               MOVE ARA-TRANS-TYPE           TO AR137WS-HOLD-TRANS-TYPE
               MOVE ARA-INVOICE              TO AR137WS-HOLD-INVOICE
               MOVE ARA-PAYMENT-SEQ          TO AR137WS-HOLD-PAYMENT-SEQ
               MOVE ARA-COMPANY              TO AR137WS-HOLD-COMPANY
               MOVE ARA-BATCH-NBR            TO AR137WS-HOLD-BATCH-NBR
               MOVE ARA-APP-SEQ              TO AR137WS-HOLD-APP-SEQ
               MOVE "N"                      TO AR137WS-NO-RECS
               GO TO 1019-NEXT
           END-IF.
                   
           IF (ARA-RESULT-FL                 = "P")
               ADD ARA-APPLD-AMT             TO AR137WS-APAM
               COMPUTE AR137WS-OPEN-AMT      = AR137WS-CPNMT
                                             - ARA-APPLD-AMT
                                             - ARA-ADJ-AMT
               MOVE ARA-TRANS-TYPE           TO AR137WS-HOLD-TRANS-TYPE
               MOVE ARA-INVOICE              TO AR137WS-HOLD-INVOICE
               MOVE ARA-PAYMENT-SEQ          TO AR137WS-HOLD-PAYMENT-SEQ
               MOVE ARA-COMPANY              TO AR137WS-HOLD-COMPANY
               MOVE ARA-BATCH-NBR            TO AR137WS-HOLD-BATCH-NBR
               MOVE ARA-APP-SEQ              TO AR137WS-HOLD-APP-SEQ
               MOVE "N"                      TO AR137WS-NO-RECS
               GO TO 1019-NEXT
           END-IF.

           IF  (ARO-LAST-FC-DATE         NOT = ZEROES)
           AND (ARA-DEPOSIT-DATE             <= ARO-LAST-FC-DATE)
               IF (AR137WS-APPLD-EXIST       = "N")
                   COMPUTE AR137WS-OPEN-AMT  = AR137WS-CPNMT
                                             - ARA-APPLD-AMT
                                             - ARA-ADJ-AMT
                   MOVE "Y"                  TO AR137WS-APPLD-EXIST
               ELSE
                   COMPUTE AR137WS-OPEN-AMT  = AR137WS-OPEN-AMT
                                             - ARA-APPLD-AMT
                                             - ARA-ADJ-AMT
               END-IF
               MOVE ARA-TRANS-TYPE           TO AR137WS-HOLD-TRANS-TYPE
               MOVE ARA-INVOICE              TO AR137WS-HOLD-INVOICE
               MOVE ARA-PAYMENT-SEQ          TO AR137WS-HOLD-PAYMENT-SEQ
               MOVE ARA-COMPANY              TO AR137WS-HOLD-COMPANY
               MOVE ARA-BATCH-NBR            TO AR137WS-HOLD-BATCH-NBR
               MOVE ARA-APP-SEQ              TO AR137WS-HOLD-APP-SEQ
               IF (AR137WS-OPEN-AMT          = ZEROES)
                   MOVE "Y"                  TO AR137WS-NO-RECS
               ELSE
                   MOVE "N"                  TO AR137WS-NO-RECS
               END-IF
               GO TO 1019-NEXT
           END-IF.

           MOVE "N"                          TO AR137WS-NO-RECS.

           IF (AR137WS-FIRST-APPLD           = "Y")
               MOVE ARA-DEPOSIT-DATE         TO AR137WS-FC-TO-DATE (I2)
           END-IF.

           IF (AR137WS-APPLD-EXIST       NOT = "Y")
               COMPUTE AR137WS-OPEN-AMT      = AR137WS-CPNMT
                                             - ARA-APPLD-AMT
                                             - ARA-ADJ-AMT
               IF (AR137WS-OPEN-AMT      NOT = ZEROES)
                   MOVE "Y"                  TO AR137WS-FIRST-APPLD
               END-IF
           ELSE
               COMPUTE AR137WS-OPEN-AMT      = AR137WS-OPEN-AMT
                                             - ARA-APPLD-AMT
                                             - ARA-ADJ-AMT
               MOVE "N"                      TO AR137WS-FIRST-APPLD
           END-IF.

           IF  (AR137WS-OPEN-AMT             = ZEROES)
           AND (I2                           > 1)
               GO TO 1019-NEXT.

           ADD 1                            TO I2.

           IF (ARO-LAST-FC-DATE             = ZEROES)
               IF (AR137WS-APPLD-EXIST  NOT = "Y")
                   MOVE ARO-DUE-DATE        TO AR137WS-FC-FROM-DATE (I2)
                   MOVE ARA-DEPOSIT-DATE    TO AR137WS-FC-TO-DATE (I2)
                   COMPUTE AR137WS-FC-OPEN-AMT (I2)
                                            = AR137WS-CPNMT
                                            - ARA-ADJ-AMT
               ELSE
                   MOVE ARA-DEPOSIT-DATE    TO AR137WS-FC-FROM-DATE (I2)
                   MOVE AR137WS-OPEN-AMT    TO AR137WS-FC-OPEN-AMT (I2)
               END-IF
           ELSE
               IF (AR137WS-APPLD-EXIST  NOT = "Y")
                   MOVE ARO-LAST-FC-DATE    TO AR137WS-FC-FROM-DATE (I2)
                   MOVE ARA-DEPOSIT-DATE    TO AR137WS-FC-TO-DATE (I2)
                   COMPUTE AR137WS-FC-OPEN-AMT (I2)
                                            = AR137WS-CPNMT
                                            - ARA-ADJ-AMT
               ELSE
                   IF (AR137WS-OPEN-AMT     = ZEROES)
                       MOVE ARO-LAST-FC-DATE TO AR137WS-FC-FROM-DATE(I2)
                       MOVE ARA-DEPOSIT-DATE TO AR137WS-FC-TO-DATE (I2)
                       MOVE ARA-APPLD-AMT    TO AR137WS-FC-OPEN-AMT (I2)
                   ELSE
                       MOVE ARA-DEPOSIT-DATE TO AR137WS-FC-FROM-DATE(I2)
                       MOVE AR137WS-OPEN-AMT TO AR137WS-FC-OPEN-AMT (I2)
                   END-IF
               END-IF
           END-IF.

           MOVE ARA-TRANS-TYPE           TO AR137WS-FC-TRANS-TYPE (I2).
           MOVE ARA-INVOICE              TO AR137WS-FC-INVOICE (I2).
           MOVE ARA-PAYMENT-SEQ          TO AR137WS-FC-PAYMENT-SEQ (I2).
           MOVE ARA-COMPANY              TO AR137WS-FC-COMPANY (I2).
           MOVE ARA-BATCH-NBR            TO AR137WS-FC-BATCH-NBR (I2).
           MOVE ARA-APP-SEQ              TO AR137WS-FC-APP-SEQ (I2).
           MOVE "N"                      TO AR137WS-FC-LAST-REC (I2).
           MOVE "Y"                      TO AR137WS-APPLD-EXIST.

           IF  (AR137WS-OPEN-AMT         = ZEROES)
           AND (ARO-LAST-FC-DATE     NOT = ZEROES)
               MOVE "Y"                  TO AR137WS-TABLE-CREATED
               GO TO 1019-NEXT
           END-IF.

           IF (AR137WS-FIRST-APPLD       = "Y")
               ADD 1                     TO I2
               MOVE AR137WS-OPEN-AMT     TO AR137WS-FC-OPEN-AMT (I2)
               MOVE ARA-TRANS-TYPE       TO AR137WS-FC-TRANS-TYPE (I2)
               MOVE ARA-INVOICE          TO AR137WS-FC-INVOICE (I2)
               MOVE ARA-PAYMENT-SEQ      TO AR137WS-FC-PAYMENT-SEQ (I2)
               MOVE ARA-COMPANY          TO AR137WS-FC-COMPANY (I2)
               MOVE ARA-BATCH-NBR        TO AR137WS-FC-BATCH-NBR (I2)
               MOVE ARA-APP-SEQ          TO AR137WS-FC-APP-SEQ (I2)
               MOVE "N"                  TO AR137WS-FC-LAST-REC (I2)
           END-IF.

           MOVE ARA-DEPOSIT-DATE         TO AR137WS-FC-FROM-DATE (I2).
           MOVE "Y"                      TO AR137WS-TABLE-CREATED.

       1019-NEXT.

           PERFORM 860-FIND-NXTRNG-ARASET1.

       1019-END.

      ******************************************************************
       1020-CALC-ARO-OPEN-AMT.
      ******************************************************************

           IF  (ACM-LATE-PAY-FL            = "B")
           AND (ACO-INT-BY-DATE-FL         = "Y")
               GO TO 1020-NEXT.

      **** "0" = UNRELEASED
           IF (ARA-STATUS                  = ZEROES)
               GO TO 1020-FIND-NEXT-ARASET1.

           IF  (ARA-APPLD-AMT              = ZEROES)
           AND (ARA-ADJ-AMT                = ZEROES)
                GO TO 1020-FIND-NEXT-ARASET1.

           IF (ARA-DEPOSIT-DATE            > PRM-AS-OF-DATE)
               GO TO 1020-FIND-NEXT-ARASET1.

      **** "F" = FULLY APPLIED ON TIME
      **** "W" = FINANCE CHARGED ASSESSED
      ****
           IF (ARA-RESULT-FL               = "F" OR "W")
               IF (ARA-GL-DATE             <= PRM-AS-OF-DATE)
               MOVE "N"                    TO AR137WS-RECF
               GO TO 1020-FIND-NEXT-ARASET1.

      ****
      ****  ACCUMULATE PAYMENT AMOUNTS
      ****
           IF (ARA-GL-DATE                 <= PRM-AS-OF-DATE)
               ADD ARA-APPLD-AMT               TO AR137WS-APAM
               ADD ARA-ADJ-AMT                 TO AR137WS-APAM.

      **** "C" = CREDIT MEMO
           IF  (ARA-CR-TYPE        = "C")
           AND (ACO-INT-BY-DATE-FL = "N")
               GO TO 1020-FIND-NEXT-ARASET1.

      **** SPACES = UNKNOWN OR UNDEFINED
           IF (ARA-CR-TYPE                 = SPACES)
               GO TO 1020-FIND-NEXT-ARASET1.

      **** "X" = PARTIALLY APPLIED LATE WITH CHARGES ASSESSED
      **** "Y" = FULLY APPLIED LATE WITH CHARGES ASSESSED
      ****
           IF (ARA-RESULT-FL               = "X" OR "Y")
               GO TO 1020-FIND-NEXT-ARASET1.

      **** SPACES = UNKNOWN OR UNDEFINED
           IF (ARA-RESULT-FL               = SPACES)
               GO TO 1020-FIND-NEXT-ARASET1.

           IF (ACO-INT-BY-DATE-FL          = "N")
               IF (ACM-LATE-PAY-FL         = "L" OR "B")
                   INITIALIZE                WSDR-FR-DATE
                   INITIALIZE                AR137WS-LAPAM
                   MOVE ARA-DEPOSIT-DATE   TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   COMPUTE AR137WS-DPL     = WSDR-JULIAN-DAYS
                                           - AR137WS-LDAT-LATE
                   IF (AR137WS-DPL     NOT > ZERO)
                       GO TO 1020-FIND-NEXT-ARASET1
                   END-IF
               END-IF
               GO TO 1020-CONT
           END-IF.

       1020-NEXT.

           IF  (ACM-LATE-PAY-FL            = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG = "Y")
               INITIALIZE                     WSDR-FR-DATE
               INITIALIZE                     AR137WS-LAPAM
               MOVE PRM-AS-OF-DATE         TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPF         = WSDR-JULIAN-DAYS
                                           - AR137WS-LDAT-FIN
           END-IF.

           IF  (ACM-LATE-PAY-FL             = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-LATE-CHRG           NOT = "Y")
               INITIALIZE                     WSDR-FR-DATE
               INITIALIZE                     AR137WS-LAPAM
               MOVE AR137WS-FC-TO-DATE (I2) TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPF          = WSDR-JULIAN-DAYS
                                            - AR137WS-LDAT-FIN
               IF (AR137WS-DPF              <= 1)
                   GO TO 1020-END
               END-IF
           END-IF.

           IF  (ACM-LATE-PAY-FL             = "B")
           AND (AR137WS-LATE-CHRG           = "Y")
               INITIALIZE                     WSDR-FR-DATE
               INITIALIZE                     AR137WS-LAPAM
               MOVE AR137WS-FC-TO-DATE (I2) TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPL          = WSDR-JULIAN-DAYS
                                            - AR137WS-LDAT-FIN
               IF (AR137WS-DPL              <= 1)
                   GO TO 1020-END
               END-IF
           END-IF.

      **** "L" = LATE CHARGES TO BE ASSESSED
      ****
           IF (ACM-LATE-PAY-FL   = "L")
               INITIALIZE WSDR-FR-DATE
               INITIALIZE AR137WS-LAPAM
               MOVE ARA-DEPOSIT-DATE TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPL = WSDR-JULIAN-DAYS
                                   - AR137WS-LDAT-LATE
                                   - 1
               IF (AR137WS-DPL NOT > ZERO)
                   GO TO 1020-FIND-NEXT-ARASET1
               END-IF
           END-IF.

           IF  (ACO-CURRENCY-CD   NOT = ACM-CURRENCY-CD)
           AND (AR137WS-EDIT-CURR     = WS-FALSE)
                PERFORM 1050-EDIT-CURRENCY
                THRU    1050-END
                IF (ERROR-FOUND)
                    GO TO 1020-END
                END-IF
           END-IF.

       1020-CONT.

           IF  (ARA-TRANS-TYPE      = "C")
           AND (ACO-INT-BY-DATE-FL  = "N")
               GO TO 1020-FIND-NEXT-ARASET1.

      **  PROCESSING LATE CHARGES OR LATE CHARGES AND FINANCE CHARGES
      **** "L" = LATE CHARGES TO BE ASSESSED
      **** "B" = LATE CHARGES AND FINANCE CHARGES TO BE ASSESSED
      ****
           IF  (ACM-LATE-PAY-FL                = "L" OR "B")
      ****
      **** "L" = FULLY APPLIED LATE
      **** "D" = PARTIALLY APPLIED LATE
      ****
               IF  (ARA-RESULT-FL              = "L" OR "D")
               OR  (ACO-INT-BY-DATE-FL         = "Y")
               AND (ACM-LATE-PAY-FL            = "B")
      ****
      **** ACO-INT-BY-DATE-FL = "Y" = USE INTEREST RATES BY DATE RANGES
      **** ARO-LATE-CH-FL     = "Y" = LATE CHARGES IS TO BE APPLIED TO
      ****                            INVOICE
      ****
                   IF  (ACO-INT-BY-DATE-FL     = "Y")
                   AND (ARO-LATE-CH-FL     NOT = "Y")
                       GO TO 1020-FIND-NEXT-ARASET1
                   END-IF
                   IF  (ACO-INT-BY-DATE-FL     = "Y")
                   AND (ARO-LATE-CH-FL         = "Y")
                       MOVE "N"                TO AR137WS-RATE-ERROR
                       PERFORM 1060-CALC-CHRG-BY-DATE
                       THRU    1060-END
                       IF (AR137WS-RATE-ERROR = "Y")
                           IF (ARAPPLIED-FOUND)
                               GO TO 1020-FIND-NEXT-ARASET1
                           ELSE
                               GO TO 1020-END
                           END-IF
                       END-IF
      **  THE FOLLOWING FLAG CAN BE USED TO STOP THE CREATION OF ENTRIES
      **  IN WORKFILE W1 IF THE SUM OF THE FINANCE CHARGES COMPUTED IN
      **  THE 1060 ROUTINE IS BELOW THE INVOICE MINIMUM FINANCE CHARGE
      **  AS ENTERED IN THE ARCUSTOMER DATABASE. 
                       PERFORM 1023-LOAD-WORK-INT-BY-DATE
                       THRU    1023-END  
                       IF (ARA-RESULT-FL               = "L")
                           MOVE PRM-COMPANY            TO WFC-COMPANY
                           MOVE AR137WS-ARO-CUSTOMER   TO WFC-CUSTOMER
                           INITIALIZE WFC-LATE-SEQ-NBR
                           MOVE ARA-TRANS-TYPE         TO WFC-TRANS-TYPE
                           MOVE ARA-INVOICE            TO WFC-TRANS-NBR
                           INITIALIZE WFC-SEQ-NBR
                                      WFC-PAYMENT-SEQ
                                      WFC-ORIG-CURRENCY
                                      WFC-ORIG-RATE
                                      WFC-CURR-MUDV
                                      WFC-ORIG-ND
                                      WFC-ORIG-AMT
                                      WFC-DUE-DATE
                                      WFC-ARO-DUE-DATE
                                      WFC-PROCESS-LEVEL
                                      WFC-OPEN-AMOUNT
                                      WFC-LATE-AMOUNT
                                      WFC-FINANCE-AMOUNT
                                      WFC-DPF
                                      WFC-DPL
                                      WFC-TYPE
                                      WFC-MIN-CHG-FL
                                      WFC-TRANS-DATE
                                      WFC-CHRG-RATE
                                      WFC-NO-OF-DAYS
                                      WFC-DPL-EFF
                                      WFC-DEPOSIT-DATE
                                      WFC-MEMO-CHG-FL
                                      WFC-DPF-EFF
                                      WFC-BATCH-NBR
                                      WFC-APP-SEQ
                           WRITE AR137W4-REC
                       END-IF
                       IF  (AR137WS-ARO-TOTAL-FIN-CHRG   NOT = "Y")
                       AND (AR137WS-WORKB-DONE               = "N")
                           PERFORM 1022-LOAD-WORKB
                           THRU    1022-END
                       END-IF
                       IF (ACM-LATE-PAY-FL     = "L")
                           GO TO 1020-FIND-NEXT-ARASET1
                       END-IF
                   END-IF
                   IF  (ACO-INT-BY-DATE-FL NOT = "Y")
                   AND (ARO-LATE-CH-FL         = "Y")
                       PERFORM 1021-LOAD-WORK
                       THRU    1021-END  
                   END-IF
               ELSE 
                   IF (ACM-LATE-PAY-FL         = "L")
                       GO TO 1020-FIND-NEXT-ARASET1
                   END-IF
               END-IF
           ELSE
               IF (ACM-LATE-PAY-FL             = "L")
                   GO TO 1020-FIND-NEXT-ARASET1
               END-IF
           END-IF.

           IF  (ACM-LATE-PAY-FL            NOT = "L")
           AND (ACO-INT-BY-DATE-FL             = "Y")
               MOVE "Y"                        TO AR137WS-WORKB-DONE
               GO TO 1020-END
           END-IF.

       1020-FIND-NEXT-ARASET1.

           IF (ARAPPLIED-FOUND)
           PERFORM 860-FIND-NXTRNG-ARASET1.

       1020-END.

      ******************************************************************
       1021-LOAD-WORK.
      ******************************************************************

      ****
      **** CREATE WORKFILE 1 RECORDS FOR LATE FINANCE CHARGES WHEN NOT
      **** USING INTEREST-RATE-BY-DATE-RANGES.
      ****
           INITIALIZE WSDR-FR-DATE.
           INITIALIZE AR137WS-LAPAM.
           INITIALIZE AR137WS-MEMO-FINCHG-REC-FLG.

           MOVE ARA-DEPOSIT-DATE           TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.

           COMPUTE AR137WS-DPL = WSDR-JULIAN-DAYS
                               - AR137WS-LDAT-LATE.

           IF (AR137WS-DPL                 NOT > ZERO)
               GO TO 1021-END.

           IF  (ACO-CURRENCY-CD            NOT = ACM-CURRENCY-CD)
           AND (AR137WS-EDIT-CURR              = WS-FALSE)
               PERFORM 1050-EDIT-CURRENCY
               THRU    1050-END
               IF (ERROR-FOUND)
                   GO TO 1021-END.

           MOVE ACM-FIN-CHRG-CD        TO DB-FIN-CHRG-CD.
           MOVE ARA-DEPOSIT-DATE       TO DB-EFF-DATE.
           PERFORM 850-FIND-NLT-AFISET2.
           IF (ARFINRATE-NOTFOUND)
           OR (AFI-FIN-CHRG-CD         NOT = ACM-FIN-CHRG-CD)
               MOVE ACM-CUSTOMER       TO D3-ERR-CUSTOMER
               MOVE ACM-FIN-CHRG-CD    TO D3-FIELD-NAME
               MOVE 205                TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE        TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                  TO AR137WS-ERROR-CNT
               GO TO 1021-END
           ELSE
               MOVE AFI-CHRG-RATE      TO WF-CHRG-RATE
               MOVE ARA-DEPOSIT-DATE   TO WF-DEPOSIT-DATE
           END-IF.

           MOVE ARA-APPLD-AMT          TO AR137WS-LAPAM.
           INITIALIZE                     WF-OPEN-AMOUNT.
           MOVE "D"                    TO WF-TYPE.
           MOVE ARA-APPLD-AMT          TO WF-LATE-AMOUNT.
           MOVE AR137WS-DUDT-LATE      TO WF-DUE-DATE.
           MOVE ARO-DUE-DATE           TO WF-ARO-DUE-DATE.
           ADD 1                       TO AR137WS-SEQ-NBR.
           IF (AR137WS-SEQ-NBR  = ZEROES)
               ADD 1                   TO AR137WS-SEQ-NBR
           END-IF.
           MOVE AR137WS-SEQ-NBR        TO WF-SEQ-NBR.
           MOVE ARA-TRANS-TYPE         TO WF-TRANS-TYPE.
           MOVE ARA-INVOICE            TO WF-TRANS-NBR.
           MOVE AR137WS-ARO-CUSTOMER   TO WF-CUSTOMER.
           MOVE ARO-PROCESS-LEVEL      TO WF-PROCESS-LEVEL.
           MOVE AR137WS-DPL            TO WF-DPL.
           MOVE AR137WS-DPL            TO WF-DPL-EFF.
           INITIALIZE                     WF-DPF.
           MOVE ARO-TRANS-DATE         TO WF-TRANS-DATE.

           COMPUTE AR137WS-FINC        =  ARA-APPLD-AMT
                                       *  AR137WS-DPL.

           COMPUTE AR137WS-FINC ROUNDED = AR137WS-FINC / 30.

           IF (ACO-LATE-CH-RND-FL = "Y")
               COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FINC
                                                * AFI-CHRG-RATE
               MOVE IFCAWS-BASE-AMT0    TO AR137WS-FINC
           ELSE
               IF (ACO-BASE-ND         = 2)
                   COMPUTE IFCAWS-BASE-AMT2 ROUNDED = AR137WS-FINC
                                                    * AFI-CHRG-RATE
                   MOVE IFCAWS-BASE-AMT2    TO WF-FINANCE-AMOUNT
                ELSE
                   COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FINC
                                                    * AFI-CHRG-RATE
                   MOVE IFCAWS-BASE-AMT0    TO WF-FINANCE-AMOUNT.

           ADD WF-FINANCE-AMOUNT            TO AR137WS-FNET.
474744     ADD WF-FINANCE-AMOUNT           TO AR137WS-CUST-AMT.
474744     MOVE 1                          TO WF-PRINT-FL.

           INITIALIZE IFCAWS-BASE-AMT2
                      IFCAWS-BASE-AMT0.
           INITIALIZE AR137WS-MEMO-FINCHG-REC-FLG.
           INITIALIZE WF-MEMO-CHG-FL.
           INITIALIZE WF-NO-OF-DAYS.
           INITIALIZE WF-DPF-EFF.
           INITIALIZE WF-BATCH-NBR.

           IF (ACO-CURRENCY-CD         NOT = ACM-CURRENCY-CD)
               MOVE ARO-COMPANY            TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD        TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD        TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                   TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE         TO IFCAWS-EFFECT-DATE
               MOVE ZEROS                  TO IFCAWS-BASE-AMOUNT
               MOVE WF-FINANCE-AMOUNT      TO IFCAWS-TRAN-AMOUNT
               MOVE AR137WS-CURR-RATE      TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV      TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND     TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND            TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               MOVE IFCAWS-BASE-AMOUNT     TO WF-ORIG-AMT
               MOVE AR137WS-CURR-RATE      TO WF-ORIG-RATE
               MOVE AR137WS-CURR-MUDV      TO WF-CURR-MUDV
               MOVE IFCCWS-CURRENCY-ND     TO WF-ORIG-ND
               MOVE ACM-CURRENCY-CD        TO WF-ORIG-CURRENCY
           ELSE
               MOVE ACO-CURRENCY-CD        TO WF-ORIG-CURRENCY
               MOVE 1                      TO WF-ORIG-RATE
               MOVE "M"                    TO WF-CURR-MUDV
               MOVE ACO-BASE-ND            TO WF-ORIG-ND
               MOVE WF-FINANCE-AMOUNT      TO WF-ORIG-AMT.

           ADD WF-ORIG-AMT             TO AR137WS-ORIG-AMT.

           MOVE ARA-PAYMENT-SEQ        TO WF-PAYMENT-SEQ.
           MOVE PRM-COMPANY            TO WF-COMPANY.
342466     MOVE AR137WS-COUNTRY-CODE       TO WF-COUNTRY-CODE.

           WRITE AR137W1-REC.

           MOVE AR137WS-PRL-SV         TO AR137WS-PRL.
           MOVE AR137WS-PRLSV-SV       TO AR137WS-PRLSV.

           PERFORM 1022-LOAD-WORKB
           THRU    1022-END.

       1021-END.

      ******************************************************************
       1022-LOAD-WORKB.
      ******************************************************************

           IF (ACO-INT-BY-DATE-FL                   = "N")
               MOVE ARA-COMPANY                     TO WFB-COMPANY
               MOVE ARA-TRANS-TYPE                  TO WFB-TRANS-TYPE
               MOVE ARA-INVOICE                     TO WFB-TRANS-NBR
               MOVE ARA-PAYMENT-SEQ                 TO WFB-PAYMENT-SEQ
               MOVE ARA-BATCH-NBR                   TO WFB-BATCH-NBR
               MOVE ARA-APP-SEQ                     TO WFB-APP-SEQ
           END-IF.
           
           IF  (ACO-INT-BY-DATE-FL                   = "Y")
           AND (ACM-LATE-PAY-FL                      = "L")
           AND (ARA-RESULT-FL                        = "D")
           AND (AR137WS-LATE-FULLY-PAID              = WS-FALSE)
               GO TO 1022-END.

           IF (ACO-INT-BY-DATE-FL                   = "Y")
               IF (ACM-LATE-PAY-FL                  = "L")
                   MOVE ARA-COMPANY                 TO WFB-COMPANY
                   MOVE ARA-TRANS-TYPE              TO WFB-TRANS-TYPE
                   MOVE ARA-INVOICE                 TO WFB-TRANS-NBR
                   MOVE ARA-PAYMENT-SEQ             TO WFB-PAYMENT-SEQ
                   MOVE ARA-BATCH-NBR               TO WFB-BATCH-NBR
                   MOVE ARA-APP-SEQ                 TO WFB-APP-SEQ
               ELSE
                   MOVE AR137WS-FC-COMPANY (I2)     TO WFB-COMPANY
                   MOVE AR137WS-FC-TRANS-TYPE (I2)  TO WFB-TRANS-TYPE
                   MOVE AR137WS-FC-INVOICE (I2)     TO WFB-TRANS-NBR
                   MOVE AR137WS-FC-PAYMENT-SEQ (I2) TO WFB-PAYMENT-SEQ
                   MOVE AR137WS-FC-BATCH-NBR (I2)   TO WFB-BATCH-NBR
                   MOVE AR137WS-FC-APP-SEQ (I2)     TO WFB-APP-SEQ
               END-IF
           END-IF.

           WRITE AR137W3-REC.

       1022-END.

      ******************************************************************
       1023-LOAD-WORK-INT-BY-DATE.
      ******************************************************************

      ****
      **** CREATE WORKFILE 1 RECORDS FOR LATE FINANCE CHARGES WHEN
      **** USING INTEREST-RATE-BY-DATE-RANGES.
      ****
           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               MOVE ARO-ORIG-AMT                TO AR137WS-LAPAM
               INITIALIZE                          WF-LATE-AMOUNT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-LATE-CHRG           NOT = "Y")
               MOVE AR137WS-FC-OPEN-AMT (I2)    TO AR137WS-LAPAM
               INITIALIZE                          WF-LATE-AMOUNT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-LATE-CHRG               = "Y")
               MOVE AR137WS-FC-OPEN-AMT (I2)    TO AR137WS-LAPAM
               INITIALIZE                          WF-OPEN-AMOUNT
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE ARA-APPLD-AMT               TO AR137WS-LAPAM
               INITIALIZE                          WFC-OPEN-AMOUNT
           END-IF.

           PERFORM 1024-CREATE-ITEMS-FROM-TABLE
           THRU    1024-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > AR137WS-NBR-CHRG).

       1023-END.

      *****************************************************************
       1024-CREATE-ITEMS-FROM-TABLE.
      *****************************************************************

           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE "D"                         TO WFC-TYPE
           ELSE
               MOVE "D"                         TO WF-TYPE
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               MOVE AR137WS-OPEN-AMT            TO WF-OPEN-AMOUNT
           END-IF.
        
           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-LATE-CHRG           NOT = "Y")
               MOVE AR137WS-FC-OPEN-AMT (I2)    TO WF-OPEN-AMOUNT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-LATE-CHRG               = "Y")
               MOVE AR137WS-FC-OPEN-AMT (I2)    TO WF-LATE-AMOUNT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "L")
               MOVE ARA-APPLD-AMT               TO WFC-LATE-AMOUNT
               MOVE AR137WS-INV-PAY-DATE(I1)    TO WFC-DEPOSIT-DATE
               MOVE AR137WS-INV-DUE-DATE(I1)    TO WFC-DUE-DATE
           ELSE
               MOVE AR137WS-INV-PAY-DATE(I1)    TO WF-DEPOSIT-DATE
               MOVE AR137WS-INV-DUE-DATE(I1)    TO WF-DUE-DATE
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               IF (ARO-LAST-FC-DATE             = ZEROES)
                   MOVE ARO-DUE-DATE            TO WFC-ARO-DUE-DATE
               ELSE
                   MOVE ARO-LAST-FC-DATE        TO WFC-ARO-DUE-DATE
               END-IF
           ELSE
               IF (ARO-LAST-FC-DATE             = ZEROES)
                   MOVE ARO-DUE-DATE            TO WF-ARO-DUE-DATE
               ELSE
                   MOVE ARO-LAST-FC-DATE        TO WF-ARO-DUE-DATE
           END-IF.

           ADD  1                               TO AR137WS-SEQ-NBR.
           IF (AR137WS-SEQ-NBR  = ZEROES)
               ADD 1                            TO AR137WS-SEQ-NBR
           END-IF.
           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE AR137WS-SEQ-NBR             TO WFC-SEQ-NBR
           ELSE
               MOVE AR137WS-SEQ-NBR             TO WF-SEQ-NBR
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               MOVE ARO-TRANS-TYPE              TO WF-TRANS-TYPE
               MOVE ARO-INVOICE                 TO WF-TRANS-NBR
           END-IF.
       
           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
               MOVE AR137WS-FC-TRANS-TYPE (I2)  TO WF-TRANS-TYPE
               MOVE AR137WS-FC-INVOICE (I2)     TO WF-TRANS-NBR
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "L")
               MOVE ARA-TRANS-TYPE              TO WFC-TRANS-TYPE
               MOVE ARA-INVOICE                 TO WFC-TRANS-NBR
               MOVE AR137WS-ARO-CUSTOMER        TO WFC-CUSTOMER
               MOVE ARO-PROCESS-LEVEL           TO WFC-PROCESS-LEVEL
               MOVE AR137WS-RATE-BY-DATE(I1)    TO WFC-CHRG-RATE
               MOVE AR137WS-NO-OF-DAYS(I1)      TO WFC-NO-OF-DAYS
           ELSE
               MOVE AR137WS-ARO-CUSTOMER        TO WF-CUSTOMER
               MOVE ARO-PROCESS-LEVEL           TO WF-PROCESS-LEVEL
               MOVE AR137WS-RATE-BY-DATE(I1)    TO WF-CHRG-RATE
               MOVE AR137WS-NO-OF-DAYS(I1)      TO WF-NO-OF-DAYS
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               MOVE PRM-AS-OF-DATE              TO WSDR-FR-DATE
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
               MOVE AR137WS-FC-TO-DATE (I2)     TO WSDR-FR-DATE
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "L")
               MOVE ARA-DEPOSIT-DATE            TO WSDR-FR-DATE
           END-IF.

           PERFORM 900-DATE-TO-JULIAN.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               COMPUTE AR137WS-DPF              = WSDR-JULIAN-DAYS
                                                - AR137WS-LDAT-FIN
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-LATE-CHRG           NOT = "Y")
               MOVE WSDR-JULIAN-DAYS            TO AR137WS-FC-HOLD-DAYS
               IF (I2 = 1)
                   MOVE AR137WS-DUE-DATE-PLUS   TO WSDR-FR-DATE
               ELSE
                   MOVE AR137WS-FC-FROM-DATE (I2) TO WSDR-FR-DATE
               END-IF
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPF              = AR137WS-FC-HOLD-DAYS
                                                - WSDR-JULIAN-DAYS
               IF (AR137WS-FC-LAST-REC (I2)     = "Y")
                   ADD 1                        TO AR137WS-DPF
               END-IF
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-LATE-CHRG               = "Y")
               IF (I2                           = 1)
                   COMPUTE AR137WS-DPL          = WSDR-JULIAN-DAYS
                                                - AR137WS-LDAT-FIN
                                                - 1
               ELSE
                   MOVE WSDR-JULIAN-DAYS        TO AR137WS-FC-HOLD-DAYS
                   MOVE AR137WS-FC-FROM-DATE (I2) TO WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   COMPUTE AR137WS-DPL          = AR137WS-FC-HOLD-DAYS
                                                - WSDR-JULIAN-DAYS
               END-IF
           END-IF.
               
           IF (ACM-LATE-PAY-FL                  = "L")
               COMPUTE AR137WS-DPL              =  WSDR-JULIAN-DAYS
                                                -  AR137WS-LDAT-LATE
                                                - 1
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               MOVE AR137WS-DPF                 TO WF-DPF
               IF (I1 = 1)
                   MOVE AR137WS-DPF             TO WF-DPF-EFF
               ELSE
                   MOVE 0                       TO WF-DPF-EFF
               END-IF
               INITIALIZE                          WF-DPL
               INITIALIZE                          WF-DPL-EFF
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-LATE-CHRG           NOT = "Y")
               MOVE AR137WS-DPF                 TO WF-DPF
               IF (I1 = 1)
                   MOVE AR137WS-DPF             TO WF-DPF-EFF
               ELSE                           
                   MOVE 0                       TO WF-DPF-EFF
               END-IF
               INITIALIZE                          WF-DPL
                                                   WF-DPL-EFF
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-LATE-CHRG               = "Y")
               MOVE AR137WS-DPL                 TO WF-DPL
               IF (I1 = 1)
                   MOVE AR137WS-DPL             TO WF-DPL-EFF
               ELSE
                   MOVE 0                       TO WF-DPL-EFF
               END-IF
               INITIALIZE                          WF-DPF
               INITIALIZE                          WF-DPF-EFF
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE AR137WS-DPL                 TO WFC-DPL
               IF (I1 = 1)
                   MOVE AR137WS-DPL             TO WFC-DPL-EFF
               ELSE
                   INITIALIZE                      WFC-DPL-EFF
               END-IF
               INITIALIZE                          WFC-DPF
                                                   WFC-DPF-EFF
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE ARO-TRANS-DATE              TO WFC-TRANS-DATE
           ELSE
               MOVE ARO-TRANS-DATE              TO WF-TRANS-DATE
           END-IF.

           IF (ACM-LATE-PAY-FL                = "L")
               IF (WFC-TRANS-TYPE             = "C")
                   COMPUTE WFC-FINANCE-AMOUNT = AR137WS-AMT-BY-DATE (I1)
                                              * NEGATIVE-ONE
               ELSE
                   MOVE AR137WS-AMT-BY-DATE (I1) TO WFC-FINANCE-AMOUNT
               END-IF
           ELSE
               IF (WF-TRANS-TYPE              = "C")
                   COMPUTE WF-FINANCE-AMOUNT  = AR137WS-AMT-BY-DATE (I1)
                                              * NEGATIVE-ONE
               ELSE
                   MOVE AR137WS-AMT-BY-DATE (I1) TO WF-FINANCE-AMOUNT
               END-IF
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE AR137WS-MEMO-FINCHG-REC-FLG TO WFC-MEMO-CHG-FL
           ELSE
               MOVE AR137WS-MEMO-FINCHG-REC-FLG TO WF-MEMO-CHG-FL
           END-IF.

      **** ACCUMULATE LATE CHARGES
           IF (AR137WS-MEMO-FINCHG-REC-FLG      = SPACES)
               IF (ACM-LATE-PAY-FL              = "L")
                   IF (WFC-TRANS-TYPE           = "C")
                       COMPUTE AR137WS-AMT-BY-DATE (I1) =
                                                AR137WS-AMT-BY-DATE (I1)
                                                * NEGATIVE-ONE
                   END-IF
                   ADD AR137WS-AMT-BY-DATE (I1) TO AR137WS-CALC-FNET
               ELSE
                   IF (WF-TRANS-TYPE            = "C")
                       COMPUTE AR137WS-AMT-BY-DATE (I1) =
                                                AR137WS-AMT-BY-DATE (I1)
                                                * NEGATIVE-ONE
                   END-IF
                   ADD AR137WS-AMT-BY-DATE (I1) TO AR137WS-FNET
               END-IF
           END-IF.

           IF (ACO-CURRENCY-CD NOT = ACM-CURRENCY-CD)
               MOVE PRM-COMPANY             TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD         TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD         TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                    TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE          TO IFCAWS-EFFECT-DATE
               MOVE ZEROS                   TO IFCAWS-BASE-AMOUNT
               MOVE WFC-FINANCE-AMOUNT      TO IFCAWS-TRAN-AMOUNT
               MOVE AR137WS-CURR-RATE       TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV       TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND      TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND             TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               IF (ACM-LATE-PAY-FL          = "L")
                   MOVE IFCAWS-BASE-AMOUNT  TO WFC-ORIG-AMT
                   MOVE AR137WS-CURR-RATE   TO WFC-ORIG-RATE
                   MOVE AR137WS-CURR-MUDV   TO WFC-CURR-MUDV
                   MOVE IFCCWS-CURRENCY-ND  TO WFC-ORIG-ND
                   MOVE ACM-CURRENCY-CD     TO WFC-ORIG-CURRENCY
               ELSE
                   MOVE IFCAWS-BASE-AMOUNT  TO WF-ORIG-AMT
                   MOVE AR137WS-CURR-RATE   TO WF-ORIG-RATE
                   MOVE AR137WS-CURR-MUDV   TO WF-CURR-MUDV
                   MOVE IFCCWS-CURRENCY-ND  TO WF-ORIG-ND
                   MOVE ACM-CURRENCY-CD     TO WF-ORIG-CURRENCY
               END-IF
           ELSE
               IF (ACM-LATE-PAY-FL          = "L")
                   MOVE ACO-CURRENCY-CD     TO WFC-ORIG-CURRENCY
                   MOVE 1                   TO WFC-ORIG-RATE
                   MOVE "M"                 TO WFC-CURR-MUDV
                   MOVE ACO-BASE-ND         TO WFC-ORIG-ND
                   MOVE WFC-FINANCE-AMOUNT  TO WFC-ORIG-AMT
               ELSE
                   MOVE ACO-CURRENCY-CD     TO WF-ORIG-CURRENCY
                   MOVE 1                   TO WF-ORIG-RATE
                   MOVE "M"                 TO WF-CURR-MUDV
                   MOVE ACO-BASE-ND         TO WF-ORIG-ND
                   MOVE WF-FINANCE-AMOUNT   TO WF-ORIG-AMT.

           IF (ACM-LATE-PAY-FL                  = "L")
               ADD WFC-ORIG-AMT                 TO AR137WS-CALC-ORIG-AMT
           ELSE
               ADD WF-ORIG-AMT                  TO AR137WS-ORIG-AMT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               MOVE ARO-PAYMENT-SEQ             TO WF-PAYMENT-SEQ
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
               MOVE AR137WS-FC-PAYMENT-SEQ (I2) TO WF-PAYMENT-SEQ
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "L")
               MOVE ARA-PAYMENT-SEQ             TO WFC-PAYMENT-SEQ
               MOVE ARA-BATCH-NBR               TO WFC-BATCH-NBR
               MOVE ARA-APP-SEQ                 TO WFC-APP-SEQ
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               MOVE PRM-COMPANY                 TO WFC-COMPANY
           ELSE
               MOVE PRM-COMPANY                 TO WF-COMPANY
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG      = "Y")
               INITIALIZE                          WF-LATE-AMOUNT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-LATE-CHRG           NOT = "Y")
               INITIALIZE                          WF-LATE-AMOUNT
           END-IF.

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-LATE-CHRG               = "Y")
               INITIALIZE                          WF-OPEN-AMOUNT
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               INITIALIZE                          WFC-OPEN-AMOUNT
                                                   WFC-MIN-CHG-FL
           ELSE
               INITIALIZE                          WF-MIN-CHG-FL
                                                   WF-BATCH-NBR
           END-IF.

           IF (ACM-LATE-PAY-FL                  = "L")
               ADD 1                            TO AR137WS-LATE-SEQ-NBR
               MOVE AR137WS-LATE-SEQ-NBR        TO WFC-LATE-SEQ-NBR
           END-IF.

           IF (ACM-LATE-PAY-FL              NOT = "L")
342466         MOVE AR137WS-COUNTRY-CODE    TO WF-COUNTRY-CODE
J66145         MOVE 1                       TO WF-PRINT-FL  
               WRITE AR137W1-REC
           ELSE
               WRITE AR137W4-REC
           END-IF.

           MOVE AR137WS-PRL-SV                  TO AR137WS-PRL.
           MOVE AR137WS-PRLSV-SV                TO AR137WS-PRLSV.

           MOVE "Y"                             TO AR137WS-CHRG-EXIST.

       1024-END.

      ******************************************************************
       1026-FIND-ARAPPLIED.
      ******************************************************************

           INITIALIZE                          WFC-AR137C-KEY.

           MOVE WS-FALSE                    TO AR137CWS-FILE.

           START AR137W4-FILE               KEY NOT < WFC-AR137C-KEY
               INVALID KEY
                   MOVE WS-TRUE             TO AR137CWS-FILE
           END-START.

           IF (AR137C-FOUND)
               READ AR137W4-FILE NEXT RECORD
                   AT END
                       MOVE WS-TRUE         TO AR137CWS-FILE
           ELSE
               GO TO 1026-END
           END-IF.

           IF (WFC-LATE-SEQ-NBR             = ZEROES)
               READ AR137W4-FILE NEXT RECORD
                   AT END
                       MOVE WS-TRUE         TO AR137CWS-FILE
           ELSE
               MOVE WS-TRUE                 TO AR137CWS-FILE
               GO TO 1026-END
           END-IF.

           PERFORM 1028-LOAD-ARAPPLIED
           THRU    1028-END
               UNTIL (AR137C-NOTFOUND).

           ADD AR137WS-CALC-ORIG-AMT        TO AR137WS-ORIG-AMT.
           ADD AR137WS-CALC-FNET            TO AR137WS-FNET.

       1026-END.

      ******************************************************************
       1028-LOAD-ARAPPLIED.
      ******************************************************************

           IF (WFC-LATE-SEQ-NBR         NOT = ZEROES)
               MOVE WFC-COMPANY             TO WF-COMPANY
               MOVE WFC-CUSTOMER            TO WF-CUSTOMER
               MOVE WFC-SEQ-NBR             TO WF-SEQ-NBR
               MOVE WFC-TRANS-TYPE          TO WF-TRANS-TYPE
               MOVE WFC-TRANS-NBR           TO WF-TRANS-NBR
               MOVE WFC-PAYMENT-SEQ         TO WF-PAYMENT-SEQ
               MOVE WFC-ORIG-CURRENCY       TO WF-ORIG-CURRENCY
               MOVE WFC-ORIG-RATE           TO WF-ORIG-RATE
               MOVE WFC-CURR-MUDV           TO WF-CURR-MUDV
               MOVE WFC-ORIG-ND             TO WF-ORIG-ND
               MOVE WFC-ORIG-AMT            TO WF-ORIG-AMT
               MOVE WFC-DUE-DATE            TO WF-DUE-DATE
               MOVE WFC-ARO-DUE-DATE        TO WF-ARO-DUE-DATE
               MOVE WFC-PROCESS-LEVEL       TO WF-PROCESS-LEVEL
               MOVE WFC-OPEN-AMOUNT         TO WF-OPEN-AMOUNT
               MOVE WFC-LATE-AMOUNT         TO WF-LATE-AMOUNT
               MOVE WFC-FINANCE-AMOUNT      TO WF-FINANCE-AMOUNT
474744         ADD WF-FINANCE-AMOUNT        TO AR137WS-CUST-AMT
474744         MOVE 1                       TO WF-PRINT-FL
               MOVE WFC-DPF                 TO WF-DPF
               MOVE WFC-DPL                 TO WF-DPL
               MOVE WFC-TYPE                TO WF-TYPE
               MOVE WFC-MIN-CHG-FL          TO WF-MIN-CHG-FL
               MOVE WFC-TRANS-DATE          TO WF-TRANS-DATE
               MOVE WFC-CHRG-RATE           TO WF-CHRG-RATE
               MOVE WFC-NO-OF-DAYS          TO WF-NO-OF-DAYS
               MOVE WFC-DPL-EFF             TO WF-DPL-EFF
               MOVE WFC-DEPOSIT-DATE        TO WF-DEPOSIT-DATE
               MOVE WFC-MEMO-CHG-FL         TO WF-MEMO-CHG-FL
               MOVE WFC-DPF-EFF             TO WF-DPF-EFF
               MOVE WFC-BATCH-NBR           TO WF-BATCH-NBR
           END-IF.

342466     MOVE AR137WS-COUNTRY-CODE        TO WF-COUNTRY-CODE.
           WRITE AR137W1-REC.

           ADD 1                            TO AR137WS-W1-REC-CNT.

           READ AR137W4-FILE NEXT RECORD
               AT END
                   MOVE WS-TRUE             TO AR137CWS-FILE.

       1028-END.

      ******************************************************************
       1030-DISPUTES.
      ******************************************************************

           IF (ADP-DISPUTE-DATE > PRM-AS-OF-DATE)
               GO TO 1030-FIND-NEXT-ARDISPUTE.

           IF  (ADP-RESOLV-DATE NOT = ZEROES)
           AND (ADP-RESOLV-DATE    <= PRM-AS-OF-DATE)
               GO TO 1030-FIND-NEXT-ARDISPUTE.

           ADD ADP-DISPUTE-AMT             TO AR137WS-APAM.

       1030-FIND-NEXT-ARDISPUTE.

           PERFORM 860-FIND-NXTRNG-ADPSET1.

       1030-END.

      ******************************************************************
       1037-CREDIT-MEMO.
      ******************************************************************

           IF (ARO-DUE-DATE > PRM-AS-OF-DATE)
               GO TO 1037-END.

           MOVE "Y"                        TO AR137WS-RECF.
           MOVE ARO-TRAN-AMT               TO AR137WS-OPENC.
           IF (ARO-APPLIED-SEQ NOT = ZEROES)
               PERFORM 1045-CALC-AMO-OPEN-AMT
               THRU    1045-AMO-END.

           IF (AR137WS-RECF = "Y")
               ADD AR137WS-OPENC           TO AR137WS-CRED.

       1037-END.

      ******************************************************************
       1045-CALC-AMO-OPEN-AMT.
      ******************************************************************

           MOVE ARO-COMPANY                TO DB-CR-COMPANY.
           MOVE ARZ-CUSTOMER               TO DB-CR-CUSTOMER.
           MOVE ARO-TRANS-TYPE             TO DB-CR-TYPE.
           MOVE ARO-INVOICE                TO DB-CR-NBR.
           MOVE ARASET3-CR-NBR             TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-ARASET3.
           PERFORM
               UNTIL (ARAPPLIED-NOTFOUND)

      **** "0" = UNRELEASED
                   IF (ARA-STATUS NOT = ZEROES)
                       IF (ARA-GL-DATE <= PRM-AS-OF-DATE)
                           SUBTRACT ARA-APPLD-AMT FROM AR137WS-OPENC
                       END-IF
                   END-IF
                   PERFORM 860-FIND-NXTRNG-ARASET3

           END-PERFORM.

           IF (AR137WS-OPENC = ZEROES)
               MOVE "N"                    TO AR137WS-RECF.

       1045-AMO-END.

      ******************************************************************
       1046-FINANCE-NET.
      ******************************************************************

           INITIALIZE                         WF-MIN-CHG-FL.

      **** IF INTEREST-RATES-BY-DATES IS = "Y"
      **** THEN ACM-FIN-CALC-TYPE MUST BE = "D" (DETAIL METHOD)

           IF (ACO-INT-BY-DATE-FL = "Y")
               GO TO 1046-NEXT
           END-IF.

      **** THIS ROUTINE COMPARES THE SUMMARY OF THE FINANCE CHARGES 
      **** CALCULATED FOR A DETAIL METHOD ARCUSTOMER OR THE AMOUNT
      **** OF FINANCE CHARGES FOR A NET METHOD ARCUSTOMER AND 
      **** COMPARES THE TOTAL TO THE MINIMUM FINANCE CHARGE LIMIT 
      **** FOR THE ARCUSTOMER.

      ****
      **** NET METHOD ARCUSTOMER
      ****
           IF (ACM-FIN-CALC-TYPE               = "N")
               MOVE "N"                        TO WF-TYPE
               MOVE AR137WS-PNMT               TO WF-OPEN-AMOUNT
               IF (AR137WS-FNET                <  ACM-FIN-MIN-CHRG)
                   IF (ACM-MINIMUM             = "Y")
                       MOVE ACM-FIN-MIN-CHRG   TO AR137WS-FNET
                       MOVE "*"                TO WF-MIN-CHG-FL
                   END-IF
                   IF (ACM-MINIMUM             = "N")
                       INITIALIZE                 AR137WS-FNET
                   END-IF
               END-IF     
               MOVE AR137WS-FNET               TO AR137WS-ORIG-AMT
           END-IF.

      ****
      **** DETAIL METHOD ARCUSTOMER
      ****
           IF (ACM-FIN-CALC-TYPE               = "D")
               MOVE "D"                        TO WF-TYPE
               MOVE AR137WS-PNMT               TO WF-OPEN-AMOUNT
               IF (AR137WS-FNET                <  ACM-FIN-MIN-CHRG)
                   IF (ACM-MINIMUM             = "Y")
                       MOVE ACM-FIN-MIN-CHRG   TO AR137WS-FNET
                       MOVE "*"                TO WF-MIN-CHG-FL
                   END-IF
                   IF (ACM-MINIMUM             = "N")
                       MOVE "*"                TO WF-MIN-CHG-FL
                       INITIALIZE                 AR137WS-FNET
                   END-IF
               END-IF
           END-IF.

           IF (AR137WS-FNET           = ZEROES)  
               IF (ACM-FIN-CALC-TYPE  = "N")
                   GO TO 1046-END
               END-IF
               IF  (ACM-FIN-CALC-TYPE = "D")
               AND (WF-MIN-CHG-FL     = "*")
                    GO TO 1046-CONT
               END-IF
               IF  (ACM-FIN-CALC-TYPE = "D")
               AND (WF-MIN-CHG-FL     =  SPACES)
                    GO TO 1046-END
               END-IF
           END-IF.

       1046-NEXT.

           IF (ACO-INT-BY-DATE-FL              = "Y")
               IF (AR137WS-FNET                <= ZEROES)
                   MOVE "*"                    TO WF-MIN-CHG-FL
                   GO TO 1046-CONT
               END-IF
               IF (AR137WS-FNET                < ACM-FIN-MIN-CHRG)
                   IF (ACM-MINIMUM             = "Y")
                       MOVE ACM-FIN-MIN-CHRG   TO AR137WS-FNET
                       MOVE "*"                TO WF-MIN-CHG-FL
                   END-IF
                   IF (ACM-MINIMUM             = "N")
                       INITIALIZE                 AR137WS-FNET
                       MOVE "*"                TO WF-MIN-CHG-FL
                   END-IF
               END-IF
           END-IF.

           IF (ACO-CURRENCY-CD NOT = ACM-CURRENCY-CD)
               PERFORM 1050-EDIT-CURRENCY
               THRU    1050-END
               IF (ERROR-FOUND)
                   GO TO 1046-END.

           IF (ACM-CURRENCY-CD          NOT = ACO-CURRENCY-CD)

      **** TRIANGULATION REQUIRED
               MOVE PRM-COMPANY             TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD         TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD         TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                    TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE          TO IFCAWS-EFFECT-DATE
               MOVE AR137WS-FNET            TO IFCAWS-TRAN-AMOUNT
               MOVE ZEROS                   TO IFCAWS-BASE-AMOUNT
               MOVE AR137WS-CURR-RATE       TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV       TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND      TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND             TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               IF (ACM-FIN-CALC-TYPE        = "N")
                   IF (IFCAWS-BASE-AMOUNT       = ZERO)
                       GO TO 1046-CONT
                   END-IF
               END-IF
               IF (ACM-FIN-CALC-TYPE        = "D")
                   IF (IFCAWS-BASE-AMOUNT       = ZERO)
                       GO TO 1046-CONT
                   END-IF
               END-IF
               MOVE IFCAWS-BASE-AMOUNT      TO AR137WS-ORIG-AMT
           ELSE
               MOVE AR137WS-FNET            TO AR137WS-ORIG-AMT
           END-IF.

           IF (AR137WS-ORIG-AMT             = ZERO)
               GO TO 1046-CONT.

           IF (ACO-DTL-FIN-CHRG             = "Y")
              IF (AR137WS-PRL               = "N")
                  MOVE AR137WS-PRLSV        TO AR137WS-PROCL
              ELSE
                  MOVE PRM-PROCESS-LEVEL    TO AR137WS-PROCL.

      **** CREATE WORKFILE RECORDS TO CREATE ARDISTRIB (AR-DISTRIBUTION)
           IF (ACO-DTL-FIN-CHRG               = "N")
               IF (AR137WS-PRL                = "N")
                   MOVE AR137WS-PRLSV         TO AR137WS-PROCL
                   MOVE PRM-COMPANY           TO WFA-COMPANY
                   MOVE AR137WS-PROCL         TO WFA-PROCESS-LEVEL
                   IF (ACM-AR-CODE        NOT = SPACES)
                       MOVE ACM-AR-CODE       TO WFA-AR-CODE
                   ELSE
                       MOVE PRM-COMPANY       TO DB-COMPANY
                       MOVE AR137WS-PROCL     TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-APVSET1
                       MOVE APV-AR-CODE       TO WFA-AR-CODE
                   END-IF
                   MOVE WS-FALSE              TO AR137AWS-FILE
                   READ AR137W2-FILE KEY WFA-AR137A-KEY
                        INVALID KEY
                            MOVE WS-TRUE      TO AR137AWS-FILE
                   END-READ
                   IF (ACO-LATE-CH-RND-FL     = "Y")
                       COMPUTE AR137WS-AMT-ROUND ROUNDED
                                              = AR137WS-FNET
                       COMPUTE AR137WS-FNET   = AR137WS-AMT-ROUND
                   END-IF
                   IF (AR137A-FOUND)
                       ADD AR137WS-FNET       TO WFA-AMOUNT
                       REWRITE AR137W2-REC
                   ELSE
                       MOVE PRM-COMPANY       TO WFA-COMPANY
                       MOVE AR137WS-PROCL     TO WFA-PROCESS-LEVEL
                       IF (ACM-AR-CODE    NOT = SPACES)
                           MOVE ACM-AR-CODE   TO WFA-AR-CODE
                       ELSE
                           MOVE APV-AR-CODE   TO WFA-AR-CODE
                       END-IF
                       MOVE AR137WS-FNET      TO WFA-AMOUNT
                       MOVE "N"               TO WFA-PROC-LEVEL-FL
                       WRITE AR137W2-REC
                   END-IF
               END-IF   
           END-IF.
    
           IF (ACO-DTL-FIN-CHRG = "N")
               IF (AR137WS-PRL                = "Y")
                   MOVE PRM-PROCESS-LEVEL     TO AR137WS-PROCL
                   MOVE PRM-COMPANY           TO WFA-COMPANY
                   MOVE AR137WS-PROCL         TO WFA-PROCESS-LEVEL
                   IF (ACM-AR-CODE        NOT = SPACES)
                       MOVE ACM-AR-CODE       TO WFA-AR-CODE
                   ELSE
                       MOVE PRM-COMPANY       TO DB-COMPANY
                       MOVE AR137WS-PROCL     TO DB-PROCESS-LEVEL
                       PERFORM 840-FIND-APVSET1
                       MOVE APV-AR-CODE       TO WFA-AR-CODE
                   END-IF
                   MOVE WS-FALSE              TO AR137AWS-FILE
                   READ AR137W2-FILE KEY WFA-AR137A-KEY
                        INVALID KEY
                            MOVE WS-TRUE      TO AR137AWS-FILE
                   END-READ
                   IF (ACO-LATE-CH-RND-FL     = "Y")
                       COMPUTE AR137WS-AMT-ROUND ROUNDED
                                              = AR137WS-FNET
                       COMPUTE AR137WS-FNET   = AR137WS-AMT-ROUND
                   END-IF
                   IF (AR137A-FOUND)
                       ADD AR137WS-FNET       TO WFA-AMOUNT
                       REWRITE AR137W2-REC
                   ELSE
                       MOVE PRM-COMPANY        TO WFA-COMPANY
                       MOVE AR137WS-PROCL      TO WFA-PROCESS-LEVEL
                       IF (ACM-AR-CODE     NOT = SPACES)
                           MOVE ACM-AR-CODE    TO WFA-AR-CODE
                       ELSE
                           MOVE APV-AR-CODE    TO WFA-AR-CODE
                       END-IF
                       MOVE AR137WS-FNET       TO WFA-AMOUNT
                       MOVE "Y"                TO WFA-PROC-LEVEL-FL
                       WRITE AR137W2-REC
                   END-IF
               END-IF
           END-IF.

       1046-CONT.

           INITIALIZE                         WF-TRANS-TYPE.

           IF (ACM-FIN-CALC-TYPE = "N")
               ADD 1                       TO AR137WS-SEQ-NBR
               IF (AR137WS-SEQ-NBR  = ZEROES)
                    ADD 1                  TO AR137WS-SEQ-NBR
               END-IF
               MOVE AR137WS-SEQ-NBR        TO WF-SEQ-NBR
           ELSE
               INITIALIZE                     WF-SEQ-NBR
           END-IF.

           MOVE AR137WS-FNET               TO WF-FINANCE-AMOUNT.
474744     ADD WF-FINANCE-AMOUNT           TO AR137WS-CUST-AMT.
474744     MOVE 1                          TO WF-PRINT-FL.
           MOVE AR137WS-ORIG-AMT           TO WF-ORIG-AMT.

           INITIALIZE                         WF-DPF
                                              WF-DPL
                                              WF-DUE-DATE
                                              WF-ARO-DUE-DATE
                                              WF-TRANS-NBR
                                              WF-TRANS-DATE.

           IF (ACO-CURRENCY-CD         NOT = ACM-CURRENCY-CD)
               IF (AR137WS-CURR-RATE   NOT = ZEROES)
                   MOVE AR137WS-CURR-RATE  TO WF-ORIG-RATE
               ELSE
                   COMPUTE WF-ORIG-RATE ROUNDED =
                                       1 / IFGRWS-SELL-RATE
               END-IF
               MOVE IFCRWS-MULT-DIV        TO WF-CURR-MUDV
               MOVE IFCCWS-CURRENCY-ND     TO WF-ORIG-ND
               MOVE ACM-CURRENCY-CD        TO WF-ORIG-CURRENCY
           ELSE
               MOVE ACO-CURRENCY-CD        TO WF-ORIG-CURRENCY
               MOVE 1                      TO WF-ORIG-RATE
               MOVE "M"                    TO WF-CURR-MUDV
               MOVE ACO-BASE-ND            TO WF-ORIG-ND.

           IF (WF-TYPE                     = "N")
               IF (ACO-CURRENCY-CD     NOT = ACM-CURRENCY-CD)
                   MOVE PRM-COMPANY        TO IFCAWS-COMPANY
                   MOVE ACO-CURRENCY-CD    TO IFCAWS-FR-CURR-CODE
                   MOVE ACM-CURRENCY-CD    TO IFCAWS-TO-CURR-CODE
                   MOVE "AR"               TO IFCAWS-SYSTEM
                   MOVE PRM-AS-OF-DATE     TO IFCAWS-EFFECT-DATE
                   MOVE ZEROS              TO IFCAWS-BASE-AMOUNT
                   MOVE AR137WS-FNET       TO IFCAWS-TRAN-AMOUNT
                   MOVE AR137WS-CURR-RATE  TO IFCAWS-BASERATE
                   MOVE AR137WS-CURR-MUDV  TO IFCAWS-MULT-DIV
                   MOVE IFCCWS-CURRENCY-ND TO IFCAWS-BASE-ND
                   MOVE ACO-BASE-ND        TO IFCAWS-TRAN-ND
                   PERFORM 690-CALCULATE-AMOUNT-60
                   MOVE IFCAWS-BASE-AMOUNT TO WF-ORIG-AMT
               ELSE
                   MOVE AR137WS-FNET       TO WF-ORIG-AMT.

           MOVE ACM-CUSTOMER               TO WF-CUSTOMER.

           IF (ARFINRATE-FOUND)
               MOVE AFI-CHRG-RATE          TO WF-CHRG-RATE
           ELSE
               INITIALIZE                     WF-CHRG-RATE
           END-IF.

           MOVE PRM-COMPANY                TO WF-COMPANY.
           MOVE AR137WS-PROCL              TO WF-PROCESS-LEVEL.

           INITIALIZE                         WF-NO-OF-DAYS.
           INITIALIZE                         WF-DPL-EFF.
           INITIALIZE                         WF-DEPOSIT-DATE.
           INITIALIZE                         WF-MEMO-CHG-FL.
           INITIALIZE                         WF-PAYMENT-SEQ.
           INITIALIZE                         WF-LATE-AMOUNT.
           INITIALIZE                         WF-DEPOSIT-DATE.
           INITIALIZE                         WF-DPF-EFF.
           INITIALIZE                         WF-BATCH-NBR.

342466     MOVE AR137WS-COUNTRY-CODE        TO WF-COUNTRY-CODE.

           WRITE AR137W1-REC.

       1046-END.

      ******************************************************************
       1050-EDIT-CURRENCY.
      ******************************************************************

           MOVE ACM-CURRENCY-CD          TO IFCCWS-CURRENCY-CODE.
           PERFORM 605-EDIT-CURRENCY-CODE-60.
           IF  (ERROR-FOUND)
           AND (IFCCWS-CURSOR-POSITION   = "C")
               MOVE ACM-CUSTOMER         TO D3-ERR-CUSTOMER
               MOVE ACM-CURRENCY-CD      TO D3-FIELD-NAME
               MOVE 202                  TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE          TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS   TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                    TO AR137WS-ERROR-CNT
               MOVE "Y"                  TO AR137WS-END-PROG
               GO TO 1050-END.

            MOVE PRM-COMPANY             TO IFCRWS-COMPANY.
            MOVE ACM-CURRENCY-CD         TO IFCRWS-TO-CURR-CODE.
            MOVE ACO-CURRENCY-CD         TO IFCRWS-FR-CURR-CODE.
            PERFORM 655-EDIT-CURRENCY-RELATION-60.
            IF  (ERROR-FOUND)  
            AND (IFCRWS-CURSOR-POSITION  = "C")
               MOVE ACM-CUSTOMER         TO D3-ERR-CUSTOMER
               MOVE ACM-CURRENCY-CD      TO D3-FIELD-NAME
               MOVE 201                  TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE          TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS   TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                    TO AR137WS-ERROR-CNT
               MOVE "Y"                  TO AR137WS-END-PROG
               GO TO 1050-END.

           MOVE PRM-COMPANY              TO IFGRWS-COMPANY.
           MOVE ACO-CURRENCY-CD          TO IFGRWS-FR-CURR-CODE.
           MOVE ACM-CURRENCY-CD          TO IFGRWS-TO-CURR-CODE.
           MOVE "AR"                     TO IFGRWS-SYSTEM.
           MOVE PRM-AS-OF-DATE           TO IFGRWS-EFFECT-DATE.
           PERFORM 660-GET-CURRENCY-RATE-60.
           IF  (ERROR-FOUND)
           AND (IFGRWS-CURSOR-POSITION   = "C")
               MOVE ACM-CUSTOMER         TO D3-ERR-CUSTOMER
               MOVE ACM-CURRENCY-CD      TO D3-FIELD-NAME
               MOVE 200                  TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE          TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS   TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                    TO AR137WS-ERROR-CNT
               MOVE "Y"                  TO AR137WS-END-PROG
               GO TO 1050-END.

           MOVE IFGRWS-SELL-RATE         TO AR137WS-CURR-RATE.
           MOVE IFCRWS-MULT-DIV          TO AR137WS-CURR-MUDV.
           MOVE WS-TRUE                  TO AR137WS-EDIT-CURR.

       1050-END.

      ******************************************************************
       1060-CALC-CHRG-BY-DATE.
      ******************************************************************
      *
      *    This routine is used for reading the different rate charges
      *    per date in the file ARFINRATE.
      *    Regarding the fact that the charge rate can have changed 
      *    during the period due date - pay date, the late pay charge is
      *    broken into pieces for an item, one per charge rate.
      *    Information about the different subitems is stored in the 
      *    AR137WS-CHRG to be used later in 1024-CREATE-ITEMS-FROM-TABLE
      *
      *    The table may contain multiple lines when the following
      *    condition is true:
      *         (ACM-LATE-PAY-FL    = "L" OR "B") AND
      *         (ARA-RESULT-FL      = "L" OR "D") AND
      *         (ACO-INT-BY-DATE-FL = "Y")        AND
      *         (ARO-LATE-CH-FL     = "Y").
      *
      *    The average rate is calculated to be used in 1010 instead
      *    of using same rate for all customer items as used earlier.
      *
      *    The error code AR137WS-CHRG-ERROR is set to Y when
      *    one of the following conditions is true. 
      *        - No valid record for interest rate is found in ARFINRATE
      *        - if calculated interest amount is less than 
      *            ACM-I-FIN-MIN-CHRG
      ******************************************************************

           INITIALIZE  AR137WS-NBR-CHRG
                       AR137WS-NBR-OF-DAYS-ITEM.

           PERFORM 1061-INIT-CHRG-TABLE
           THRU    1061-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > 730).

      **** FOLLOWING LINE OF CODE IS CURRENTLY NOT USED
      **** MOVE "Y"    TO AR137WS-CHRG-ERROR.
      ****

      **** Get the first charge rate that has an effective date less
      **** or equal to the (Due-Date, plus one day).
      **** Note, the field EFF-DATE within the AFISET2 index is
      **** in descending order.

      ****
      **** DETERMINE DATE OF ARO-DUE-DATE + 1
      ****                OR ARO-LAST-FC-DATE
      ****
           IF (ACM-LATE-PAY-FL                 = "L")
               MOVE ARO-DUE-DATE               TO WSDR-FR-DATE.

           IF (ACM-LATE-PAY-FL                 = "B")
               IF  (AR137WS-ARO-TOTAL-FIN-CHRG = "Y")
               OR  (AR137WS-LATE-CHRG          = "Y")
               AND (I2                         = 1)
                   IF (ARO-LAST-FC-DATE        = ZEROES)
                       MOVE ARO-DUE-DATE       TO WSDR-FR-DATE
                   ELSE
                       MOVE ARO-LAST-FC-DATE   TO WSDR-FR-DATE
                   END-IF
               ELSE
                   MOVE AR137WS-FC-FROM-DATE (I2)  TO WSDR-FR-DATE
           END-IF.

           PERFORM 900-DATE-TO-JULIAN.
           COMPUTE WSDR-JULIAN-DAYS            = (WSDR-JULIAN-DAYS
                                               +  1).
           PERFORM 900-JULIAN-TO-DATE.
           MOVE WSDR-FR-DATE                 TO AR137WS-DUE-DATE-PLUS.

      ****
      **** GET LATEST FINANCE RATE IN EFFECT AS OF ARO-DUE-DATE
      ****
           MOVE ACM-FIN-CHRG-CD           TO DB-FIN-CHRG-CD.
           MOVE AR137WS-DUE-DATE-PLUS     TO DB-EFF-DATE.
           PERFORM 850-FIND-NLT-AFISET2.
           IF (ARFINRATE-NOTFOUND)
           OR (AFI-FIN-CHRG-CD            NOT = ACM-FIN-CHRG-CD)
               MOVE ACM-CUSTOMER          TO D3-ERR-CUSTOMER
               MOVE ACM-FIN-CHRG-CD       TO D3-FIELD-NAME
               MOVE 205                   TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE           TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS    TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                     TO AR137WS-ERROR-CNT
               MOVE "Y"                   TO AR137WS-RATE-ERROR
               GO TO 1060-END.

      ****
      **** Create first row in table.
      ****
           MOVE 1                         TO AR137WS-NBR-CHRG.
           MOVE AFI-CHRG-RATE             TO AR137WS-RATE-BY-DATE
                                                (AR137WS-NBR-CHRG).

           MOVE AR137WS-DUE-DATE-PLUS     TO AR137WS-ST-DATE
                                               (AR137WS-NBR-CHRG)
                                             AR137WS-INV-DUE-DATE
                                               (AR137WS-NBR-CHRG)
                                             WSDR-FR-DATE.

           IF  (ACM-LATE-PAY-FL             = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  = "Y")
               MOVE PRM-AS-OF-DATE          TO WSDR-TO-DATE
                                               AR137WS-INV-PAY-DATE
                                                  (AR137WS-NBR-CHRG)
           END-IF.

           IF  (ACM-LATE-PAY-FL             = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
               MOVE AR137WS-FC-TO-DATE (I2) TO WSDR-TO-DATE
                                               AR137WS-INV-PAY-DATE
                                                  (AR137WS-NBR-CHRG)
           END-IF.

           IF  (ACM-LATE-PAY-FL             = "L")
               MOVE ARA-DEPOSIT-DATE        TO WSDR-TO-DATE 
                                               AR137WS-INV-PAY-DATE
                                                  (AR137WS-NBR-CHRG)
           END-IF.

           PERFORM 900-NBR-DAYS-IN-DATE-RNG.

           MOVE WSDR-NBR-DAYS             TO AR137WS-NO-OF-DAYS
                                                (AR137WS-NBR-CHRG).
           IF (AR137WS-ARO-TOTAL-FIN-CHRG   = "Y")
               ADD 1                        TO AR137WS-NO-OF-DAYS
                                                (AR137WS-NBR-CHRG).

      **** Create more lines in table if needed.
           MOVE AFI-FIN-CHRG-CD       TO DB-FIN-CHRG-CD.
           MOVE AFI-EFF-DATE          TO DB-EFF-DATE.
           PERFORM 850-FIND-NLT-AFISET1.
           PERFORM 860-FIND-NEXT-AFISET1.
           IF (ARFINRATE-FOUND)
               IF  (ACM-LATE-PAY-FL               = "B")
               AND (AR137WS-ARO-TOTAL-FIN-CHRG    = "Y")
                   PERFORM 1062-MORE-RATE-CHARGES
                   THRU    1062-END
                       UNTIL (ARFINRATE-NOTFOUND)
                       OR    (AFI-FIN-CHRG-CD NOT = ACM-FIN-CHRG-CD)
                       OR    (AFI-EFF-DATE        > PRM-AS-OF-DATE)
               END-IF
               IF  (ACM-LATE-PAY-FL               = "B")
               AND (AR137WS-ARO-TOTAL-FIN-CHRG    NOT = "Y")
                   PERFORM 1062-MORE-RATE-CHARGES
                   THRU    1062-END
                       UNTIL (ARFINRATE-NOTFOUND)
                       OR    (AFI-FIN-CHRG-CD NOT = ACM-FIN-CHRG-CD)
                       OR    (AFI-EFF-DATE >=
                                             AR137WS-FC-TO-DATE (I2))
               END-IF
               IF (ACM-LATE-PAY-FL                = "L")
                   PERFORM 1062-MORE-RATE-CHARGES 
                   THRU    1062-END
                       UNTIL (ARFINRATE-NOTFOUND)
                       OR    (AFI-FIN-CHRG-CD NOT = ACM-FIN-CHRG-CD)
                       OR    (AFI-EFF-DATE        = ARA-DEPOSIT-DATE)
                       OR    (AFI-EFF-DATE        > ARA-DEPOSIT-DATE)
               END-IF
           END-IF.

      *    Now complete the table (if more tan 1 row) by calculating
      *        - Number of days        per row

           PERFORM 1064-NBR-OF-DAYS
           THRU    1064-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > AR137WS-NBR-CHRG).
       
      *     Now complete the table by calculating
      *         - Rate amount           per row
      *         - Number of days        per item
      *         - Amount                per item

       1060-CALC-RATE-AMOUNT.

           PERFORM 1066-RATE-AMOUNT
           THRU    1066-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > AR137WS-NBR-CHRG).

      **** Calculate average rates per customer
      ****
      **** THE FOLLOWING AVERAGE CALCULATION IS NO LONGER USED.
      ****
      **** AR137WS-AMT-BY-DATE-CUST = SUM OF ARA-APPLD-AMT
      **** AR137WS-CHG-TOT-AMT      = SUM OF AR137WS-AMT-BY-DATE(TABLE)
      **** AR137WS-NBR-OF-DAYS-ITEM = SUM OF AR137WS-NO-OF-DAYS(TABLE)
      **** AR137WS-NBR-LINES-CUST   = SUM OF NUMBER OF TABLE ENTRIES

           IF (AR137WS-ARO-TOTAL-FIN-CHRG    NOT = "Y")
               ADD AR137WS-NBR-OF-DAYS-ITEM  TO AR137WS-NBR-OF-DAYS-CUST

               COMPUTE AR137WS-AVG-CHRG-RATE-CUST ROUNDED       
                                       =  (AR137WS-CHRG-TOT-AMT
                                       *  (AR137WS-NBR-LINES-CUST * 30))
                                       /  (AR137WS-NBR-OF-DAYS-CUST
                                       *   AR137WS-AMT-BY-DATE-CUST)
           END-IF.
                                           
      **** When the Invoice Finance Minimum Charge on AR10 is greater
      **** than zero, then do not process the invoice when the program
      **** calculated Invoice Late Charge is less than the
      **** Invoice Minimum Charge. - DO WRITE RECORDS TO AR137W1 FOR 
      **** PRINTING IN PHASE 2 OF THIS PROGRAM.

           MOVE SPACES TO AR137WS-MEMO-FINCHG-REC-FLG.

           IF (ACM-I-FIN-MIN-CHRG > ZERO)
               INITIALIZE AR137WS-CALC-INVOICE-LATE-CHG
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > AR137WS-NBR-CHRG)
                       COMPUTE AR137WS-CALC-INVOICE-LATE-CHG = 
                               AR137WS-CALC-INVOICE-LATE-CHG + 
                               AR137WS-AMT-BY-DATE (I1)
               END-PERFORM
               IF (AR137WS-CALC-INVOICE-LATE-CHG < ACM-I-FIN-MIN-CHRG)
                   MOVE "*"    TO AR137WS-MEMO-FINCHG-REC-FLG
               END-IF
336027         MOVE AR137WS-CALC-INVOICE-LATE-CHG TO WF-TOT-INV-LATE-CHG
           END-IF.

       1060-END.

      ******************************************************************
       1061-INIT-CHRG-TABLE.
      ******************************************************************

           MOVE ZEROES TO              AR137WS-ST-DATE     (I1)
                                       AR137WS-NO-OF-DAYS  (I1)
                                       AR137WS-RATE-BY-DATE(I1)
                                       AR137WS-AMT-BY-DATE (I1) 
                                       AR137WS-INV-DUE-DATE(I1)
                                       AR137WS-INV-PAY-DATE(I1).

       1061-END.

      ******************************************************************
       1062-MORE-RATE-CHARGES.
      ******************************************************************

           ADD 1              TO AR137WS-NBR-CHRG.
           MOVE AFI-CHRG-RATE TO AR137WS-RATE-BY-DATE(AR137WS-NBR-CHRG).
           MOVE AFI-EFF-DATE  TO AR137WS-ST-DATE     (AR137WS-NBR-CHRG).

           PERFORM 860-FIND-NEXT-AFISET1.

       1062-END.

      ******************************************************************
       1064-NBR-OF-DAYS.
      ******************************************************************

           IF  (ACM-LATE-PAY-FL                 = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
               IF (I2 = 1)
                   MOVE AR137WS-ST-DATE (I1)    TO AR137WS-FR-DATE
               ELSE
                   IF (I1 > 1)
                       MOVE AR137WS-TO-DATE     TO AR137WS-FR-DATE
                   ELSE
                       MOVE AR137WS-FC-FROM-DATE (I2) TO AR137WS-FR-DATE
                   END-IF
               END-IF
           ELSE
               MOVE AR137WS-ST-DATE (I1)        TO AR137WS-FR-DATE
           END-IF.

           MOVE AR137WS-ST-DATE (I1)        TO AR137WS-INV-DUE-DATE(I1).

           IF (I1 < AR137WS-NBR-CHRG)
               MOVE AR137WS-ST-DATE(I1 + 1) TO WSDR-FR-DATE
      **** RETURNS WSDR-JULIAN-DAYS ****   
               PERFORM 900-DATE-TO-JULIAN
               IF (AR137WS-ARO-TOTAL-FIN-CHRG   = "Y")
               OR (ACM-LATE-PAY-FL              = "L")
                   COMPUTE WSDR-JULIAN-DAYS     = (WSDR-JULIAN-DAYS
                                                -  1)
               END-IF
               PERFORM 900-JULIAN-TO-DATE 
               MOVE WSDR-FR-DATE           TO AR137WS-TO-DATE
                                              AR137WS-INV-PAY-DATE(I1)
           ELSE
               IF  (ACM-LATE-PAY-FL             = "B")
               AND (AR137WS-ARO-TOTAL-FIN-CHRG  = "Y")
                   MOVE PRM-AS-OF-DATE       TO AR137WS-TO-DATE
                                                AR137WS-INV-PAY-DATE(I1)
               END-IF                                                 
               IF  (ACM-LATE-PAY-FL             = "B")
               AND (AR137WS-ARO-TOTAL-FIN-CHRG  NOT = "Y")
                   MOVE AR137WS-FC-TO-DATE (I2) TO AR137WS-TO-DATE
                                                AR137WS-INV-PAY-DATE(I1)
               END-IF
               IF  (ACM-LATE-PAY-FL             = "L")
                   MOVE ARA-DEPOSIT-DATE     TO AR137WS-TO-DATE
                                                AR137WS-INV-PAY-DATE(I1)
               END-IF
           END-IF.

           MOVE AR137WS-FR-DATE            TO WSDR-FR-DATE.
           MOVE AR137WS-TO-DATE            TO WSDR-TO-DATE.

      **** RETURNS WSDR-NBR-DAYS ****  
           PERFORM 900-NBR-DAYS-IN-DATE-RNG.

           IF  (ACM-LATE-PAY-FL            = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG = "Y")
               COMPUTE AR137WS-NO-OF-DAYS (I1) = (WSDR-NBR-DAYS
                                               +  1)
           END-IF.

           IF  (ACM-LATE-PAY-FL            = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG NOT = "Y")
               COMPUTE AR137WS-NO-OF-DAYS (I1) = WSDR-NBR-DAYS
               IF  (AR137WS-FC-LAST-REC (I2)   = "Y")
               AND (AR137WS-LATE-CHRG      NOT = "Y")
               AND (I1 = AR137WS-NBR-CHRG)
                   ADD 1                   TO AR137WS-NO-OF-DAYS (I1)
               END-IF
           END-IF.

           IF  (ACM-LATE-PAY-FL            = "L")
               IF (WSDR-TO-DATE            = ARA-DEPOSIT-DATE)
                   COMPUTE AR137WS-NO-OF-DAYS (I1) = WSDR-NBR-DAYS
               ELSE
                   COMPUTE AR137WS-NO-OF-DAYS (I1) = (WSDR-NBR-DAYS
                                                   +  1)
               END-IF
           END-IF.

       1064-END.

      ******************************************************************
       1066-RATE-AMOUNT.
      ******************************************************************

           IF  (ACM-LATE-PAY-FL              = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG   = "Y")
               COMPUTE AR137WS-FIN           = AR137WS-OPEN-AMT
                                             * AR137WS-NO-OF-DAYS(I1)
               COMPUTE AR137WS-FIN ROUNDED   = (AR137WS-FIN * 12) / 365
           END-IF.

           IF  (ACM-LATE-PAY-FL              = "B")
           AND (AR137WS-ARO-TOTAL-FIN-CHRG   NOT = "Y")
               ADD AR137WS-FC-OPEN-AMT (I2)  TO AR137WS-AMT-BY-DATE-CUST
               COMPUTE AR137WS-FIN           = AR137WS-FC-OPEN-AMT (I2)
                                             * AR137WS-NO-OF-DAYS(I1)
               COMPUTE AR137WS-FIN ROUNDED   = (AR137WS-FIN * 12) / 365
           END-IF.

           IF  (ACM-LATE-PAY-FL              = "L")
               ADD ARA-APPLD-AMT             TO AR137WS-AMT-BY-DATE-CUST
               COMPUTE AR137WS-FIN           = ARA-APPLD-AMT
                                             * AR137WS-NO-OF-DAYS(I1)
               COMPUTE AR137WS-FIN ROUNDED   = (AR137WS-FIN * 12) / 365
           END-IF.

           IF (ACO-CURRENCY-CD            NOT = ACM-CURRENCY-CD)
               MOVE IFCCWS-CURRENCY-ND    TO IFCAWS-TRAN-ND
           ELSE
               MOVE ACO-BASE-ND           TO IFCAWS-TRAN-ND.

           IF (IFCAWS-TRAN-ND      = 2)
               COMPUTE IFCAWS-BASE-AMT2 ROUNDED = AR137WS-FIN
                                             * AR137WS-RATE-BY-DATE (I1)
               MOVE IFCAWS-BASE-AMT2    TO AR137WS-AMT-BY-DATE (I1)
            ELSE
               COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FIN
                                             * AR137WS-RATE-BY-DATE (I1)
               MOVE IFCAWS-BASE-AMT0    TO AR137WS-AMT-BY-DATE (I1).

           INITIALIZE IFCAWS-BASE-AMT2
                      IFCAWS-BASE-AMT0.

           ADD AR137WS-AMT-BY-DATE(I1)    TO AR137WS-CHRG-TOT-AMT.
           ADD AR137WS-NO-OF-DAYS(I1)     TO AR137WS-NBR-OF-DAYS-ITEM.
           ADD 1                          TO AR137WS-NBR-LINES-CUST.

       1066-END.

      ******************************************************************
       1115-OPEN-PAYMENTS.
      ******************************************************************

           IF (APM-GL-DATE                 > PRM-AS-OF-DATE)
               GO TO 1115-NEXT-ARPAYMENT.

      **** "P" = CUSTOMER PAYMENT
           IF (APM-TRANS-TYPE          NOT = "P")
               GO TO 1115-NEXT-ARPAYMENT.

           IF  (APM-CANCEL-DATE        NOT = ZEROES)
           AND (APM-CANCEL-DATE        NOT > PRM-AS-OF-DATE)
               GO TO 1115-NEXT-ARPAYMENT.

           IF  (APM-TRANSFER-DATE      NOT = ZEROES)
           AND (APM-CUSTOMER               = APM-TRNS-CUST)
               IF (APM-TRANSFER-DATE       > PRM-AS-OF-DATE)
                   GO TO 1115-NEXT-ARPAYMENT.

           IF  (APM-TRANSFER-DATE      NOT = ZEROES)
           AND (APM-CUSTOMER           NOT = APM-TRNS-CUST)
           AND (APM-TRNS-CUST          NOT = SPACES)
           AND (APM-TRANSFER-DATE      NOT > PRM-AS-OF-DATE)
               GO TO 1115-NEXT-ARPAYMENT.

           IF (APM-ADJ-AMT                 = APM-TRAN-AMT)
               GO TO 1115-NEXT-ARPAYMENT.

           IF (APM-LAST-FC-DATE            = ZEROES)
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE APM-BATCH-NBR          TO DB-BATCH-NBR
               PERFORM 840-FIND-APHSET1
               IF (ARPYMNTHDR-FOUND)
                   MOVE APH-DEPOSIT-DATE   TO AR137WS-DUDT-FIN
                                              WSDR-FR-DATE
                   PERFORM 900-DATE-TO-JULIAN
                   MOVE WSDR-JULIAN-DAYS   TO AR137WS-LDAT-FIN
               END-IF
           ELSE
               MOVE APM-LAST-FC-DATE       TO AR137WS-DUDT-FIN
                                              WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               MOVE WSDR-JULIAN-DAYS       TO AR137WS-LDAT-FIN
           END-IF.

           MOVE PRM-AS-OF-DATE             TO WSDR-FR-DATE.
           PERFORM 900-DATE-TO-JULIAN.
           COMPUTE AR137WS-DPF             = WSDR-JULIAN-DAYS
                                           - AR137WS-LDAT-FIN.
           IF (AR137WS-DPF             NOT > ZEROES)
               GO TO 1115-NEXT-ARPAYMENT.

           IF (PRM-UPDATE-OPTION         = "U")
               IF (AR137WS-PRL           = "N")
                   IF (AR137WS-PRLSV     = SPACES)
                       MOVE APM-PROCESS-LEVEL TO AR137WS-PRLSV
                   ELSE
                       IF (AR137WS-PRLSV NOT = APM-PROCESS-LEVEL)
                           MOVE "Y"      TO AR137WS-PRL.

           MOVE "N"                       TO AR137WS-APPLD-EXIST
                                             AR137WS-APM-TOTAL-FIN-CHRG.

           MOVE APM-COMPANY                TO DB-CR-COMPANY.
           MOVE APM-CUSTOMER               TO DB-CR-CUSTOMER.
           MOVE APM-TRANS-TYPE             TO DB-CR-TYPE.
           MOVE APM-TRANS-NBR              TO DB-CR-NBR.
           MOVE APM-PAYMENT-SEQ            TO DB-CR-PYMNT-SEQ.
           MOVE APM-BATCH-NBR              TO DB-CR-BATCH.
           MOVE ARASET3-CR-BATCH           TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-ARASET3.
           IF (ARAPPLIED-NOTFOUND)
               MOVE "Y"                    TO AR137WS-APM-TOTAL-FIN-CHRG
               MOVE APM-TRAN-AMT           TO AR137WS-OPEN-AMT
               PERFORM 1120-CALC-APM-OPEN-AMT
               THRU    1120-END
           ELSE
               INITIALIZE                     I2 
                                              AR137WS-FIRST-APPLD
                                              AR137WS-TABLE-CREATED
                                              AR137WS-HOLD-TRANS-TYPE
                                              AR137WS-HOLD-INVOICE
                                              AR137WS-HOLD-PAYMENT-SEQ
                                              AR137WS-HOLD-COMPANY
                                              AR137WS-HOLD-BATCH-NBR
                                              AR137WS-HOLD-APP-SEQ
                                              AR137WS-NO-RECS
               MOVE 1                      TO I3
                                              AR137WS-TABLE-SORT
               MOVE 0                      TO AR137WS-TABLE-CONTROL
               PERFORM 1119-CREATE-APM-FC-TABLE
               THRU    1119-END
                   UNTIL (ARAPPLIED-NOTFOUND)
           END-IF.

           IF  (AR137WS-APM-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-TABLE-CREATED       NOT = "Y")
           AND (AR137WS-NO-RECS                 = "N")
               ADD 1                        TO I2   
               MOVE APM-GL-DATE             TO AR137WS-FC-FROM-DATE (I2)
               MOVE PRM-AS-OF-DATE          TO AR137WS-FC-TO-DATE (I2)
               MOVE AR137WS-OPEN-AMT        TO AR137WS-FC-OPEN-AMT (I2)
               MOVE AR137WS-HOLD-TRANS-TYPE TO
                                              AR137WS-FC-TRANS-TYPE (I2)
               MOVE AR137WS-HOLD-INVOICE    TO AR137WS-FC-INVOICE (I2)
               MOVE AR137WS-HOLD-PAYMENT-SEQ TO
                                             AR137WS-FC-PAYMENT-SEQ (I2)
               MOVE AR137WS-HOLD-COMPANY    TO AR137WS-FC-COMPANY (I2)
               MOVE AR137WS-HOLD-BATCH-NBR  TO AR137WS-FC-BATCH-NBR (I2)
               MOVE AR137WS-HOLD-APP-SEQ    TO AR137WS-FC-APP-SEQ (I2)
               MOVE "Y"                     TO AR137WS-FC-LAST-REC (I2)
               MOVE I2                      TO AR137WS-MAX-FC-TABLE
           END-IF.

           IF  (AR137WS-APM-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-TABLE-CREATED           = "Y")
               IF (AR137WS-OPEN-AMT         NOT = ZEROES)
                   MOVE PRM-AS-OF-DATE      TO AR137WS-FC-TO-DATE (I2)
                   MOVE "Y"                 TO AR137WS-FC-LAST-REC (I2)
               ELSE
                   MOVE "Y"                 TO AR137WS-FC-LAST-REC (I2)
               END-IF
               MOVE I2                      TO AR137WS-MAX-FC-TABLE
           END-IF.

           IF (AR137WS-MAX-FC-TABLE         > 1)
               MOVE 1                       TO I2
               MOVE 2                       TO I3
               PERFORM 1118-SORT-APM-FC-DATA
               THRU    1118-END
                   UNTIL (AR137WS-SORT).

           IF  (AR137WS-APM-TOTAL-FIN-CHRG  NOT = "Y")
           AND (AR137WS-NO-RECS                 = "N")
               PERFORM 1120-CALC-APM-OPEN-AMT
               THRU    1120-END
                   VARYING I2 FROM 1 BY 1 UNTIL
                                            (I2 > AR137WS-MAX-FC-TABLE).

       1115-NEXT-ARPAYMENT.    

           IF (PRM-FUTURE-APPS     = "N")
               PERFORM 860-FIND-NXTRNG-APMSET6
           ELSE
               PERFORM 860-FIND-NXTRNG-APMSET8.

       1115-END.

      ******************************************************************
       1118-SORT-APM-FC-DATA.
      ******************************************************************

           PERFORM
           UNTIL (I3 > AR137WS-MAX-FC-TABLE)
               IF (AR137WS-FC-FROM-DATE (I3)
                                            < AR137WS-FC-FROM-DATE (I2))
                   MOVE AR137WS-FC-CHRG (I2) TO AR137WS-FC-CHRG-REC
                   MOVE AR137WS-FC-CHRG (I3) TO AR137WS-FC-CHRG (I2)
                   MOVE AR137WS-FC-CHRG-REC  TO AR137WS-FC-CHRG (I3)
                   MOVE 1                    TO AR137WS-TABLE-CONTROL
                   ADD 1                     TO I2
                                                I3
               ELSE
                   ADD 1                     TO I2
                                                I3
               END-IF
           END-PERFORM.

           IF (AR137WS-TABLE-CONTROL         = "0")
               MOVE 0                        TO AR137WS-TABLE-SORT
           ELSE
               INITIALIZE                       AR137WS-TABLE-CONTROL
               MOVE 1                        TO I2
               MOVE 2                        TO I3
               MOVE 1                        TO AR137WS-TABLE-SORT
           END-IF.

       1118-END.

      ******************************************************************
       1119-CREATE-APM-FC-TABLE.
      ******************************************************************

           MOVE "N"                        TO AR137WS-NO-RECS. 

           IF (APM-LAST-FC-DATE            = ZEROES)
               IF (ARA-GL-DATE            <= APM-GL-DATE)
                   IF (AR137WS-APPLD-EXIST NOT = "Y")
                       IF (ARA-TRANS-TYPE = "C")
                           COMPUTE AR137WS-OPEN-AMT = APM-TRAN-AMT
                                                    - APM-ADJ-AMT
                                                    + ARA-CR-APP-AMT
                       ELSE
                           COMPUTE AR137WS-OPEN-AMT = APM-TRAN-AMT
                                                    - APM-ADJ-AMT
                                                    - ARA-CR-APP-AMT
                       END-IF
                       MOVE "Y"            TO AR137WS-APPLD-EXIST
                   ELSE
                       IF (ARA-TRANS-TYPE = "C")
                           COMPUTE AR137WS-OPEN-AMT = AR137WS-OPEN-AMT
                                                    + ARA-CR-APP-AMT
                       ELSE
                           COMPUTE AR137WS-OPEN-AMT = AR137WS-OPEN-AMT
                                                    - ARA-CR-APP-AMT
                       END-IF
                   END-IF
               MOVE ARA-CR-TYPE            TO AR137WS-HOLD-TRANS-TYPE
               MOVE ARA-CR-NBR             TO AR137WS-HOLD-INVOICE
               MOVE ARA-CR-PYMNT-SEQ       TO AR137WS-HOLD-PAYMENT-SEQ
               MOVE ARA-CR-COMPANY         TO AR137WS-HOLD-COMPANY
               MOVE ARA-CR-BATCH           TO AR137WS-HOLD-BATCH-NBR
               MOVE ARA-CR-APP-SEQ         TO AR137WS-HOLD-APP-SEQ
               IF (AR137WS-OPEN-AMT        = ZEROES)
                   MOVE "Y"                TO AR137WS-NO-RECS
               END-IF
               GO TO 1119-NEXT
               END-IF
           ELSE
               IF (ARA-DEPOSIT-DATE        <= APM-LAST-FC-DATE)
                   IF (AR137WS-APPLD-EXIST NOT = "Y")
                       IF (ARA-TRANS-TYPE = "C")
                           COMPUTE AR137WS-OPEN-AMT = APM-TRAN-AMT
                                                    - APM-ADJ-AMT
                                                    + ARA-CR-APP-AMT
                       ELSE
                           COMPUTE AR137WS-OPEN-AMT = APM-TRAN-AMT
                                                    - APM-ADJ-AMT
                                                    - ARA-CR-APP-AMT
                       END-IF
                       MOVE "Y"            TO AR137WS-APPLD-EXIST
                   ELSE
                       IF (ARA-TRANS-TYPE = "C")
                           COMPUTE AR137WS-OPEN-AMT = AR137WS-OPEN-AMT
                                                    + ARA-CR-APP-AMT
                       ELSE
                           COMPUTE AR137WS-OPEN-AMT = AR137WS-OPEN-AMT
                                                    - ARA-CR-APP-AMT
                       END-IF
                   END-IF
               MOVE ARA-CR-TYPE            TO AR137WS-HOLD-TRANS-TYPE
               MOVE ARA-CR-NBR             TO AR137WS-HOLD-INVOICE
               MOVE ARA-CR-PYMNT-SEQ       TO AR137WS-HOLD-PAYMENT-SEQ
               MOVE ARA-CR-COMPANY         TO AR137WS-HOLD-COMPANY
               MOVE ARA-CR-BATCH           TO AR137WS-HOLD-BATCH-NBR
               MOVE ARA-CR-APP-SEQ         TO AR137WS-HOLD-APP-SEQ
               IF (AR137WS-OPEN-AMT        = ZEROES)
                   MOVE "Y"                TO AR137WS-NO-RECS
               END-IF
               GO TO 1119-NEXT
               END-IF
           END-IF.

           IF (ARA-DEPOSIT-DATE            > PRM-AS-OF-DATE)
           OR (ARA-GL-DATE                 > PRM-AS-OF-DATE)
               IF (AR137WS-APPLD-EXIST     = "N")
                   MOVE APM-TRAN-AMT       TO AR137WS-OPEN-AMT
               END-IF
               MOVE ARA-CR-TYPE            TO AR137WS-HOLD-TRANS-TYPE
               MOVE ARA-CR-NBR             TO AR137WS-HOLD-INVOICE
               MOVE ARA-CR-PYMNT-SEQ       TO AR137WS-HOLD-PAYMENT-SEQ
               MOVE ARA-CR-COMPANY         TO AR137WS-HOLD-COMPANY
               MOVE ARA-CR-BATCH           TO AR137WS-HOLD-BATCH-NBR
               MOVE ARA-CR-APP-SEQ         TO AR137WS-HOLD-APP-SEQ
               GO TO 1119-NEXT
           END-IF.

           IF (AR137WS-TABLE-CREATED       = "Y")
               MOVE ARA-GL-DATE            TO AR137WS-FC-TO-DATE (I2)
           END-IF.

           IF (AR137WS-APPLD-EXIST     NOT = "Y")
               IF (ARA-TRANS-TYPE = "C")
                   COMPUTE AR137WS-OPEN-AMT    = APM-TRAN-AMT
                                               - APM-ADJ-AMT
                                               + ARA-CR-APP-AMT
               ELSE
                   COMPUTE AR137WS-OPEN-AMT    = APM-TRAN-AMT
                                               - APM-ADJ-AMT
                                               - ARA-CR-APP-AMT
               END-IF
               IF (AR137WS-OPEN-AMT    NOT = ZEROES)
                   MOVE "Y"                TO AR137WS-FIRST-APPLD
               END-IF
           ELSE
               IF (ARA-TRANS-TYPE = "C")
                   COMPUTE AR137WS-OPEN-AMT    = AR137WS-OPEN-AMT
                                               + ARA-CR-APP-AMT
               ELSE
                   COMPUTE AR137WS-OPEN-AMT    = AR137WS-OPEN-AMT
                                               - ARA-CR-APP-AMT
               END-IF
               MOVE "N"                    TO AR137WS-FIRST-APPLD
           END-IF.

           IF  (AR137WS-OPEN-AMT           = ZEROES)
               MOVE "Y"                    TO AR137WS-NO-RECS
               GO TO 1119-NEXT
           END-IF.

           ADD 1                           TO I2.

           IF (APM-LAST-FC-DATE            = ZEROES)
               IF (AR137WS-APPLD-EXIST NOT = "Y")
                   IF (APM-GL-DATE     NOT = ARA-GL-DATE)
                       MOVE APM-GL-DATE    TO AR137WS-FC-FROM-DATE (I2)
                       MOVE ARA-GL-DATE    TO AR137WS-FC-TO-DATE (I2)
                       COMPUTE AR137WS-FC-OPEN-AMT (I2)
                                               = APM-TRAN-AMT
                                               - APM-ADJ-AMT
                   ELSE
                       MOVE APM-GL-DATE    TO AR137WS-FC-FROM-DATE (I2)
                       MOVE AR137WS-OPEN-AMT TO AR137WS-FC-OPEN-AMT (I2)
                       MOVE "N"            TO AR137WS-FIRST-APPLD
                   END-IF
               ELSE
                   MOVE ARA-GL-DATE        TO AR137WS-FC-FROM-DATE (I2)
                   MOVE AR137WS-OPEN-AMT   TO AR137WS-FC-OPEN-AMT (I2)
               END-IF
           ELSE
               IF (AR137WS-APPLD-EXIST NOT = "Y")
                   MOVE APM-LAST-FC-DATE   TO AR137WS-FC-FROM-DATE (I2)
                   MOVE ARA-GL-DATE        TO AR137WS-FC-TO-DATE (I2)
                   COMPUTE AR137WS-FC-OPEN-AMT (I2)
                                           = APM-TRAN-AMT
                                           - APM-ADJ-AMT
               ELSE
                   MOVE ARA-GL-DATE        TO AR137WS-FC-FROM-DATE (I2)
                   MOVE AR137WS-OPEN-AMT   TO AR137WS-FC-OPEN-AMT (I2)
               END-IF
           END-IF.

           MOVE ARA-CR-TYPE              TO AR137WS-FC-TRANS-TYPE (I2).
           MOVE ARA-CR-NBR               TO AR137WS-FC-INVOICE (I2).
           MOVE ARA-CR-PYMNT-SEQ         TO AR137WS-FC-PAYMENT-SEQ (I2).
           MOVE ARA-CR-COMPANY           TO AR137WS-FC-COMPANY (I2).
           MOVE ARA-CR-BATCH             TO AR137WS-FC-BATCH-NBR (I2).
           MOVE ARA-CR-APP-SEQ           TO AR137WS-FC-APP-SEQ (I2).
           MOVE "N"                      TO AR137WS-FC-LAST-REC (I2).
           MOVE "Y"                      TO AR137WS-APPLD-EXIST.

           IF (AR137WS-FIRST-APPLD       = "Y")
               ADD 1                     TO I2
               MOVE AR137WS-OPEN-AMT     TO AR137WS-FC-OPEN-AMT (I2)
               MOVE ARA-CR-TYPE          TO AR137WS-FC-TRANS-TYPE (I2)
               MOVE ARA-CR-NBR           TO AR137WS-FC-INVOICE (I2)
               MOVE ARA-CR-PYMNT-SEQ     TO AR137WS-FC-PAYMENT-SEQ (I2)
               MOVE ARA-CR-COMPANY       TO AR137WS-FC-COMPANY (I2)
               MOVE ARA-CR-BATCH         TO AR137WS-FC-BATCH-NBR (I2)
               MOVE ARA-CR-APP-SEQ       TO AR137WS-FC-APP-SEQ (I2)
               MOVE "N"                  TO AR137WS-FC-LAST-REC (I2)
           END-IF.

           MOVE ARA-GL-DATE              TO AR137WS-FC-FROM-DATE (I2).
           MOVE "Y"                      TO AR137WS-TABLE-CREATED.

       1119-NEXT.

           PERFORM 860-FIND-NXTRNG-ARASET3.

       1119-END.

      ******************************************************************
       1120-CALC-APM-OPEN-AMT.
      ******************************************************************

           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")
               INITIALIZE                      WSDR-FR-DATE
                                               AR137WS-LAPAM
               MOVE PRM-AS-OF-DATE          TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPF          = WSDR-JULIAN-DAYS
                                            - AR137WS-LDAT-FIN
               IF (APM-LAST-FC-DATE         = ZEROES)
                   ADD 1                    TO AR137WS-DPF
               END-IF
           ELSE
               INITIALIZE                      WSDR-FR-DATE
                                               AR137WS-LAPAM
               MOVE AR137WS-FC-TO-DATE (I2) TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPF          = WSDR-JULIAN-DAYS
                                            - AR137WS-LDAT-FIN
               IF (APM-LAST-FC-DATE         = ZEROES)
                   ADD 1                    TO AR137WS-DPF
               END-IF
           END-IF.

           MOVE "N"                         TO AR137WS-RATE-ERROR.

           PERFORM 1160-CALC-APM-CHRG-BY-DATE
           THRU    1160-END.

           IF (AR137WS-RATE-ERROR           = "Y")
               GO TO 1120-END.

           PERFORM 1123-LOAD-APM-WORK
           THRU    1123-END.

       1120-END.

      ******************************************************************
       1123-LOAD-APM-WORK.
      ******************************************************************

           MOVE APM-TRAN-AMT                TO AR137WS-LAPAM.
           INITIALIZE                          WF-LATE-AMOUNT.

           PERFORM 1124-CREATE-APM-WORK-ITEMS
           THRU    1124-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > AR137WS-NBR-CHRG).

       1123-END.
       
      ******************************************************************
       1124-CREATE-APM-WORK-ITEMS.
      ******************************************************************

           MOVE "D"                         TO WF-TYPE.

           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")
               MOVE AR137WS-OPEN-AMT        TO WF-OPEN-AMOUNT
           ELSE
               MOVE AR137WS-FC-OPEN-AMT (I2) TO WF-OPEN-AMOUNT
           END-IF.

           MOVE AR137WS-INV-PAY-DATE (I1)   TO WF-DEPOSIT-DATE.
           MOVE AR137WS-INV-DUE-DATE (I1)   TO WF-DUE-DATE.

           IF (APM-LAST-FC-DATE             = ZEROES)
               MOVE APH-DEPOSIT-DATE        TO WF-ARO-DUE-DATE
           ELSE
               MOVE APM-LAST-FC-DATE        TO WF-ARO-DUE-DATE
           END-IF.

           ADD 1                            TO AR137WS-SEQ-NBR.
           IF (AR137WS-SEQ-NBR  = ZEROES)
               ADD 1                        TO AR137WS-SEQ-NBR
           END-IF.
           MOVE AR137WS-SEQ-NBR             TO WF-SEQ-NBR.
           MOVE APM-TRANS-TYPE              TO WF-TRANS-TYPE.
           MOVE APM-TRANS-NBR               TO WF-TRANS-NBR.
           MOVE AR137WS-ARO-CUSTOMER        TO WF-CUSTOMER.
           MOVE APM-PROCESS-LEVEL           TO WF-PROCESS-LEVEL.
           MOVE AR137WS-RATE-BY-DATE (I1)   TO WF-CHRG-RATE.
           MOVE AR137WS-NO-OF-DAYS (I1)     TO WF-NO-OF-DAYS.

           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")
               MOVE PRM-AS-OF-DATE          TO WSDR-FR-DATE
           ELSE
               MOVE AR137WS-FC-TO-DATE (I2) TO WSDR-FR-DATE
           END-IF.

           PERFORM 900-DATE-TO-JULIAN.

           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")
               COMPUTE AR137WS-DPF          = WSDR-JULIAN-DAYS
                                            - AR137WS-LDAT-FIN
               IF (APM-LAST-FC-DATE         = ZEROES)
                   ADD 1                    TO AR137WS-DPF
               END-IF
           ELSE
               MOVE WSDR-JULIAN-DAYS        TO AR137WS-FC-HOLD-DAYS
               IF (I2 = 1)
                   MOVE AR137WS-DUE-DATE-PLUS     TO WSDR-FR-DATE
               ELSE
                   MOVE AR137WS-FC-FROM-DATE (I2) TO WSDR-FR-DATE
               END-IF
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE AR137WS-DPF          = AR137WS-FC-HOLD-DAYS
                                            - WSDR-JULIAN-DAYS
               IF (AR137WS-FC-LAST-REC (I2) = "Y")
                   ADD 1                    TO AR137WS-DPF
               END-IF
           END-IF.

           MOVE AR137WS-DPF             TO WF-DPF.

           IF (I1 = 1)
               MOVE AR137WS-DPF         TO WF-DPF-EFF
           ELSE
               MOVE 0                   TO WF-DPF-EFF
           END-IF.

           INITIALIZE                          WF-DPL
                                               WF-DPL-EFF.
           MOVE APM-GL-DATE                 TO WF-TRANS-DATE.
           COMPUTE WF-FINANCE-AMOUNT        = AR137WS-AMT-BY-DATE (I1)
                                            * NEGATIVE-ONE.
           MOVE AR137WS-MEMO-FINCHG-REC-FLG TO WF-MEMO-CHG-FL.

           IF (AR137WS-MEMO-FINCHG-REC-FLG  = SPACES)
               COMPUTE AR137WS-AMT-BY-DATE (I1) =
                                            AR137WS-AMT-BY-DATE (I1)
                                            * NEGATIVE-ONE
               ADD AR137WS-AMT-BY-DATE (I1) TO AR137WS-FNET
           END-IF.

           IF (ACO-CURRENCY-CD          NOT = ACM-CURRENCY-CD)
               MOVE APM-COMPANY             TO IFCAWS-COMPANY
               MOVE ACO-CURRENCY-CD         TO IFCAWS-FR-CURR-CODE
               MOVE ACM-CURRENCY-CD         TO IFCAWS-TO-CURR-CODE
               MOVE "AR"                    TO IFCAWS-SYSTEM
               MOVE PRM-AS-OF-DATE          TO IFCAWS-EFFECT-DATE
               MOVE ZEROES                  TO IFCAWS-BASE-AMOUNT
               MOVE WF-FINANCE-AMOUNT       TO IFCAWS-TRAN-AMOUNT
               MOVE AR137WS-CURR-RATE       TO IFCAWS-BASERATE
               MOVE AR137WS-CURR-MUDV       TO IFCAWS-MULT-DIV
               MOVE IFCCWS-CURRENCY-ND      TO IFCAWS-BASE-ND
               MOVE ACO-BASE-ND             TO IFCAWS-TRAN-ND
               PERFORM 690-CALCULATE-AMOUNT-60
               MOVE IFCAWS-BASE-AMOUNT      TO WF-ORIG-AMT
               MOVE AR137WS-CURR-RATE       TO WF-ORIG-RATE
               MOVE AR137WS-CURR-MUDV       TO WF-CURR-MUDV
               MOVE IFCCWS-CURRENCY-ND      TO WF-ORIG-ND
               MOVE ACM-CURRENCY-CD         TO WF-ORIG-CURRENCY
           ELSE
               MOVE ACO-CURRENCY-CD         TO WF-ORIG-CURRENCY
               MOVE 1                       TO WF-ORIG-RATE
               MOVE "M"                     TO WF-CURR-MUDV
               MOVE ACO-BASE-ND             TO WF-ORIG-ND
               MOVE WF-FINANCE-AMOUNT       TO WF-ORIG-AMT.

           ADD WF-ORIG-AMT                  TO AR137WS-ORIG-AMT.
           MOVE APM-PAYMENT-SEQ             TO WF-PAYMENT-SEQ.
           MOVE APM-BATCH-NBR               TO WF-BATCH-NBR.
           MOVE PRM-COMPANY                 TO WF-COMPANY.
           INITIALIZE                          WF-LATE-AMOUNT
                                               WF-MIN-CHG-FL.
           
342466     MOVE AR137WS-COUNTRY-CODE        TO WF-COUNTRY-CODE.
J66145     MOVE 1                           TO WF-PRINT-FL . 
           WRITE AR137W1-REC.

           MOVE "Y"                         TO AR137WS-CHRG-EXIST.

       1124-END.

      ******************************************************************
       1160-CALC-APM-CHRG-BY-DATE.
      ******************************************************************

           INITIALIZE                          AR137WS-NBR-CHRG
                                               AR137WS-NBR-OF-DAYS-ITEM.

           PERFORM 1161-INIT-APM-CHRG-TABLE
           THRU    1161-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > 730).

           IF (APM-LAST-FC-DATE             = ZEROES)
               MOVE APH-DEPOSIT-DATE        TO WSDR-FR-DATE
               MOVE WSDR-FR-DATE            TO AR137WS-DUE-DATE-PLUS
           ELSE
               MOVE APM-LAST-FC-DATE        TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               COMPUTE WSDR-JULIAN-DAYS     = WSDR-JULIAN-DAYS + 1
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE            TO AR137WS-DUE-DATE-PLUS
           END-IF.

      **** GET EARLIEST FINANCE RATE IN EFFECT AS OF APM-GL-DATE
      **** OR APM-LAST-FC-DATE

           MOVE ACM-FIN-CHRG-CD             TO DB-FIN-CHRG-CD.
           MOVE AR137WS-DUE-DATE-PLUS       TO DB-EFF-DATE.
           PERFORM 850-FIND-NLT-AFISET2.
           IF (ARFINRATE-NOTFOUND)
           OR (AFI-FIN-CHRG-CD              NOT = ACM-FIN-CHRG-CD)
               MOVE ACM-CUSTOMER            TO D3-ERR-CUSTOMER
               MOVE ACM-FIN-CHRG-CD         TO D3-FIELD-NAME
               MOVE 205                     TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE             TO D3-FIELD-DATA-ERROR
               MOVE D31-MESSAGE-ERRORS      TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               ADD  1                       TO AR137WS-ERROR-CNT
               MOVE "Y"                     TO AR137WS-RATE-ERROR
               GO TO 1160-END
           END-IF.

      **** CREATE FIRST ROW IN TABLE

           MOVE 1                           TO AR137WS-NBR-CHRG.
           MOVE AFI-CHRG-RATE               TO AR137WS-RATE-BY-DATE
                                                  (AR137WS-NBR-CHRG).
           MOVE AR137WS-DUE-DATE-PLUS       TO AR137WS-ST-DATE
                                                  (AR137WS-NBR-CHRG)
                                               AR137WS-INV-DUE-DATE
                                                  (AR137WS-NBR-CHRG)
                                               WSDR-FR-DATE.
           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")
               MOVE PRM-AS-OF-DATE          TO WSDR-TO-DATE
                                               AR137WS-INV-PAY-DATE
                                                  (AR137WS-NBR-CHRG)
           ELSE
               MOVE AR137WS-FC-TO-DATE (I2) TO WSDR-TO-DATE
                                               AR137WS-INV-PAY-DATE
                                                  (AR137WS-NBR-CHRG)
           END-IF.
           PERFORM 900-NBR-DAYS-IN-DATE-RNG.
           MOVE WSDR-NBR-DAYS               TO AR137WS-NO-OF-DAYS
                                                  (AR137WS-NBR-CHRG).
           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")                    
               ADD 1                        TO AR137WS-NO-OF-DAYS
                                                  (AR137WS-NBR-CHRG).

      **** CREATE MORE LINES IN TABLE IF NEEDED

           MOVE AFI-FIN-CHRG-CD             TO DB-FIN-CHRG-CD.
           MOVE AFI-EFF-DATE                TO DB-EFF-DATE.
           PERFORM 850-FIND-NLT-AFISET1.
           PERFORM 860-FIND-NEXT-AFISET1.
           IF (ARFINRATE-FOUND)
               PERFORM 1162-MORE-APM-RATE-CHARGES
               THRU    1162-END
                   UNTIL (ARFINRATE-NOTFOUND)
                   OR    (AFI-FIN-CHRG-CD   NOT = ACM-FIN-CHRG-CD)
                   OR    (AFI-EFF-DATE          > PRM-AS-OF-DATE)
           END-IF.

      **** CALCULATE NUMBER OF DAYS PER TABLE ROW IF NUMBER OF ROWS > 1

           PERFORM 1164-APM-NBR-OF-DAYS
           THRU    1164-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > AR137WS-NBR-CHRG).

      **** COMPLETE TABLE BY CALCULATING ROW AMOUNT     

           PERFORM 1166-APM-RATE-AMOUNT
           THRU    1166-END
               VARYING I1 FROM 1 BY 1 UNTIL (I1 > AR137WS-NBR-CHRG).

           MOVE SPACES                   TO AR137WS-MEMO-FINCHG-REC-FLG.

           IF (ACM-I-FIN-MIN-CHRG        > ZERO)
               INITIALIZE                  AR137WS-CALC-INVOICE-LATE-CHG
               PERFORM
                   VARYING I1 FROM 1 BY 1
                   UNTIL  (I1 > AR137WS-NBR-CHRG)
                       COMPUTE AR137WS-CALC-INVOICE-LATE-CHG =
                               AR137WS-CALC-INVOICE-LATE-CHG +
                               AR137WS-AMT-BY-DATE (I1)
               END-PERFORM
               IF (AR137WS-CALC-INVOICE-LATE-CHG < ACM-I-FIN-MIN-CHRG)
                   MOVE "*"              TO AR137WS-MEMO-FINCHG-REC-FLG
               END-IF
           END-IF.

       1160-END.

      ******************************************************************
       1161-INIT-APM-CHRG-TABLE.
      ******************************************************************

           MOVE ZEROES                      TO AR137WS-ST-DATE     (I1)
                                               AR137WS-NO-OF-DAYS  (I1)
                                               AR137WS-RATE-BY-DATE(I1)
                                               AR137WS-AMT-BY-DATE (I1)
                                               AR137WS-INV-DUE-DATE(I1)
                                               AR137WS-INV-PAY-DATE(I1).

       1161-END.

      ******************************************************************
       1162-MORE-APM-RATE-CHARGES.
      ******************************************************************

           ADD 1                            TO AR137WS-NBR-CHRG.    
           MOVE AFI-CHRG-RATE               TO AR137WS-RATE-BY-DATE
                                                  (AR137WS-NBR-CHRG).
           MOVE AFI-EFF-DATE                TO AR137WS-ST-DATE
                                                  (AR137WS-NBR-CHRG).
           PERFORM 860-FIND-NEXT-AFISET1.

       1162-END.

      ******************************************************************
       1164-APM-NBR-OF-DAYS.
      ******************************************************************

           IF (AR137WS-APM-TOTAL-FIN-CHRG NOT = "Y")
               IF (I2 = 1)
                   MOVE AR137WS-ST-DATE (I1)  TO AR137WS-FR-DATE
               ELSE
                   IF (I1 > 1)
                       MOVE AR137WS-TO-DATE   TO AR137WS-FR-DATE
                   ELSE
                       MOVE AR137WS-FC-FROM-DATE (I2) TO AR137WS-FR-DATE
                   END-IF
               END-IF
           ELSE
               MOVE AR137WS-ST-DATE (I1)      TO AR137WS-FR-DATE
           END-IF.

           MOVE AR137WS-ST-DATE (I1)        TO AR137WS-INV-DUE-DATE(I1).

           IF (I1 < AR137WS-NBR-CHRG)
               MOVE AR137WS-ST-DATE (I1 + 1)  TO WSDR-FR-DATE
               PERFORM 900-DATE-TO-JULIAN
               IF (AR137WS-APM-TOTAL-FIN-CHRG = "Y")
                   COMPUTE WSDR-JULIAN-DAYS   = WSDR-JULIAN-DAYS
                                              - 1
               END-IF
               PERFORM 900-JULIAN-TO-DATE
               MOVE WSDR-FR-DATE              TO AR137WS-TO-DATE
                                                AR137WS-INV-PAY-DATE(I1)
           ELSE
               IF (AR137WS-APM-TOTAL-FIN-CHRG = "Y")
                   MOVE PRM-AS-OF-DATE        TO AR137WS-TO-DATE
                                                AR137WS-INV-PAY-DATE(I1)
               ELSE
                   MOVE AR137WS-FC-TO-DATE (I2) TO AR137WS-TO-DATE
                                                AR137WS-INV-PAY-DATE(I1)
               END-IF
           END-IF.

           MOVE AR137WS-FR-DATE               TO WSDR-FR-DATE.
           MOVE AR137WS-TO-DATE               TO WSDR-TO-DATE.
           PERFORM 900-NBR-DAYS-IN-DATE-RNG.

           IF (AR137WS-APM-TOTAL-FIN-CHRG     = "Y")
               COMPUTE AR137WS-NO-OF-DAYS(I1) = WSDR-NBR-DAYS
                                              + 2
           ELSE
               COMPUTE AR137WS-NO-OF-DAYS(I1) = WSDR-NBR-DAYS
                                              + 1
      *        MOVE WSDR-NBR-DAYS             TO AR137WS-NO-OF-DAYS (I1)
               IF  (AR137WS-FC-LAST-REC (I2)  = "Y")
               AND (I1 = AR137WS-NBR-CHRG)
                   ADD 1                      TO AR137WS-NO-OF-DAYS (I1)
               END-IF
           END-IF.

       1164-END.

      ******************************************************************
       1166-APM-RATE-AMOUNT.
      ******************************************************************

           IF (AR137WS-APM-TOTAL-FIN-CHRG   = "Y")
               COMPUTE AR137WS-FIN          = APM-TRAN-AMT
                                            * AR137WS-NO-OF-DAYS (I1)
               COMPUTE AR137WS-FIN ROUNDED  = (AR137WS-FIN * 12) / 365
           ELSE
               ADD AR137WS-FC-OPEN-AMT (I2) TO AR137WS-AMT-BY-DATE-CUST
               COMPUTE AR137WS-FIN          = AR137WS-FC-OPEN-AMT (I2)
                                            * AR137WS-NO-OF-DAYS (I1)
               COMPUTE AR137WS-FIN ROUNDED  = (AR137WS-FIN * 12) / 365
           END-IF.

           IF (ACO-CURRENCY-CD            NOT = ACM-CURRENCY-CD)
               MOVE IFCCWS-CURRENCY-ND    TO IFCAWS-TRAN-ND
           ELSE
               MOVE ACO-BASE-ND           TO IFCAWS-TRAN-ND.

           IF (IFCAWS-TRAN-ND      = 2)
               COMPUTE IFCAWS-BASE-AMT2 ROUNDED = AR137WS-FIN
                                             * AR137WS-RATE-BY-DATE (I1)
               MOVE IFCAWS-BASE-AMT2    TO AR137WS-AMT-BY-DATE (I1)
            ELSE
               COMPUTE IFCAWS-BASE-AMT0 ROUNDED = AR137WS-FIN
                                             * AR137WS-RATE-BY-DATE (I1)
               MOVE IFCAWS-BASE-AMT0    TO AR137WS-AMT-BY-DATE (I1).

           INITIALIZE IFCAWS-BASE-AMT2
                      IFCAWS-BASE-AMT0.

           ADD AR137WS-AMT-BY-DATE (I1)     TO AR137WS-CHRG-TOT-AMT.
           ADD AR137WS-NO-OF-DAYS (I1)      TO AR137WS-NBR-OF-DAYS-ITEM.
           ADD 1                            TO AR137WS-NBR-LINES-CUST.

       1166-END.

       1000-END.

      ******************************************************************
       2000-DO-REPORT                  SECTION 50.
      ******************************************************************
       2000-START.

      *     OPEN OUTPUT FINANCE-FILE.

           INITIALIZE WF-AR137-KEY
                      WFA-AR137A-KEY
                      WFB-AR137B-KEY.

           IF (WS-RESTART-PHASE   = 3)
               GO TO 2000-DO-ARDISTRIB.
                 
           IF (WS-RESTART-PHASE   = 4)
               GO TO 2000-DO-ARAPPLIED.
       
           IF (PROGRAM-RESTARTING = WS-FALSE)
               MOVE "Y"                    TO WS-RESTART-FIN-FLAG.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           PERFORM 840-FIND-ACOSET1.

           IF (PROGRAM-RESTARTING      = WS-FALSE)
               INITIALIZE   RPT-PAGE-COUNT (AR137-R2)
               MOVE ACO-COMPANY            TO G1N-ARO-COMPANY
               MOVE ACO-NAME               TO G1N-ACO-NAME
               MOVE ACO-CURRENCY-CD        TO G1N-ACO-CURRENCY-CD
               MOVE PRM-AS-OF-DATE         TO G1N-AS-OF-DATE
               MOVE GN1-ARO-COMPANY-NET    TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               MOVE ACO-COMPANY            TO G1-ARO-COMPANY
               MOVE ACO-NAME               TO G1-ACO-NAME
               MOVE ACO-CURRENCY-CD        TO G1-ACO-CURRENCY-CD
               MOVE PRM-AS-OF-DATE         TO G1-AS-OF-DATE
               MOVE GN1-ARO-COMPANY        TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           MOVE ACO-CUST-GROUP             TO DB-CUST-GROUP.
           PERFORM 840-FIND-ACGSET1.

342466     IF (ACO-FIN-CHG-PRFX            = SPACES)
               MOVE "FC-"                  TO AR137WS-PREF
342466     ELSE
342466         MOVE ACO-FIN-CHG-PRFX       TO AR137WS-PREF.

           IF (PROGRAM-RESTARTING              = WS-TRUE)
               MOVE WS-RESTART-COMPANY         TO WF-COMPANY
               MOVE WS-RESTART-CUSTOMER        TO WF-CUSTOMER
               MOVE WS-RESTART-SEQ-NBR         TO WF-SEQ-NBR
               MOVE WS-RESTART-TRANS-TYPE      TO WF-TRANS-TYPE
               MOVE WS-RESTART-TRANS-NBR       TO WF-TRANS-NBR
               MOVE WS-RESTART-PAYMENT-SEQ     TO WF-PAYMENT-SEQ.

           MOVE WS-FALSE                   TO AR137WS-FILE.

           START AR137W1-FILE              KEY NOT < WF-AR137-KEY
             INVALID KEY
                 MOVE WS-TRUE              TO AR137WS-FILE.
               
           IF (AR137-FOUND)
               MOVE WS-FALSE               TO AR137WS-FILE
               READ AR137W1-FILE NEXT RECORD
                   AT END
                       MOVE WS-TRUE        TO AR137WS-FILE.

           IF (AR137-NOTFOUND)
               MOVE T1-NO-DATA             TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               MOVE T2-NO-DATA             TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
               GO TO 2000-END.

           IF (PRM-UPDATE-OPTION = "U")
               MOVE ACO-FIN-AUTO-NBR       TO AR137WS-FANB
               MOVE ACO-SUMM-BTCH-NBR      TO AR137WS-SUMB
           END-IF.
               
           IF (PRM-UPDATE-OPTION = "U")
               IF (ACO-CURRENCY-CD = ACG-CURRENCY-CD)
                   MOVE 1                  TO AR137WS-GROUP-RATE
                   MOVE "M"                TO AR137WS-GROUP-MUDV
               ELSE
                   MOVE PRM-COMPANY        TO IFCRWS-COMPANY
                   MOVE ACO-CURRENCY-CD    TO IFCRWS-FR-CURR-CODE
                   MOVE ACG-CURRENCY-CD    TO IFCRWS-TO-CURR-CODE
                   PERFORM 655-EDIT-CURRENCY-RELATION-60
                   MOVE PRM-COMPANY        TO IFGRWS-COMPANY
                   MOVE ACO-CURRENCY-CD    TO IFGRWS-FR-CURR-CODE
                   MOVE ACG-CURRENCY-CD    TO IFGRWS-TO-CURR-CODE
                   MOVE "AR"               TO IFGRWS-SYSTEM
                   MOVE PRM-AS-OF-DATE     TO IFGRWS-EFFECT-DATE
                   PERFORM 660-GET-CURRENCY-RATE-60
                   MOVE IFGRWS-SELL-RATE   TO AR137WS-GROUP-RATE
                   MOVE IFCRWS-MULT-DIV    TO AR137WS-GROUP-MUDV.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE PRM-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
           PERFORM 840-FIND-APVSET1.
           IF (ARPROCLEVL-FOUND)
               MOVE APV-AR-CODE            TO AR137WS-CODE.

           INITIALIZE AR137WS-FINC
                      AR137WS-DFINC
                      AR137WS-NFINC 
                      AR137WS-ODFINC
                      AR137WS-ONFINC.

      ****
      **** PRINT DETAIL AND NET REPORTS
      ****

           PERFORM 2010-DO-ARO-COMPANY
           THRU    2010-END
               UNTIL (AR137-NOTFOUND)
               OR    (WF-COMPANY           NOT = PRM-COMPANY)
               OR    ((WF-TYPE             NOT = "D")
               AND    (WF-TYPE             NOT = "N")).

      ****
      **** PRINT COMPANY TOTALS FOR DETAIL REPORT
      ****

           IF (AR137WS-DPRT1               NOT = SPACES)
               IF (AR137WS-DET-CURR1       NOT = SPACES)
                   MOVE AR137WS-DET-CURR1  TO T1D-CURRENCY-CD
                   MOVE AR137WS-DET-ND     TO T1D-ARO-ORIG-TOTAL-ND
                   MOVE AR137WS-ODFINC     TO T1D-ARO-ORIG-TOTAL
               ELSE
                   INITIALIZE                 T1D-CURRENCY-CD
                                              T1D-ARO-ORIG-TOTAL-ND
                                              T1D-ARO-ORIG-TOTAL
               END-IF
               MOVE ACO-BASE-ND            TO T1D-ARO-COMP-TOTAL-ND
               MOVE AR137WS-DFINC          TO T1D-ARO-COMP-TOTAL
               MOVE TND1-ARO-COMPANY       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
           END-IF.

      ****
      **** PRINT COMPANY TOTALS FOR NET REPORT
      ****

           IF (AR137WS-NPRT2               NOT = SPACES)
               IF (AR137WS-NET-CURR1       NOT = SPACES)
                   MOVE AR137WS-NET-CURR1  TO T1N-CURRENCY-CD
                   MOVE AR137WS-NET-ND     TO T1N-ARO-ORIG-TOTAL-ND
                   MOVE AR137WS-ONFINC     TO T1N-ARO-ORIG-TOTAL
               ELSE
                   INITIALIZE                 T1N-CURRENCY-CD
                                              T1N-ARO-ORIG-TOTAL-ND
                                              T1N-ARO-ORIG-TOTAL
               END-IF
               MOVE ACO-BASE-ND            TO T1N-ARO-COMP-TOTAL-ND
               MOVE AR137WS-NFINC          TO T1N-ARO-COMP-TOTAL
               MOVE TNN1-ARO-COMPANY       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP
           END-IF.

           PERFORM 840-MODIFY-CKPSET1.
           MOVE 3                      TO WS-RESTART-PHASE.
           MOVE AR137WS-FANB           TO WS-RESTART-FANB.
           MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
           PERFORM 820-STORE-CKPOINT.
           PERFORM 900-SAVE-OUTPUT-FILES.
           PERFORM 920-AUDIT-END.
           PERFORM 910-AUDIT-BEGIN.

       2000-DO-ARDISTRIB.

      ****
      **** CREATE ARDISTRIB (SUMMARY BY COMPANY)
      ****
           IF (PRM-UPDATE-OPTION = "U")
               MOVE PRM-COMPANY            TO DB-COMPANY
               PERFORM 840-MODIFY-ACOSET1
               MOVE AR137WS-FANB           TO ACO-FIN-AUTO-NBR
               MOVE AR137WS-SUMB           TO ACO-SUMM-BTCH-NBR
               PERFORM 820-STORE-ARCOMP
               INITIALIZE                     WFA-AR137A-KEY
                                              AR137WS-PGM
               MOVE WS-FALSE               TO AR137AWS-FILE

               START AR137W2-FILE  KEY NOT <  WFA-AR137A-KEY
                   INVALID KEY
                       MOVE WS-TRUE        TO AR137AWS-FILE
               END-START

               IF (AR137A-FOUND)
                   MOVE WS-FALSE           TO AR137AWS-FILE
                   READ AR137W2-FILE NEXT RECORD
                       AT END
                           MOVE WS-TRUE    TO AR137AWS-FILE.

           IF (PRM-UPDATE-OPTION = "U")
               PERFORM 2300-CREATE-ARDISTRIB
               THRU    2300-END
                   UNTIL (AR137A-NOTFOUND).

           PERFORM 840-MODIFY-CKPSET1.
           MOVE 4                      TO WS-RESTART-PHASE.
           MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO.
           PERFORM 820-STORE-CKPOINT.
           PERFORM 900-SAVE-OUTPUT-FILES.
           PERFORM 920-AUDIT-END.
           PERFORM 910-AUDIT-BEGIN.

       2000-DO-ARAPPLIED.

      ****
      **** UPDATE ARAPPLIED FOR FINANCE CHARGE DATE
      ****

           IF (PRM-UPDATE-OPTION = "U")
               INITIALIZE WFB-AR137B-KEY
               MOVE WS-FALSE               TO AR137BWS-FILE

               START AR137W3-FILE  KEY NOT <  WFB-AR137B-KEY
                   INVALID KEY
                       MOVE WS-TRUE        TO AR137BWS-FILE
               END-START

               IF (AR137B-FOUND)
                   READ AR137W3-FILE NEXT RECORD
                       AT END
                           MOVE WS-TRUE    TO AR137BWS-FILE.

           IF (PRM-UPDATE-OPTION = "U")
               PERFORM 2400-MODIFY-ARAPPLIED
               THRU    2400-END
                   UNTIL (AR137B-NOTFOUND).

      ****
      **** PRINT REPORT COMPLETE MESSAGE FOR DETAIL REPORT
      **** 

           MOVE T21-REPORT-MESSAGE  TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

      ****
      **** PRINT REPORT COMPLETE MESSAGE FOR NET REPORT
      **** 

           MOVE T12-REPORT-MESSAGE TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           GO TO 2000-END.

      ******************************************************************
       2010-DO-ARO-COMPANY.
      ******************************************************************

787215     MOVE WF-PRINT-FL            TO AR137WS-PRINT-FL.
787215
           MOVE WF-COMPANY             TO G1-ARO-COMPANY
                                          G1N-ARO-COMPANY.

           MOVE WF-COMPANY             TO DB-COMPANY.
           PERFORM 840-FIND-ACOSET1.
           MOVE ACO-NAME               TO G1-ACO-NAME
                                          G1N-ACO-NAME.
           MOVE ACO-CURRENCY-CD        TO G1-ACO-CURRENCY-CD
                                          G1N-ACO-CURRENCY-CD.

      **** PRINT REPORT HEADINGS AR137-R2 ****
           IF (WF-TYPE = "N")
               IF (AR137WS-NET-PAGE = ZEROES)
                   MOVE WF-ORIG-CURRENCY      TO AR137WS-NET-CURR
                                                 AR137WS-NET-CURR1
                   MOVE WF-ORIG-ND            TO AR137WS-NET-ND
                   MOVE GN1-ARO-COMPANY-NET   TO RPT-GROUP-REQUEST
                   PERFORM 700-PRINT-RPT-GRP
                   MOVE 1                     TO AR137WS-NET-PAGE
               END-IF
               MOVE WF-TYPE                   TO AR137WS-TYPE
               PERFORM 2015-PRINT-NET
               THRU    2015-END
          ELSE
      **** PRINT REPORT HEADINGS AR137-R1 **** 
               IF (WF-TYPE = "D")
                   IF (AR137WS-DET-PAGE = ZEROES)
                       MOVE WF-ORIG-CURRENCY  TO AR137WS-DET-CURR
                                                 AR137WS-DET-CURR1
                       MOVE WF-ORIG-ND        TO AR137WS-DET-ND
                       MOVE GN1-ARO-COMPANY   TO RPT-GROUP-REQUEST
                       PERFORM 700-PRINT-RPT-GRP
                       MOVE 1                 TO AR137WS-DET-PAGE
                   END-IF
                   MOVE WF-TYPE               TO AR137WS-TYPE
                   PERFORM 2016-PRINT-DETAIL
                   THRU    2016-END
               END-IF
           END-IF.

       2010-END.

      ******************************************************************
       2015-PRINT-NET.
      ******************************************************************

      ****
      **** FINANCE CHARGE RECORD ONLY
      ****
474744     IF (AR137WS-PRINT-FL            = ZERO)
474744         GO TO 2015-NEXT-AR137.

           MOVE WF-COMPANY                 TO DB-COMPANY.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.
           PERFORM 840-FIND-ACMSET1.

           IF (ACM-CURRENCY-CD NOT = AR137WS-NET-CURR)
               INITIALIZE AR137WS-NET-CURR1.

           MOVE WF-CUSTOMER                TO R1G6-ARO-CUSTOMER
                                              AR137WS-ARO-CUSTOMER.

           MOVE ACO-CUST-GROUP             TO DB-CUST-GROUP.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.
           PERFORM 840-FIND-CUDSET1.
           MOVE CUD-NAME                   TO R1G6-CUD-NAME.
           MOVE ACO-BASE-ND                TO R1G6-NET-DPD-AMT-ND
                                              R1G6-FIN-AMT-ND.
           MOVE WF-OPEN-AMOUNT             TO R1G6-NET-DPD-AMT.
           MOVE WF-FINANCE-AMOUNT          TO R1G6-FIN-AMT.
           MOVE ACM-CONTACT                TO R1G6-CONTACT.
           MOVE ACM-FIN-CHRG-CD            TO R1G6-FIN-GL-CODE.

           COMPUTE R1G6-CHRG-RATE          = WF-CHRG-RATE
                                           * 100. 

342466     IF (WF-COUNTRY-CODE             = "SE")
               COMPUTE R1G6-CHRG-RATE      = WF-CHRG-RATE
                                           * 12.

           MOVE WF-MIN-CHG-FL              TO R1G6-MIN.

           IF (WF-ORIG-CURRENCY NOT = ACO-CURRENCY-CD)
               MOVE "*"                    TO R1G6-CURRENCY-FL
           ELSE
               MOVE SPACES                 TO R1G6-CURRENCY-FL.

      ****
      **** PRINT REPORT DETAIL LINES AR137-R2 ****
      ****
           MOVE R1GN6-ARO-INVOICE          TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           IF (AR137WS-NPRT2 = SPACES)
               MOVE "Y"                    TO AR137WS-NPRT2.

           ADD  WF-FINANCE-AMOUNT          TO AR137WS-NFINC.
           ADD  WF-ORIG-AMT                TO AR137WS-ONFINC.
           MOVE WF-FINANCE-AMOUNT          TO AR137WS-CFINC.
           MOVE WF-ORIG-AMT                TO AR137WS-OFINC.

           MOVE WF-PROCESS-LEVEL           TO AR137WS-PROC-LEVEL.
           MOVE WF-ORIG-CURRENCY           TO AR137WS-ORIG-CURRENCY.
           MOVE WF-ORIG-RATE               TO AR137WS-ORIG-RATE.
           MOVE WF-CURR-MUDV               TO AR137WS-CURR-MUDV.
           MOVE WF-ORIG-ND                 TO AR137WS-ORIG-ND.

           IF  (PRM-UPDATE-OPTION          = "U")
           AND (WF-FINANCE-AMOUNT      NOT = ZEROS)
                PERFORM 2200-CREATE-AROITEMS
                THRU    2200-END
                IF (ACM-FIN-DOC-PRNT       = "Y")
                    MOVE 1                 TO AR137WS-SEQN
                    MOVE WF-OPEN-AMOUNT    TO AR137WS-OPEN
                    PERFORM 2065-CREATE-NOTICEWRK
                    THRU    2065-END
                END-IF
                MOVE PRM-COMPANY           TO DB-COMPANY
                MOVE ACM-CUSTOMER          TO DB-CUSTOMER
                MOVE WF-FINANCE-AMOUNT     TO ARWS-TRAN-AMT
                MOVE AR137WS-GROUP-RATE    TO ARWS-GROUP-RATE
                MOVE AR137WS-GROUP-MUDV    TO ARWS-GROUP-MUDV
                INITIALIZE                    ARWS-TRANS-TYPE
                INITIALIZE                    ARWS-GL-DATE
                MOVE "Y"                   TO ARWS-SKIP-MX-UPDATE
                PERFORM 700-UPDATE-ARCUSTOMER.

           IF (ACO-DTL-FIN-CHRG        = "Y")
               IF (PRM-UPDATE-OPTION       = "U")
                   PERFORM 2210-CREATE-DETAIL-ARDISTRIBS
                   THRU    2210-END.

           ADD 1                            TO AR137WS-REC-COUNTER.
           IF  (PRM-UPDATE-OPTION           = "U")
           AND (AR137WS-REC-COUNTER         > WS-MAX-OPS-IN-TRAN)
               PERFORM 840-MODIFY-CKPSET1
               MOVE WF-COMPANY             TO WS-RESTART-COMPANY
               MOVE WF-CUSTOMER            TO WS-RESTART-CUSTOMER
               MOVE WF-SEQ-NBR             TO WS-RESTART-SEQ-NBR
               MOVE WF-TRANS-TYPE          TO WS-RESTART-TRANS-TYPE
               MOVE WF-TRANS-NBR           TO WS-RESTART-TRANS-NBR
               MOVE WF-PAYMENT-SEQ         TO WS-RESTART-PAYMENT-SEQ
               MOVE WS-RESTART-INFO         TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-OUTPUT-FILES
               PERFORM 920-AUDIT-END
               PERFORM 910-AUDIT-BEGIN
               INITIALIZE AR137WS-REC-COUNTER.

       2015-NEXT-AR137.

           READ AR137W1-FILE NEXT RECORD
                AT END
                   MOVE WS-TRUE            TO AR137WS-FILE.

       2015-END.

      ******************************************************************
       2016-PRINT-DETAIL.
      ******************************************************************

474744     IF (WF-PRINT-FL                  = ZERO)
474744         MOVE WF-CUSTOMER             TO AR137WS-PRT-CUSTOMER
474744         PERFORM
474744             UNTIL (AR137-NOTFOUND)
474744             OR    (WF-COMPANY  NOT = PRM-COMPANY)
474744             OR    (WF-CUSTOMER NOT = AR137WS-PRT-CUSTOMER)
474744                 READ AR137W1-FILE NEXT RECORD
474744                   AT END
474744                  MOVE WS-TRUE            TO AR137WS-FILE
474744         END-PERFORM
474744         GO TO 2016-END.

           MOVE WF-COMPANY                 TO DB-COMPANY.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.
           PERFORM 840-FIND-ACMSET1.

           IF (ACM-CURRENCY-CD NOT = AR137WS-DET-CURR)
               INITIALIZE AR137WS-DET-CURR1.

           IF (PRM-UPDATE-OPTION = "U")
               IF (ACM-FIN-DOC-PRNT = "Y")
                   INITIALIZE AR137WS-SEQN.

           MOVE WF-CUSTOMER                TO G2-ARO-CUSTOMER
                                              T21-ARO-CUSTOMER
                                              AR137WS-ARO-CUSTOMER.
           MOVE ACM-FIN-CHRG-CD            TO G2-ACM-FIN-CHRG-CD.

           MOVE ACM-CONTACT                TO G2-ACM-CONTACT.
           MOVE ACM-PHONE-NMBR             TO G2-ACM-PHONE-NMBR.
           MOVE ACM-PHONE-EXT              TO G2-ACM-PHONE-EXT.

           MOVE ACO-CUST-GROUP             TO DB-CUST-GROUP.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.
           PERFORM 840-FIND-CUDSET1.
           MOVE CUD-NAME                   TO G2-CUD-NAME.

           IF (AR137WS-DPRT1               = SPACES)
               MOVE "Y"                    TO AR137WS-DPRT1.

      ****
      **** PRINT DETAIL CUSTOMER REPORT HEADINGS FOR AR137-R1 ****
      ****
           MOVE GN2-ARO-CUSTOMER           TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           INITIALIZE                         AR137WS-CFINC
                                              AR137WS-OFINC.

           MOVE AR137WS-PREF               TO AR137WS-PREF-NEXT.
           COMPUTE AR137WS-NBR-NEXT        =  AR137WS-FANB + 1.

           INITIALIZE                         AR137WS-FIN-CHARGE.
           INITIALIZE                         AR137WS-MIN-CHG-FL.
           INITIALIZE                         AR137WS-CUSTOMER.

           MOVE WS-TRUE                    TO AR137WS-NOTICE-FL.

           PERFORM 2060-DO-ARO-CUSTOMER
           THRU    2060-END
               UNTIL (AR137-NOTFOUND)
               OR    (WF-COMPANY  NOT = PRM-COMPANY)
               OR    (WF-CUSTOMER NOT = AR137WS-ARO-CUSTOMER)
               OR    (WF-TYPE     NOT = "D").

           INITIALIZE                         T21-MIN-MESSAGE.

      ****
      **** ERROR MESSAGE 101 = CUSTOMER MINIMUM CHARGE
      **** ERROR MESSAGE 102 = LESS THAN CUSTOMER MINIMUM 
      **** ERROR MESSAGE 103 - COMPANY IS SET TO ROUND
      **** ERROR MESSAGE 104 - NO CHARGE
      ****

258200     MOVE PRM-COMPANY                TO DB-COMPANY.
258200     MOVE AR137WS-CUSTOMER           TO DB-CUSTOMER.
258200     PERFORM 840-FIND-ACMSET1.

           IF (AR137WS-CFINC                   < ACM-FIN-MIN-CHRG)
               IF (ACM-MINIMUM                 = "Y")
                   IF  (AR137WS-CFINC          <= ZERO)
                       INITIALIZE                 AR137WS-CFINC
                       INITIALIZE                 AR137WS-OFINC
                       MOVE 104                TO CRT-MSG-NBR
                       PERFORM 790-GET-MSG
                       MOVE CRT-MESSAGE        TO T21-MIN-MESSAGE
                   ELSE
                       IF (ACO-CURRENCY-CD  NOT = ACM-CURRENCY-CD)
                           MOVE WF-COMPANY        TO IFCAWS-COMPANY
                           MOVE ACO-CURRENCY-CD   TO IFCAWS-FR-CURR-CODE
                           MOVE ACM-CURRENCY-CD   TO IFCAWS-TO-CURR-CODE
                           MOVE "AR"               TO IFCAWS-SYSTEM
                           MOVE PRM-AS-OF-DATE     TO IFCAWS-EFFECT-DATE
                           MOVE ZEROS              TO IFCAWS-BASE-AMOUNT
                           MOVE ACM-FIN-MIN-CHRG   TO IFCAWS-TRAN-AMOUNT
                           MOVE AR137WS-CURR-RATE  TO IFCAWS-BASERATE
                           MOVE AR137WS-CURR-MUDV  TO IFCAWS-MULT-DIV
                           MOVE ACO-BASE-ND        TO IFCAWS-TRAN-ND
                           MOVE AR137WS-ORIG-ND    TO IFCAWS-BASE-ND
                           PERFORM 690-CALCULATE-AMOUNT-60
                           MOVE IFCAWS-BASE-AMOUNT TO AR137WS-OFINC
                           MOVE ACM-FIN-MIN-CHRG   TO AR137WS-CFINC
                           MOVE 101                TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE        TO T21-MIN-MESSAGE
                       ELSE
                           MOVE ACM-FIN-MIN-CHRG   TO AR137WS-CFINC
                                                      AR137WS-OFINC
                           MOVE 101                TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE        TO T21-MIN-MESSAGE
                       END-IF
                   END-IF
               ELSE
                   IF (ACM-MINIMUM             = "N")
                       IF  (AR137WS-CFINC      <= ZERO)
                           INITIALIZE             AR137WS-CFINC
                           INITIALIZE             AR137WS-OFINC
                           MOVE 104            TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE    TO T21-MIN-MESSAGE
                       ELSE
                           INITIALIZE             AR137WS-CFINC
                                                  AR137WS-OFINC
                           MOVE 102            TO CRT-MSG-NBR
                           PERFORM 790-GET-MSG
                           MOVE CRT-MESSAGE    TO T21-MIN-MESSAGE.

           IF  (ACO-LATE-CH-RND-FL               = "Y")
           AND (T21-MIN-MESSAGE                  = SPACES)
               COMPUTE AR137WS-AMT-ROUND ROUNDED = AR137WS-CFINC
               COMPUTE AR137WS-CFINC             = AR137WS-AMT-ROUND
               COMPUTE AR137WS-AMT-ROUND ROUNDED = AR137WS-OFINC
               COMPUTE AR137WS-OFINC             = AR137WS-AMT-ROUND
               MOVE 103                          TO CRT-MSG-NBR
               PERFORM 790-GET-MSG
               MOVE CRT-MESSAGE                  TO T21-MIN-MESSAGE
           END-IF.
                   
      ****
      **** CREATE AROITEMS RECORDS FOR LATE AND/OR FINANCE CHARGES
      **** WHEN INTEREST-RATES-BY-DATES IS NOT USED
      ****
           IF  (PRM-UPDATE-OPTION          = "U")
           AND (AR137WS-CFINC              > ZEROES)
           AND (ACO-INT-BY-DATE-FL     NOT = "Y")
                PERFORM 2200-CREATE-AROITEMS
                THRU    2200-END
                IF  (ACM-FIN-DOC-PRNT       = "Y")
                AND (AR137WS-MIN-CHG-FL     = "*")
                     MOVE WF-OPEN-AMOUNT    TO AR137WS-OPEN         
                     PERFORM 2066-CREATE-NOTICEWRK
                     THRU    2066-END
                END-IF
                MOVE PRM-COMPANY           TO DB-COMPANY
                MOVE ACM-CUSTOMER          TO DB-CUSTOMER
                MOVE AR137WS-CFINC         TO ARWS-TRAN-AMT
                MOVE AR137WS-GROUP-RATE    TO ARWS-GROUP-RATE
                MOVE AR137WS-GROUP-MUDV    TO ARWS-GROUP-MUDV
                INITIALIZE                    ARWS-TRANS-TYPE
                INITIALIZE                    ARWS-GL-DATE
                MOVE "Y"                   TO ARWS-SKIP-MX-UPDATE
                PERFORM 700-UPDATE-ARCUSTOMER
           END-IF.

      ****
      **** CREATE AROITEMS RECORDS FOR LATE AND/OR FINANCE CHARGES
      **** WHEN INTEREST-RATES-BY-DATES IS USED
      ****
           IF  (PRM-UPDATE-OPTION          = "U")
           AND (AR137WS-CFINC              > ZEROES)
           AND (ACO-INT-BY-DATE-FL         = "Y")
                PERFORM 2200-CREATE-AROITEMS
                THRU    2200-END
                MOVE PRM-COMPANY           TO DB-COMPANY
                MOVE ACM-CUSTOMER          TO DB-CUSTOMER
                MOVE AR137WS-CFINC         TO ARWS-TRAN-AMT
                MOVE AR137WS-GROUP-RATE    TO ARWS-GROUP-RATE
                MOVE AR137WS-GROUP-MUDV    TO ARWS-GROUP-MUDV
                INITIALIZE                    ARWS-TRANS-TYPE
                INITIALIZE                    ARWS-GL-DATE
                MOVE "Y"                   TO ARWS-SKIP-MX-UPDATE
                MOVE WS-FALSE              TO AR137WS-NOTICEWRK-ADDED
                PERFORM 700-UPDATE-ARCUSTOMER
           END-IF.

      ****
      **** CREATE DETAIL AR DISTRIBUTION RECORDS FOR
      **** LATE AND/OR FINANCE CHARGES
      ****
           IF  (PRM-UPDATE-OPTION          = "U")
           AND (AR137WS-CFINC              > ZEROES)
           AND (ACO-DTL-FIN-CHRG           = "Y")
                PERFORM 2210-CREATE-DETAIL-ARDISTRIBS
                THRU    2210-END
           END-IF.

      ****
      **** CREATE AND PRINT TOTAL INFORMATION FOR AR CUSTOMER ON
      **** DETAIL REPORT
      ****
           MOVE ACO-BASE-ND                TO T21-ARO-CUST-TOTAL-ND.
           MOVE AR137WS-CFINC              TO T21-ARO-CUST-TOTAL.
           IF (ACM-CURRENCY-CD NOT = ACO-CURRENCY-CD)
               MOVE AR137WS-ORIG-ND        TO T21-ARO-ORIG-TOTAL-ND
               MOVE AR137WS-OFINC          TO T21-ARO-ORIG-TOTAL
               MOVE ACM-CURRENCY-CD        TO T21-CURRENCY-CD
           ELSE
               INITIALIZE                     T21-ARO-ORIG-TOTAL-ND
                                              T21-ARO-ORIG-TOTAL
                                              T21-CURRENCY-CD.
           MOVE TN21-ARO-CUSTOMER          TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           ADD AR137WS-CFINC               TO AR137WS-DFINC.
           ADD AR137WS-OFINC               TO AR137WS-ODFINC.

       2016-END.

      ******************************************************************
       2060-DO-ARO-CUSTOMER.
      ******************************************************************

      **** 
      **** FINANCE CHARGE DETAIL RECORD PROCESSING IN 1046-FINANCE-NET
      **** SETS WF-SEQ-NBR TO ZEROES
      ****
787215     IF (WF-PRINT-FL                 = ZERO)
474744         GO TO 2060-FIND-NEXT-AR137.

           IF (WF-SEQ-NBR = ZEROES)
               MOVE WF-PROCESS-LEVEL       TO AR137WS-PROC-LEVEL
               MOVE WF-FINANCE-AMOUNT      TO AR137WS-FIN-CHARGE
               MOVE WF-MIN-CHG-FL          TO AR137WS-MIN-CHG-FL
               MOVE WF-CUSTOMER            TO AR137WS-CUSTOMER
               IF (WF-FINANCE-AMOUNT       <= ZEROES)
               OR (WF-FINANCE-AMOUNT       < ACM-FIN-MIN-CHRG)
                   MOVE WS-FALSE           TO AR137WS-NOTICE-FL
               END-IF
               GO TO 2060-FIND-NEXT-AR137.

           IF (AR137WS-NOTICE-FL = WS-FALSE)
               GO TO 2060-CONT.

           IF (PRM-UPDATE-OPTION NOT =  "U")
               GO TO 2060-CONT.

           IF (ACM-FIN-DOC-PRNT NOT =  "Y")
               GO TO 2060-CONT.

           IF  (WF-TYPE = "D")
           AND (NOTICEWRK-WS-NOT-EXIST)
                MOVE WF-COMPANY            TO DB-COMPANY
                MOVE WF-TRANS-TYPE         TO DB-TRANS-TYPE
                MOVE WF-TRANS-NBR          TO DB-INVOICE
                MOVE WF-PAYMENT-SEQ        TO DB-PAYMENT-SEQ
                PERFORM 840-FIND-AROSET1
                IF (ARO-SUM-LINE = "Y")
                    MOVE WF-COMPANY        TO AR137WS-ARH-COMPANY
                    MOVE WF-CUSTOMER       TO AR137WS-ARH-CUSTOMER
                    MOVE ARO-ALT-TYPE      TO AR137WS-ARH-TRANS-TYPE
                    MOVE WF-TRANS-NBR      TO AR137WS-ARH-INVOICE
                END-IF
           END-IF.

      ******************************************************************
      **** IF WS-OPEN-AMOUNT NOT = ZEROES (FINANCE CHARGE RECORD)
      ****                                (NOTE:WF-LATE-AMOUNT = ZEROES)
      **** IF WS-LATE-AMOUNT NOT = ZEROES (LATE CHARGE RECORD)
      ****                                (NOTE:WF-OPEN-AMOUNT = ZEROES)
      ******************************************************************

           IF  (ACO-INT-BY-DATE-FL          = "Y")
           AND (WF-MEMO-CHG-FL          NOT = "*")
           AND (AR137WS-MIN-CHG-FL      NOT = "*")
                ADD 1                       TO AR137WS-SEQN
                IF (WF-OPEN-AMOUNT NOT      = ZEROES)
                    IF (WF-TRANS-TYPE       = "C")
                        COMPUTE WF-OPEN-AMOUNT   = WF-OPEN-AMOUNT
                                                 * NEGATIVE-ONE
                    END-IF
                    MOVE WF-OPEN-AMOUNT     TO AR137WS-OPEN
                ELSE
                    IF (WF-TRANS-TYPE       = "C")
                        COMPUTE WF-LATE-AMOUNT   = WF-LATE-AMOUNT
                                                 * NEGATIVE-ONE
                    END-IF
                    MOVE WF-LATE-AMOUNT     TO AR137WS-OPEN
                END-IF
                MOVE AR137WS-TRN            TO AR137WS-TRN-HOLD
                MOVE AR137WS-TRN-NEXT       TO AR137WS-TRN
                PERFORM 2065-CREATE-NOTICEWRK
                THRU    2065-END
                MOVE AR137WS-TRN-HOLD       TO AR137WS-TRN 
           END-IF.

           IF  (ACO-INT-BY-DATE-FL NOT = "Y")
           AND (WF-MEMO-CHG-FL     NOT = "*")
           AND (AR137WS-MIN-CHG-FL NOT = "*")
           AND (ARH-SUM-LINE       NOT = "Y")
                ADD 1                       TO AR137WS-SEQN
                IF (WF-OPEN-AMOUNT      NOT = ZEROES)
                    MOVE WF-OPEN-AMOUNT     TO AR137WS-OPEN
                ELSE
                    MOVE WF-LATE-AMOUNT     TO AR137WS-OPEN
                END-IF
                MOVE AR137WS-TRN            TO AR137WS-TRN-HOLD
                MOVE AR137WS-TRN-NEXT       TO AR137WS-TRN
                PERFORM 2065-CREATE-NOTICEWRK
                THRU    2065-END
                MOVE AR137WS-TRN-HOLD       TO AR137WS-TRN 
           END-IF.

           IF  (ACO-INT-BY-DATE-FL NOT = "Y")
           AND (WF-MEMO-CHG-FL     NOT = "*")
           AND (AR137WS-MIN-CHG-FL NOT = "*")
           AND (ARH-SUM-LINE           = "Y")
           AND (NOTICEWRK-WS-NOT-EXIST)
                ADD 1                       TO AR137WS-SEQN
                IF (WF-OPEN-AMOUNT      NOT = ZEROES)
                    MOVE WF-OPEN-AMOUNT     TO AR137WS-OPEN
                ELSE
                    MOVE WF-LATE-AMOUNT     TO AR137WS-OPEN
                END-IF
                MOVE AR137WS-TRN            TO AR137WS-TRN-HOLD
                MOVE AR137WS-TRN-NEXT       TO AR137WS-TRN
                PERFORM 2063-CREATE-NOTICEWRK-WS
                THRU    2063-END
                MOVE AR137WS-TRN-HOLD       TO AR137WS-TRN 
                GO TO 2060-CONT 
           END-IF.

           IF  (ACO-INT-BY-DATE-FL NOT = "Y")
           AND (WF-MEMO-CHG-FL     NOT = "*")
           AND (AR137WS-MIN-CHG-FL NOT = "*")
           AND (ARH-SUM-LINE           = "Y")
           AND (NOTICEWRK-WS-EXISTS)
                IF (WF-OPEN-AMOUNT      NOT = ZEROES)
                    MOVE WF-OPEN-AMOUNT     TO AR137WS-OPEN
                ELSE
                    MOVE WF-LATE-AMOUNT     TO AR137WS-OPEN
                END-IF
                PERFORM 2063-CREATE-NOTICEWRK-WS
                THRU    2063-END
           END-IF.      

           IF (PRM-UPDATE-OPTION  = "U")
               IF  (NOTICEWRK-WS-EXISTS)
               AND ((AR137-NOTFOUND)
               OR   (WF-COMPANY   NOT = AR137WS-ARH-COMPANY)
               OR   (WF-CUSTOMER  NOT = AR137WS-ARH-CUSTOMER) 
               OR   (WF-TRANS-NBR NOT = AR137WS-ARH-INVOICE))
               OR   (WF-TYPE      NOT = "D")
                     PERFORM 2064-CREATE-NOTICEWRK
                     THRU    2064-END.

       2060-CONT.

020435*--- JT-1020435 
020435     IF  (ACO-INT-BY-DATE-FL = "Y")
020435     AND (PRM-SUPPRESS-ZEROS = "Y")
020435     AND (ACM-I-FIN-MIN-CHRG  NOT = ZEROS)
116658     AND (AR137WS-FIN-ABSOL-VAL   < ACM-I-FIN-MIN-CHRG)     
020435          GO TO 2060-FIND-NEXT-AR137
020435     END-IF.
020435
           MOVE WF-TRANS-TYPE              TO WS-ARO-TRANS-TYPE.
           MOVE WF-TRANS-NBR               TO WS-ARO-INVOICE.

           MOVE WF-PROCESS-LEVEL           TO G3-ARO-PROCESS-LEVEL.
           MOVE WF-TRANS-TYPE              TO G3-ARO-TRANS-TYPE.
           MOVE WF-TRANS-NBR               TO G3-ARO-INVOICE.

      **** WF-DUE-DATE INCLUES GRACE PERIOD DAYS
           IF (ACO-INT-BY-DATE-FL NOT = "Y")
               MOVE WF-DUE-DATE            TO G3-ARO-DUE-DATE
           ELSE
               MOVE WF-ARO-DUE-DATE        TO G3-ARO-DUE-DATE
           END-IF.

           MOVE ACO-BASE-ND                TO G3-OPEN-AMOUNT-ND
                                              G3-LATE-PAYMENT-ND
                                              G3-FINANCE-AMT-ND.

           COMPUTE G3-ARF-CHRG-RATE        =  WF-CHRG-RATE
                                           *  100.
342466     IF (WF-COUNTRY-CODE             = "SE")
342466         COMPUTE G3-ARF-CHRG-RATE = G3-ARF-CHRG-RATE * 12.

           MOVE WF-NO-OF-DAYS              TO G3-NO-OF-DAYS.

           MOVE WF-ORIG-CURRENCY           TO AR137WS-ORIG-CURRENCY.
           MOVE WF-ORIG-RATE               TO AR137WS-ORIG-RATE.
           MOVE WF-CURR-MUDV               TO AR137WS-CURR-MUDV.
           MOVE WF-ORIG-ND                 TO AR137WS-ORIG-ND.

           IF (WF-DPF NOT = ZEROES)
               IF (ACO-INT-BY-DATE-FL      = "Y")
                   MOVE WF-DPF-EFF         TO G3-DPD-PAID-LATE
               ELSE
                   MOVE WF-DPF             TO G3-DPD-PAID-LATE
               END-IF
               IF  (WF-TRANS-TYPE          = "C" OR "P")
               AND (PRM-UPDATE-OPTION      = "R")
                   COMPUTE WF-OPEN-AMOUNT  = WF-OPEN-AMOUNT
                                           * NEGATIVE-ONE
               END-IF
               MOVE WF-OPEN-AMOUNT         TO G3-OPEN-AMOUNT
               INITIALIZE                     G3-LATE-PAYMENT
           ELSE
               MOVE WF-DPL-EFF             TO G3-DPD-PAID-LATE
               IF  (WF-TRANS-TYPE          = "C")
               AND (PRM-UPDATE-OPTION      = "R")
                   COMPUTE WF-LATE-AMOUNT  = WF-LATE-AMOUNT
                                           * NEGATIVE-ONE
               END-IF
               MOVE WF-LATE-AMOUNT         TO G3-LATE-PAYMENT
               INITIALIZE                     G3-OPEN-AMOUNT
           END-IF.

           MOVE WF-FINANCE-AMOUNT          TO G3-FINANCE-AMT.

           IF (WF-ORIG-CURRENCY NOT = ACO-CURRENCY-CD)
               MOVE "*"                    TO G3-CURRENCY-FL
           ELSE
               MOVE SPACES                 TO G3-CURRENCY-FL.

           IF (ACO-INT-BY-DATE-FL = "Y")
               MOVE WF-MEMO-CHG-FL         TO G3-MEMO-RECORD
           ELSE
               MOVE AR137WS-MIN-CHG-FL     TO G3-MEMO-RECORD
           END-IF.

           MOVE GN3D-ARO-INVOICE           TO RPT-GROUP-REQUEST.
           PERFORM 700-PRINT-RPT-GRP.

           IF   (ACO-INT-BY-DATE-FL        = "Y")
           AND  (ACM-I-FIN-MIN-CHRG        NOT = ZEROS)
336027     AND  (WF-TOT-INV-LATE-CHG < ACM-I-FIN-MIN-CHRG)
               MOVE GN3D-INV-MINIMUM       TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.

           IF  (WF-MEMO-CHG-FL     NOT = "*")
           AND (AR137WS-MIN-CHG-FL NOT = "*")
               ADD WF-FINANCE-AMOUNT         TO AR137WS-CFINC
               ADD WF-ORIG-AMT               TO AR137WS-OFINC
           END-IF.

           IF (ACO-INT-BY-DATE-FL          = "N")
               IF  (PRM-UPDATE-OPTION      = "U")
               AND (AR137WS-FIN-CHARGE     > ZEROES)
               AND (AR137WS-NOTICE-FL      = WS-TRUE)
                   PERFORM 2080-UPDATE-ARO-DATE
                   THRU    2080-END
               END-IF
           END-IF.

           IF (ACO-INT-BY-DATE-FL          = "Y")
               IF  (PRM-UPDATE-OPTION      = "U")
               AND (ACM-LATE-PAY-FL        = "B")
                   IF (WF-TRANS-TYPE   NOT = "P")
                       PERFORM 2080-UPDATE-ARO-DATE
                       THRU    2080-END
                   ELSE
                       PERFORM 2085-UPDATE-APM-DATE
                       THRU    2085-END
                   END-IF
               END-IF
           END-IF.

       2060-FIND-NEXT-AR137.

           INITIALIZE                         AR137WS-MIN-CHG-FL.

           ADD 1                            TO AR137WS-REC-COUNTER.
           IF  (PRM-UPDATE-OPTION           = "U")
           AND (AR137WS-REC-COUNTER         > WS-MAX-OPS-IN-TRAN)
               PERFORM 840-MODIFY-CKPSET1
               MOVE WF-COMPANY             TO WS-RESTART-COMPANY
               MOVE WF-CUSTOMER            TO WS-RESTART-CUSTOMER
               MOVE WF-SEQ-NBR             TO WS-RESTART-SEQ-NBR
               MOVE WF-TRANS-TYPE          TO WS-RESTART-TRANS-TYPE
               MOVE WF-TRANS-NBR           TO WS-RESTART-TRANS-NBR
               MOVE WF-PAYMENT-SEQ         TO WS-RESTART-PAYMENT-SEQ
               MOVE WS-RESTART-INFO        TO CKP-RESTART-INFO
               PERFORM 820-STORE-CKPOINT
               PERFORM 900-SAVE-OUTPUT-FILES
               PERFORM 920-AUDIT-END
               PERFORM 910-AUDIT-BEGIN
               INITIALIZE AR137WS-REC-COUNTER.

           READ AR137W1-FILE NEXT RECORD
                AT END
                   MOVE WS-TRUE            TO AR137WS-FILE.

       2060-END.

      ******************************************************************
       2063-CREATE-NOTICEWRK-WS.
      ******************************************************************

           IF (NOTICEWRK-WS-EXISTS)
               ADD AR137WS-OPEN            TO AR137WS-NTW-AMOUNT-1
               ADD WF-FINANCE-AMOUNT       TO AR137WS-NTW-AMOUNT-2
               GO TO 2063-END
           END-IF.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE PRM-TEXT-CODE              TO DB-TEXT-CODE.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.

           PERFORM 840-FIND-ACMSET1.

           IF (ARCUSTOMER-FOUND)
               MOVE ACM-LANGUAGE-CODE      TO DB-LANGUAGE-CODE
           ELSE
               MOVE SPACES                 TO DB-LANGUAGE-CODE
           END-IF.

           PERFORM 840-FIND-ANTSET1.

           IF (ARPAYNOTTX-NOTFOUND)
               INITIALIZE                     ANT-COMMENT-TEXT(1)
                                              ANT-COMMENT-TEXT(2)
                                              ANT-COMMENT-TEXT(3)
                                              ANT-COMMENT-TEXT(4).

           MOVE PRM-COMPANY                TO AR137WS-NTW-COMPANY
                                              AR137WS-ARH-COMPANY.
           MOVE WF-CUSTOMER                TO AR137WS-NTW-CUSTOMER
                                              AR137WS-ARH-CUSTOMER.
           MOVE WF-TRANS-NBR               TO AR137WS-ARH-INVOICE.
           MOVE AR137WS-TRN                TO AR137WS-NTW-NOTICE-NBR.
           MOVE PRM-AS-OF-DATE             TO AR137WS-NTW-DATE.
           MOVE ACO-CURRENCY-CD            TO AR137WS-NTW-CURRENCY-CD.
           MOVE ANT-COMMENT-TEXT(1)        TO AR137WS-COMMENT-LINE-1A.
           MOVE AR137WS-COMMENT-LINE-1     TO AR137WS-NTW-COMMENT-WRK-1.
           MOVE ANT-COMMENT-TEXT(2)        TO AR137WS-NTW-COMMENT-WRK-2.
           MOVE ANT-COMMENT-TEXT(3)        TO AR137WS-NTW-COMMENT-WRK-3.
           MOVE ANT-COMMENT-TEXT(4)        TO AR137WS-NTW-COMMENT-WRK-4.
           MOVE AR137WS-OPEN               TO AR137WS-NTW-AMOUNT-1.
           MOVE WF-FINANCE-AMOUNT          TO AR137WS-NTW-AMOUNT-2.
           MOVE AR137WS-SEQN               TO AR137WS-NTW-SEQ-NBR.
           MOVE WF-CHRG-RATE               TO AR137WS-NTW-CHRG-RATE.
           MOVE WF-DEPOSIT-DATE            TO AR137WS-NTW-DATE-PAID.
           MOVE WF-NO-OF-DAYS              TO AR137WS-NTW-RATE-DAYS.

      ****
      **** DETAIL CALCULATION METHOD
      ****
           IF (AR137WS-TYPE = "D")
               MOVE "D"                    TO AR137WS-NTW-NOTICE-TYPE
               MOVE WF-TRANS-NBR           TO AR137WS-NTW-NOTICE-VAR-1
               MOVE WF-TRANS-DATE          TO AR137WS-DATE-YMD
               INITIALIZE NTW-NOTICE-VAR (2)
               MOVE AR137WS-DATE           TO AR137WS-NTW-NOTICE-VAR-2
               MOVE WF-DUE-DATE            TO AR137WS-DATE-YMD
               INITIALIZE NTW-NOTICE-VAR (3)
               MOVE AR137WS-DATE           TO AR137WS-NTW-NOTICE-VAR-3
               IF (WF-DPF NOT = ZEROES)
                   MOVE WF-DPF             TO AR137WS-TEMP1
                   INITIALIZE NTW-NOTICE-VAR (4)
                   STRING "       " AR137WS-TEMP1-RED DELIMITED BY SIZE
                                         INTO AR137WS-NTW-NOTICE-VAR-4
               ELSE
                   MOVE WF-DPL             TO AR137WS-TEMP1
                   INITIALIZE NTW-NOTICE-VAR (4)
                   STRING "       " AR137WS-TEMP1-RED DELIMITED BY SIZE
                                         INTO AR137WS-NTW-NOTICE-VAR-4
               END-IF
               INITIALIZE                     AR137WS-NTW-UPDATE-DATE
               INITIALIZE                     AR137WS-NTW-DESCRIPTION
           END-IF.

           MOVE WS-TRUE                    TO AR137WS-NOTICEWRK-FLG.

       2063-END.

      ******************************************************************
       2064-CREATE-NOTICEWRK.
      ******************************************************************

           PERFORM 800-CREATE-NOTICEWRK.

           MOVE AR137WS-NTW-COMPANY        TO NTW-COMPANY.
           MOVE AR137WS-NTW-CUSTOMER       TO NTW-CUSTOMER.
           MOVE AR137WS-NTW-NOTICE-NBR     TO NTW-NOTICE-NBR.
           MOVE AR137WS-NTW-DATE           TO NTW-DATE.
           MOVE AR137WS-NTW-CURRENCY-CD    TO NTW-CURRENCY-CD.
           MOVE AR137WS-NTW-COMMENT-WRK-1  TO NTW-COMMENT-WRK (1).
           MOVE AR137WS-NTW-COMMENT-WRK-2  TO NTW-COMMENT-WRK (2).
           MOVE AR137WS-NTW-COMMENT-WRK-3  TO NTW-COMMENT-WRK (3).
           MOVE AR137WS-NTW-COMMENT-WRK-4  TO NTW-COMMENT-WRK (4).
           MOVE AR137WS-NTW-AMOUNT-1       TO NTW-AMOUNT (1).
           MOVE AR137WS-NTW-AMOUNT-2       TO NTW-AMOUNT (2).
           MOVE AR137WS-NTW-SEQ-NBR        TO NTW-SEQ-NBR.
           MOVE AR137WS-NTW-CHRG-RATE      TO NTW-CHRG-RATE.
           MOVE AR137WS-NTW-DATE-PAID      TO NTW-DATE-PAID.
           MOVE AR137WS-NTW-RATE-DAYS      TO NTW-RATE-DAYS.
           MOVE AR137WS-NTW-NOTICE-TYPE    TO NTW-NOTICE-TYPE.
           MOVE AR137WS-NTW-NOTICE-VAR-1   TO NTW-NOTICE-VAR (1).
           MOVE AR137WS-NTW-NOTICE-VAR-2   TO NTW-NOTICE-VAR (2).
           MOVE AR137WS-NTW-NOTICE-VAR-3   TO NTW-NOTICE-VAR (3).
           MOVE AR137WS-NTW-NOTICE-VAR-4   TO NTW-NOTICE-VAR (4).
           MOVE AR137WS-NTW-UPDATE-DATE    TO NTW-UPDATE-DATE.
           MOVE AR137WS-NTW-DESCRIPTION    TO NTW-DESCRIPTION.

           PERFORM 820-STORE-NOTICEWRK.

           MOVE WS-FALSE                   TO AR137WS-NOTICEWRK-FLG.

       2064-END.

      ******************************************************************
       2065-CREATE-NOTICEWRK.
      ******************************************************************

      **** DATA AND LANGUAGE TRANSLATION
           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE PRM-TEXT-CODE              TO DB-TEXT-CODE.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.

           PERFORM 840-FIND-ACMSET1.

           IF (ARCUSTOMER-FOUND)
               MOVE ACM-LANGUAGE-CODE      TO DB-LANGUAGE-CODE
           ELSE
               MOVE SPACES                 TO DB-LANGUAGE-CODE
           END-IF.

           PERFORM 840-FIND-ANTSET1.

           IF (ARPAYNOTTX-NOTFOUND)
               INITIALIZE ANT-COMMENT-TEXT(1)
               INITIALIZE ANT-COMMENT-TEXT(2)
               INITIALIZE ANT-COMMENT-TEXT(3)
               INITIALIZE ANT-COMMENT-TEXT(4).

      **** END

           PERFORM 800-CREATE-NOTICEWRK.

           MOVE PRM-COMPANY                TO NTW-COMPANY.
           MOVE WF-CUSTOMER                TO NTW-CUSTOMER.
           MOVE AR137WS-TRN                TO NTW-NOTICE-NBR.
           MOVE PRM-AS-OF-DATE             TO NTW-DATE.
           MOVE ACO-CURRENCY-CD            TO NTW-CURRENCY-CD.

      **** DATA AND LANGUAGE TRANSLATION
           MOVE ANT-COMMENT-TEXT(1)        TO AR137WS-COMMENT-LINE-1A.
           MOVE AR137WS-COMMENT-LINE-1     TO NTW-COMMENT-WRK (1).
      **** END

           MOVE ANT-COMMENT-TEXT(2)        TO NTW-COMMENT-WRK (2).
           MOVE ANT-COMMENT-TEXT(3)        TO NTW-COMMENT-WRK (3).
           MOVE ANT-COMMENT-TEXT(4)        TO NTW-COMMENT-WRK (4).
           MOVE AR137WS-OPEN               TO NTW-AMOUNT (1).
           MOVE WF-FINANCE-AMOUNT          TO NTW-AMOUNT (2).
           MOVE AR137WS-SEQN               TO NTW-SEQ-NBR.

           MOVE WF-CHRG-RATE               TO NTW-CHRG-RATE.
           MOVE WF-DEPOSIT-DATE            TO NTW-DATE-PAID.
           MOVE WF-NO-OF-DAYS              TO NTW-RATE-DAYS.

      ****
      **** DETAIL CALCULATION METHOD
      ****
           IF (AR137WS-TYPE = "D")
               MOVE "D"                    TO NTW-NOTICE-TYPE
               MOVE WF-TRANS-NBR           TO NTW-NOTICE-VAR (1)
               MOVE WF-TRANS-DATE          TO AR137WS-DATE-YMD
               INITIALIZE NTW-NOTICE-VAR (2)
               MOVE AR137WS-DATE           TO NTW-NOTICE-VAR (2)
               MOVE WF-DUE-DATE            TO AR137WS-DATE-YMD
               INITIALIZE NTW-NOTICE-VAR (3)
               MOVE AR137WS-DATE           TO NTW-NOTICE-VAR (3)
               IF (WF-DPF NOT = ZEROES)
                   MOVE WF-DPF             TO AR137WS-TEMP1
                   INITIALIZE NTW-NOTICE-VAR (4)
                   STRING "       " AR137WS-TEMP1-RED DELIMITED BY SIZE
                                         INTO NTW-NOTICE-VAR (4)
               ELSE
                   MOVE WF-DPL             TO AR137WS-TEMP1
                   INITIALIZE NTW-NOTICE-VAR (4)
                   STRING "       " AR137WS-TEMP1-RED DELIMITED BY SIZE
                                         INTO NTW-NOTICE-VAR (4)
               END-IF
               INITIALIZE                     NTW-UPDATE-DATE
               INITIALIZE                     NTW-DESCRIPTION
           END-IF.

      ****
      **** NET CALCULATION METHOD
      ****
           IF (AR137WS-TYPE = "N")
               MOVE "N"                    TO NTW-NOTICE-TYPE
               INITIALIZE                     NTW-NOTICE-VAR (1)
                                              NTW-NOTICE-VAR (2)
                                              NTW-NOTICE-VAR (3)
                                              NTW-NOTICE-VAR (4)
                                              NTW-UPDATE-DATE
                                              NTW-DESCRIPTION
           END-IF.

           PERFORM 820-STORE-NOTICEWRK.

       2065-END.

      ******************************************************************
       2066-CREATE-NOTICEWRK.
      ******************************************************************

      **** DATA AND LANGUAGE TRANSLATION
           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE PRM-TEXT-CODE              TO DB-TEXT-CODE.
           MOVE WF-CUSTOMER                TO DB-CUSTOMER.

           PERFORM 840-FIND-ACMSET1.

           IF (ARCUSTOMER-FOUND)
               MOVE ACM-LANGUAGE-CODE      TO DB-LANGUAGE-CODE
           ELSE
               MOVE SPACES                 TO DB-LANGUAGE-CODE
           END-IF.

           PERFORM 840-FIND-ANTSET1.

           IF (ARPAYNOTTX-NOTFOUND)
               INITIALIZE ANT-COMMENT-TEXT(1)
               INITIALIZE ANT-COMMENT-TEXT(2)
               INITIALIZE ANT-COMMENT-TEXT(3)
               INITIALIZE ANT-COMMENT-TEXT(4).

      **** END

           PERFORM 800-CREATE-NOTICEWRK.

           MOVE PRM-COMPANY                TO NTW-COMPANY.
           MOVE AR137WS-CUSTOMER           TO NTW-CUSTOMER.
           MOVE AR137WS-TRN                TO NTW-NOTICE-NBR.
           MOVE PRM-AS-OF-DATE             TO NTW-DATE.
           MOVE ACO-CURRENCY-CD            TO NTW-CURRENCY-CD.

      **** DATA AND LANGUAGE TRANSLATION
           MOVE ANT-COMMENT-TEXT(1)        TO AR137WS-COMMENT-LINE-1A.
           MOVE AR137WS-COMMENT-LINE-1     TO NTW-COMMENT-WRK (1).
      **** END

           MOVE ANT-COMMENT-TEXT(2)        TO NTW-COMMENT-WRK (2).
           MOVE ANT-COMMENT-TEXT(3)        TO NTW-COMMENT-WRK (3).
           MOVE ANT-COMMENT-TEXT(4)        TO NTW-COMMENT-WRK (4).
           MOVE AR137WS-OPEN               TO NTW-AMOUNT (1).
           MOVE AR137WS-FIN-CHARGE         TO NTW-AMOUNT (2).
           MOVE AR137WS-SEQN               TO NTW-SEQ-NBR.

           INITIALIZE                         NTW-CHRG-RATE.
           INITIALIZE                         NTW-DATE-PAID.

      **** DETAIL CALCULATION METHOD
           IF (AR137WS-TYPE = "D")
               MOVE "D"                    TO NTW-NOTICE-TYPE
               INITIALIZE                     NTW-NOTICE-VAR (1)
               MOVE PRM-AS-OF-DATE         TO AR137WS-DATE-YMD
               INITIALIZE                     NTW-NOTICE-VAR (2)
               MOVE AR137WS-DATE           TO NTW-NOTICE-VAR (2)
               INITIALIZE                     AR137WS-DATE-YMD
               INITIALIZE                     NTW-NOTICE-VAR (3)
               MOVE AR137WS-DATE           TO NTW-NOTICE-VAR (3)
               INITIALIZE                     NTW-NOTICE-VAR (4)
               INITIALIZE                     NTW-UPDATE-DATE
               INITIALIZE                     NTW-DESCRIPTION.

           PERFORM 820-STORE-NOTICEWRK.

       2066-END.

      ******************************************************************
       2080-UPDATE-ARO-DATE.
      ******************************************************************

      **** WHEN WF-OPEN-AMOUNT NOT = ZEROES = FINANCE CHARGE RECORD
      **** WHEN WF-LATE-AMOUNT NOT = ZEROES = LATE CHARGE RECORD
      ****
           IF (WF-SEQ-NBR              NOT = ZEROES)
               MOVE PRM-COMPANY            TO DB-COMPANY
               MOVE WF-TRANS-TYPE          TO DB-TRANS-TYPE
               MOVE WF-TRANS-NBR           TO DB-INVOICE
               MOVE WF-PAYMENT-SEQ         TO DB-PAYMENT-SEQ
               PERFORM 840-MODIFY-AROSET1
               IF (AROITEMS-FOUND)
                   MOVE PRM-AS-OF-DATE     TO ARO-LAST-FC-DATE
                   PERFORM 820-STORE-AROITEMS
               END-IF
           END-IF.
       2080-END.

      ******************************************************************
       2085-UPDATE-APM-DATE.
      ******************************************************************

           IF (WF-SEQ-NBR              NOT = ZEROES)
               MOVE WF-COMPANY             TO DB-COMPANY
               MOVE WF-BATCH-NBR           TO DB-BATCH-NBR
               MOVE WF-PAYMENT-SEQ         TO DB-PAYMENT-SEQ
               PERFORM 840-MODIFY-APMSET1
               IF (ARPAYMENT-FOUND)
                   MOVE PRM-AS-OF-DATE     TO APM-LAST-FC-DATE
                   PERFORM 820-STORE-ARPAYMENT
               END-IF
           END-IF.
       2085-END.
           
      ******************************************************************
       2200-CREATE-AROITEMS.
      ******************************************************************

           PERFORM 800-CREATE-AROITEMS.
           PERFORM 3000-INIT-ARACPIVOT-ARO.
           PERFORM 800-CREATE-AROIHDR.

           MOVE PRM-COMPANY                TO ARO-COMPANY
                                              ARH-COMPANY.
      ****
      **** CREATE DEBIT MEMOS (ARO-TRANS-TYPE = "D")
      ****
           MOVE "D"                        TO ARO-TRANS-TYPE
                                              ARH-TRANS-TYPE.
           MOVE "D"                        TO ARO-ALT-TYPE
                                              ARH-ALT-TYPE.
           ADD 1                           TO AR137WS-FANB.
           MOVE AR137WS-FANB               TO AR137WS-NBR.
           MOVE AR137WS-TRN                TO ARO-INVOICE
                                              ARH-INVOICE.
           MOVE ACM-MEMO-TERM              TO ARH-TERMS-CD.
           MOVE ACM-CUSTOMER               TO ARO-CUSTOMER
                                              ARH-CUSTOMER.
      ****
      **** OPEN TRANSACTIONS (ARO-STATUS = "1")
      ****
           MOVE 1                          TO ARO-STATUS
                                              ARH-STATUS.
           ADD 1                           TO AR137WS-SUMB.
           MOVE AR137WS-SUMB               TO ARH-BATCH-NBR.
           MOVE 1                          TO ARO-PAYMENT-SEQ
                                              ARH-PAYMENT-SEQ.

           MOVE PRM-COMPANY                TO DB-COMPANY.

           IF (ACM-AR-CODE             NOT = SPACES)
               MOVE ACM-AR-CODE            TO ARH-AR-CODE
               MOVE AR137WS-PROC-LEVEL     TO ARH-PROCESS-LEVEL
                                              ARO-PROCESS-LEVEL
           ELSE                        
               MOVE AR137WS-PROC-LEVEL     TO DB-PROCESS-LEVEL
                                              ARH-PROCESS-LEVEL
                                              ARO-PROCESS-LEVEL
               PERFORM 840-FIND-APVSET1
               MOVE APV-AR-CODE            TO ARH-AR-CODE
           END-IF.

           MOVE PRM-AS-OF-DATE             TO ARH-TRANS-DATE
                                              ARH-GL-DATE
                                              ARO-TRANS-DATE.
342466     IF (PRM-DUE-DATE                NOT = ZEROS)
342466         MOVE PRM-DUE-DATE           TO ARO-DUE-DATE
342466     ELSE
               MOVE PRM-AS-OF-DATE         TO ARO-DUE-DATE.
      ****
      **** ARH-ORIG-CODE = "F" = FINANCE AND/OR LATE PAYMENT CHARGES
      ****
           MOVE "F"                        TO ARH-ORIG-CODE.
           MOVE "AR"                       TO ARH-SYSTEM.
      ****
      **** MESSAGE 100 = "FINANCE-LATE CHARGES"
      ****
           MOVE "N"                        TO ARH-SUM-LINE.
           MOVE 100                        TO CRT-MSG-NBR.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE                TO ARH-DESC.
           MOVE ACM-SALESMAN               TO ARH-SALESMAN.
           MOVE AR137WS-ORIG-CURRENCY      TO ARH-ORIG-CURRENCY.
      ****
      **** SUM OF FINANCE AND/OR LATE PAYMENT CHARGES 
      ****
           MOVE AR137WS-CFINC              TO ARO-TRAN-AMT
                                              ARH-TRAN-AMT
                                              ARH-BASE-DETAIL
                                              ARO-SORT-AMOUNT.
      ****
      **** SUM OF ORIGINAL OR BASE AMOUNTS USED TO COMPUTE FINANCE
      **** AND/OR LATE PAYMENT CHARGES 
      ****
           MOVE AR137WS-OFINC              TO ARO-ORIG-AMT
                                              ARH-ORIG-AMT.
           MOVE AR137WS-GROUP-RATE         TO ARH-GROUP-RATE.
           MOVE AR137WS-GROUP-MUDV         TO ARH-GROUP-MUDV.
           IF (AR137WS-CURR-RATE           NOT = 1)
               MOVE PRM-COMPANY            TO IFCRWS-COMPANY
               MOVE ACM-CURRENCY-CD        TO IFCRWS-FR-CURR-CODE
               MOVE ACO-CURRENCY-CD        TO IFCRWS-TO-CURR-CODE
               PERFORM 655-EDIT-CURRENCY-RELATION-60
               MOVE PRM-COMPANY            TO IFGRWS-COMPANY
               MOVE ACO-CURRENCY-CD        TO IFGRWS-TO-CURR-CODE
               MOVE ACM-CURRENCY-CD        TO IFGRWS-FR-CURR-CODE
               MOVE "AR"                   TO IFGRWS-SYSTEM
               MOVE PRM-AS-OF-DATE         TO IFGRWS-EFFECT-DATE
               PERFORM 660-GET-CURRENCY-RATE-60
               MOVE IFCRWS-MULT-DIV        TO ARH-CURR-MUDV
               MOVE IFGRWS-SELL-RATE       TO ARH-ORIG-RATE
           ELSE
               MOVE AR137WS-CURR-RATE      TO ARH-ORIG-RATE
               MOVE AR137WS-CURR-MUDV      TO ARH-CURR-MUDV.
           MOVE "N"                        TO ARH-REVALUE-FL.
           MOVE AR137WS-ORIG-ND            TO ARH-ORIG-ND.
           MOVE ACM-BANK-INST-CODE         TO ARH-BANK-INST-CODE.
           MOVE ARH-BANK-INST-CODE         TO ARO-BANK-INST-CODE.
           MOVE ACM-BANK-INST-CODE         TO CBEBWS-BANK-INST-CODE.
           PERFORM 550-EDIT-BANK-INST-CODE-70.
           IF (NO-ERROR-FOUND)
               MOVE CBEBWS-BANK-INST-TYPE  TO ARH-BANK-INST-TYPE.
           MOVE ARH-BANK-INST-TYPE         TO ARO-BANK-INST-TYPE.

           MOVE AR137WS-TRN                TO ARH-LATE-CH-TR-NBR.
           MOVE AR137WS-OFINC              TO ARH-LATE-CH-OR-AMT.
           MOVE AR137WS-CFINC              TO ARH-LATE-CH-TR-AMT.
           MOVE "N"                        TO ARH-LATE-CH-FL.

           MOVE "N"                        TO ARPVT-AROIHDR.
           MOVE ARH-CUST-PO-NBR            TO ARPVT-CUST-PO-NBR.
           MOVE ARH-PROCESS-LEVEL          TO ARPVT-PROCESS-LEVEL.
           MOVE ARH-TRANS-USER1            TO ARPVT-TRANS-USER1.
           MOVE ARH-BANK-INST-CODE         TO ARPVT-BANK-INST-CODE.
           MOVE ARH-BANK-INST-TYPE         TO ARPVT-BANK-INST-TYPE.
           MOVE ARH-XREF-TYPE              TO ARPVT-XREF-TYPE.
           MOVE ARH-XREF-NBR               TO ARPVT-XREF-NBR.
           MOVE ARH-ORIG-CURRENCY          TO ARPVT-ORIG-CURRENCY.
           MOVE ARH-ORIG-ND                TO ARPVT-ORIG-ND.
           MOVE ARH-ORIG-RATE              TO ARPVT-ORIG-RATE.
           MOVE ARH-CURR-MUDV              TO ARPVT-CURR-MUDV.
           MOVE ARH-SUM-LINE               TO ARPVT-SUM-LINE.
           MOVE "A"                        TO ARPVT-FC.
           PERFORM 2000-ARPVT-PROCESS-TRAN.
           INITIALIZE CRT-ERROR-NBR
                      CRT-ERROR-CAT.

           MOVE ARH-GL-DATE            TO ARO-GL-DATE.
           MOVE ARH-ORIG-ND            TO ARO-ORIG-ND.
           MOVE ARH-BATCH-NBR          TO ARO-BATCH-NBR.
           MOVE ARH-XREF-NBR           TO ARO-XREF-NBR.
           MOVE ARH-SALESMAN           TO ARO-SALESMAN.
           MOVE ARH-ORIG-RATE          TO ARO-ORIG-RATE.
           MOVE ARH-SUM-LINE           TO ARO-SUM-LINE.
           MOVE ARH-GROUP-RATE         TO ARO-GROUP-RATE.
           MOVE ARH-ORIG-CODE          TO ARO-ORIG-CODE.
           MOVE ARH-AR-CODE            TO ARO-AR-CODE.
           MOVE ARH-ORIG-CURRENCY      TO ARO-ORIG-CURRENCY.
           MOVE ARH-XREF-COMPANY       TO ARO-XREF-COMPANY.
           MOVE ARH-GROUP-MUDV         TO ARO-GROUP-MUDV.
           MOVE ARH-CURR-MUDV          TO ARO-CURR-MUDV.
           MOVE ARH-CANCEL-FLAG        TO ARO-CANCEL-FLAG.
           MOVE ARH-REVALUE-FL         TO ARO-REVALUE-FL.
           MOVE ARH-XREF-TYPE          TO ARO-XREF-TYPE.
           MOVE ARH-DESC               TO ARO-DESC.
           MOVE ARH-TERMS-CD           TO ARO-TERMS-CD.
           MOVE ARH-AC-CUSTOMER-ID     TO ARO-AC-CUSTOMER-ID.

           MOVE ARH-LATE-CH-FL         TO ARO-LATE-CH-FL.
           MOVE ARH-LST-DUNLTR-STM     TO ARO-LST-DUNLTR-STM.
           MOVE ARH-LAST-LTR-DATE      TO ARO-LAST-LTR-DATE.
           MOVE ARH-LST-LTRTEXT-CD     TO ARO-LST-LTRTEXT-CD.
           MOVE ARH-SENT-FOR-COLL      TO ARO-SENT-FOR-COLL.
           MOVE ARH-DUN-LTR-IND        TO ARO-DUN-LTR-IND.
           MOVE ARH-ASSESS-DUN-FEE     TO ARO-ASSESS-DUN-FEE.

           PERFORM 820-STORE-AROIHDR.
           PERFORM 3000-UPDATE-ARACPIVOT-ARO.
           PERFORM 820-STORE-AROITEMS.

           MOVE ARO-COMPANY                 TO DB-COMPANY.
           MOVE ARO-PROCESS-LEVEL           TO DB-PROCESS-LEVEL.
           MOVE "02"                        TO DB-TYPE.
           PERFORM 840-FIND-ARSSET1.
           IF (ARSUMMARY-NOTFOUND)
           OR (ARS-COMPANY NOT = ARO-COMPANY)
               PERFORM 800-CREATE-ARSUMMARY
               MOVE ARO-COMPANY            TO ARS-COMPANY
               MOVE ARH-PROCESS-LEVEL      TO ARS-PROCESS-LEVEL
               MOVE "02"                   TO ARS-TYPE
               MOVE ARO-TRAN-AMT           TO ARS-TODAYS-AMT
                                              ARS-PERIOD-AMT
                                              ARS-CURR-YR-AMT
           ELSE
               PERFORM 840-MODIFY-ARSSET1
               ADD  ARO-TRAN-AMT           TO ARS-TODAYS-AMT
                                              ARS-PERIOD-AMT
                                              ARS-CURR-YR-AMT.

           PERFORM 820-STORE-ARSUMMARY.

           MOVE "N"                        TO AR137WS-NO-RECS.

       2200-END.

      ******************************************************************
       2210-CREATE-DETAIL-ARDISTRIBS.
      ******************************************************************

           MOVE PRM-COMPANY                TO DB-COMPANY.

           IF (ACM-AR-CODE      NOT = SPACES)
               MOVE ACM-AR-CODE            TO DB-AR-CODE
           ELSE
               MOVE AR137WS-PROC-LEVEL     TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-APVSET1                  
               MOVE APV-AR-CODE            TO DB-AR-CODE.

           PERFORM 840-FIND-RCDSET1.
           IF  (ARCODE-FOUND)
           AND (RCD-USED-FL NOT = "Y")
               PERFORM 840-MODIFY-RCDSET1
               MOVE "Y"                    TO RCD-USED-FL
               PERFORM 820-STORE-ARCODE.

           PERFORM 2220-CREATE-DTL-ARDISTRIB
           THRU    2220-END.

           MOVE RCD-ACCT-UNIT              TO AMD-ACCT-UNIT
                                              AMD-BASE-ACCTUNIT.
           MOVE RCD-ACCOUNT                TO AMD-ACCOUNT.
           MOVE RCD-SUB-ACCT               TO AMD-SUB-ACCT.
           MOVE AR137WS-CFINC              TO AMD-TRAN-AMT
                                              AMD-TO-COMP-AMT.
           MOVE AR137WS-OFINC              TO AMD-ORIG-AMT.
           MOVE "02"                       TO AMD-UPDATE-SUM.
           MOVE 1                          TO AMD-DIST-SEQ.
           MOVE AR137WS-PROC-LEVEL         TO AMD-PROCESS-LEVEL.
           MOVE PRM-JRNL-BOOK-NBR          TO AMD-JRNL-BOOK-NBR.

           MOVE PRM-AS-OF-DATE             TO AMD-TRANS-DATE.
        
           PERFORM 820-STORE-ARDISTRIB.

           MOVE PRM-COMPANY                TO DB-COMPANY.

           MOVE AR137WS-PROC-LEVEL         TO DB-PROCESS-LEVEL.
           PERFORM 840-FIND-APVSET1.
           IF (APV-FIN-GL-CODE NOT = SPACES)
               MOVE APV-FIN-GL-CODE        TO DB-AR-CODE
           ELSE
               MOVE PRM-FIN-GL-CODE        TO DB-AR-CODE
           END-IF.

           PERFORM 840-FIND-RCDSET1.
           IF  (ARCODE-FOUND)
           AND (RCD-USED-FL NOT = "Y")
               PERFORM 840-MODIFY-RCDSET1
               MOVE "Y"                    TO RCD-USED-FL
               PERFORM 820-STORE-ARCODE.

           PERFORM 2220-CREATE-DTL-ARDISTRIB
           THRU    2220-END.

           MOVE RCD-ACCT-UNIT              TO AMD-ACCT-UNIT
                                              AMD-BASE-ACCTUNIT.
           MOVE RCD-ACCOUNT                TO AMD-ACCOUNT.
           MOVE RCD-SUB-ACCT               TO AMD-SUB-ACCT.
           INITIALIZE                         AMD-TRAN-AMT
                                              AMD-UPDATE-SUM
                                              AMD-ORIG-AMT
                                              AMD-TO-COMP-AMT.
           SUBTRACT AR137WS-CFINC        FROM AMD-TRAN-AMT
                                              AMD-TO-COMP-AMT.
           SUBTRACT AR137WS-OFINC        FROM AMD-ORIG-AMT.
           MOVE 2                          TO AMD-DIST-SEQ.
           MOVE AR137WS-PROC-LEVEL         TO AMD-PROCESS-LEVEL.
           MOVE PRM-JRNL-BOOK-NBR          TO AMD-JRNL-BOOK-NBR.

           MOVE PRM-AS-OF-DATE             TO AMD-TRANS-DATE.
        
           PERFORM 820-STORE-ARDISTRIB.

       2210-END.

      ******************************************************************
       2220-CREATE-DTL-ARDISTRIB.
      ******************************************************************

           PERFORM 800-CREATE-ARDISTRIB.

           MOVE PRM-COMPANY                TO AMD-GL-COMPANY.
           MOVE PRM-AS-OF-DATE             TO AMD-GL-DATE.
           MOVE AR137WS-SUMB               TO AMD-BATCH-NBR.
           MOVE WS-SYSTEM-DATE-YMD         TO AMD-CREATE-DATE.
           MOVE HHMMSS                     TO AMD-CREATE-TIME.
           MOVE PRM-COMPANY                TO AMD-COMPANY.
           MOVE "RN"                       TO AMD-DST-TYPE.
           MOVE "A"                        TO AMD-DST-SOURCE.
           MOVE "S"                        TO AMD-ACCUM-TYPE.
           MOVE AR137WS-DESC               TO AMD-DESC.
           MOVE 2                          TO AMD-STATUS.
           MOVE ACO-CURRENCY-CD            TO AMD-ORIG-CURRENCY.
           MOVE 1                          TO AMD-ORIG-RATE.
           ADD 1                           TO AR137WS-PGM.
           MOVE AR137WS-PGM                TO AMD-PROG-SEQ-NBR.
           MOVE ARO-CUSTOMER               TO AMD-CUSTOMER.
           MOVE ARO-TRANS-TYPE             TO AMD-TRANS-TYPE.
           MOVE ARO-INVOICE                TO AMD-INVOICE.

           PERFORM
               VARYING I1 FROM 1 BY 1 
               UNTIL  (I1                         > 3)
               OR     (IFSCATWS-FIELD-NAME (I1)   = SPACES)
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-CUSTOMER-TXT)  
                      MOVE ACM-CUSTOMER        TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1)
                                              = ARWS-PROCESS-LEVEL-TXT)
                      MOVE AR137WS-PROC-LEVEL   TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-BATCH-TXT)  
                      MOVE AR137WS-SUMB        TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-TRANS-USER1-TXT)
                      MOVE SPACES                TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-TRANS-USER2-TXT)
                      MOVE SPACES                TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-TRANS-USER3-TXT)
                      MOVE SPACES                TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-TRANS-USER4-TXT)
                      MOVE SPACES                TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-MAJ-CLASS-TXT)
                      MOVE ACM-MAJ-CLASS         TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-MIN-CLASS-TXT)
                      MOVE ACM-MIN-CLASS         TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1) = ARWS-SALESMAN-TXT)
                      MOVE ACM-SALESMAN          TO AMD-MX-VALUE (I1)
                  END-IF
                  IF (IFSCATWS-FIELD-NAME (I1)
                                           = ARWS-CREDIT-ANALYST-TXT)
                      MOVE ACM-CREDIT-ANLYST     TO AMD-MX-VALUE (I1)
                  END-IF
           END-PERFORM.

       2220-END.

      ******************************************************************
       2300-CREATE-ARDISTRIB.
      ******************************************************************
               
           IF (WFA-AMOUNT              = ZEROES)
               GO TO 2300-NEXT-AR137W2-FILE.

           MOVE WFA-AR-CODE                TO DB-AR-CODE.

           PERFORM 840-FIND-RCDSET1.
           IF  (ARCODE-FOUND)
           AND (RCD-USED-FL NOT = "Y")
               PERFORM 840-MODIFY-RCDSET1
               MOVE "Y"                    TO RCD-USED-FL
               PERFORM 820-STORE-ARCODE.

           PERFORM 2310-CREATE-COMP-ARDISTRIB
           THRU    2310-END.

           MOVE RCD-ACCT-UNIT              TO AMD-ACCT-UNIT
                                              AMD-BASE-ACCTUNIT.
           MOVE RCD-ACCOUNT                TO AMD-ACCOUNT.
           MOVE RCD-SUB-ACCT               TO AMD-SUB-ACCT.
           MOVE WFA-AMOUNT                 TO AMD-TRAN-AMT
                                              AMD-ORIG-AMT
                                              AMD-TO-COMP-AMT.
           MOVE "02"                       TO AMD-UPDATE-SUM.
           MOVE 1                          TO AMD-DIST-SEQ.
           MOVE WFA-PROCESS-LEVEL          TO AMD-PROCESS-LEVEL.
           MOVE PRM-JRNL-BOOK-NBR          TO AMD-JRNL-BOOK-NBR.

           PERFORM 820-STORE-ARDISTRIB.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           MOVE WFA-PROCESS-LEVEL          TO DB-PROCESS-LEVEL.
           PERFORM 840-FIND-APVSET1.

           IF (WFA-PROC-LEVEL-FL = "N")
               IF (APV-FIN-GL-CODE     NOT = SPACES)
                   MOVE APV-FIN-GL-CODE    TO DB-AR-CODE
               ELSE
                   MOVE PRM-FIN-GL-CODE    TO DB-AR-CODE
               END-IF
           ELSE
               MOVE PRM-FIN-GL-CODE        TO DB-AR-CODE
           END-IF.

           MOVE PRM-COMPANY                TO DB-COMPANY.
           PERFORM 840-FIND-RCDSET1.
           IF  (ARCODE-FOUND)
           AND (RCD-USED-FL NOT = "Y")
               PERFORM 840-MODIFY-RCDSET1
               MOVE "Y"                    TO RCD-USED-FL
               PERFORM 820-STORE-ARCODE.

           PERFORM 2310-CREATE-COMP-ARDISTRIB
           THRU    2310-END.

           MOVE RCD-ACCT-UNIT              TO AMD-ACCT-UNIT
                                              AMD-BASE-ACCTUNIT.
           MOVE RCD-ACCOUNT                TO AMD-ACCOUNT.
           MOVE RCD-SUB-ACCT               TO AMD-SUB-ACCT.
           INITIALIZE                         AMD-TRAN-AMT
                                              AMD-ORIG-AMT
                                              AMD-UPDATE-SUM
                                              AMD-TO-COMP-AMT.
           SUBTRACT WFA-AMOUNT           FROM AMD-TRAN-AMT
                                              AMD-ORIG-AMT
                                              AMD-TO-COMP-AMT.
           MOVE 2                          TO AMD-DIST-SEQ.
           MOVE WFA-PROCESS-LEVEL          TO AMD-PROCESS-LEVEL.
           MOVE PRM-JRNL-BOOK-NBR          TO AMD-JRNL-BOOK-NBR.

           PERFORM 820-STORE-ARDISTRIB.

       2300-NEXT-AR137W2-FILE.

           READ AR137W2-FILE NEXT RECORD
                AT END
                   MOVE WS-TRUE            TO AR137AWS-FILE.

       2300-END.

      ******************************************************************
       2310-CREATE-COMP-ARDISTRIB.
      ******************************************************************

           PERFORM 800-CREATE-ARDISTRIB.

           MOVE PRM-COMPANY                TO AMD-GL-COMPANY.
           MOVE PRM-AS-OF-DATE             TO AMD-GL-DATE
                                              AMD-TRANS-DATE.
           MOVE AR137WS-SUMB               TO AMD-BATCH-NBR.
           MOVE WS-SYSTEM-DATE-YMD         TO AMD-CREATE-DATE.
           MOVE HHMMSS                     TO AMD-CREATE-TIME.
           MOVE PRM-COMPANY                TO AMD-COMPANY.
           MOVE "RN"                       TO AMD-DST-TYPE.
           MOVE "A"                        TO AMD-DST-SOURCE.
           MOVE "S"                        TO AMD-ACCUM-TYPE.
           MOVE AR137WS-DESC               TO AMD-DESC.
           MOVE 2                          TO AMD-STATUS.
           MOVE ACO-CURRENCY-CD            TO AMD-ORIG-CURRENCY.
           MOVE 1                          TO AMD-ORIG-RATE.
           ADD 1                           TO AR137WS-PGM.
           MOVE AR137WS-PGM                TO AMD-PROG-SEQ-NBR.

       2310-END.

      ******************************************************************
       2400-MODIFY-ARAPPLIED.
      ******************************************************************

           MOVE WFB-COMPANY            TO DB-COMPANY.
           MOVE WFB-TRANS-TYPE         TO DB-TRANS-TYPE.
           MOVE WFB-TRANS-NBR          TO DB-INVOICE.
           MOVE WFB-PAYMENT-SEQ        TO DB-PAYMENT-SEQ.
           MOVE WFB-BATCH-NBR          TO DB-BATCH-NBR.
           MOVE WFB-APP-SEQ            TO DB-APP-SEQ.
           PERFORM 840-MODIFY-ARASET1.
           IF (ARAPPLIED-FOUND)
               IF (ARA-RESULT-FL       = "D")
                   MOVE "X"            TO ARA-RESULT-FL
               END-IF
               IF (ARA-RESULT-FL       = "L")
                   MOVE "Y"            TO ARA-RESULT-FL
               END-IF
               PERFORM 820-STORE-ARAPPLIED.

           READ AR137W3-FILE NEXT RECORD
               AT END
                   MOVE WS-TRUE        TO AR137BWS-FILE.

       2400-END.

      ******************************************************************
       2000-END.
      ******************************************************************
      ******************************************************************
      *
      *   600-BEGIN-BROADCAST
      *   605-END-BROADCAST
      *   610-BEG-ELEMENT-TAG
      *   615-END-ELEMENT-TAG
      *
      *   These routines are used for Channels.  Service must be defined
      *   in WF07.1 before running program.
      *
      *   Input:
      *           AR137WS-DO-BASE-BROADCAST    (Y/N)
      *           AR137WS-PRINT-FILE-1  (Base)
      *           AR137WS-ELEMENT-VALUE (Element Value)
      *           AR137WS-ELEMENT-TYPE  (Element)
      *
      *   Output: none
      *
      ******************************************************************
       600-BEGIN-BROADCAST         SECTION 72.
      ******************************************************************
       600-START.
      *** THE FOLLOWING LINES OF CODE MUST BE PERFORMED ONCE PER 
      *** EXECUTION OF THE PROGRAM PRIOR TO ANY WRITING OF THE PRINTFILE
      *** BEING PUBLISHED.

           INITIALIZE WFAPI-INPUT.
           INITIALIZE WFAPI-OUTPUT.
           MOVE WFCHWS-SERVICE-NAME    TO WFAPI-I-SERVICE.
           MOVE CRT-PROGRAM-CODE       TO WFAPI-I-CRITERION-1.
           MOVE CRT-JOB-USER           TO WFAPI-I-CRITERION-2.
           MOVE CRT-JOB-NAME           TO WFAPI-I-CRITERION-3.
           PERFORM 1000-WF-SERVICE.
           IF (WFAPI-O-RETURN-CODE = ZEROS)
              INITIALIZE WFAPI-INPUT
              MOVE WFAPI-O-SERVICE     TO WFAPI-I-SERVICE
              MOVE WFAPI-O-AGENT       TO WFAPI-I-AGENT
              MOVE WFAPI-O-PROCEDURE   TO WFAPI-I-PROCEDURE
              STRING CRT-PROGRAM-CODE "-"
                     CRT-JOB-USER     "-"
                     CRT-JOB-NAME     DELIMITED BY SIZE
                                      INTO WFAPI-I-WORK-TITLE
              INITIALIZE WFAPI-OUTPUT
              PERFORM 1000-WF-CREATE-SETUP.

       600-SECTION-END.
      ****************************************************************
       605-END-BROADCAST             SECTION 72.
      ****************************************************************
       605-START.
      
      *** THE FOLLOWING LINES OF CODE MUST BE PERFORMED ONCE FOR EACH
      *** PRINTFILE TO BE PUBLISHED.

           IF  (WFAPI-O-RETURN-CODE   NOT = ZEROS)
               GO TO 605-SECTION-END.

           IF (AR137WS-DO-BASE-BROADCAST  = "Y")
              INITIALIZE WFAPI-INPUT
              MOVE WFAPI-O-WORKUNIT        TO WFAPI-I-WORKUNIT
              MOVE AR137WS-PRINT-FILE-1    TO WFAPI-I-FILE-NAME
              MOVE 500                     TO CRT-MSG-NBR
              MOVE "GLCHN"                 TO CRT-ERROR-CAT
              PERFORM 790-GET-MSG
              MOVE CRT-MESSAGE             TO WFAPI-I-DESCRIPTION
              MOVE "REPORT"                TO WFAPI-I-DOCUMENT-ID
              INITIALIZE WFAPI-OUTPUT
              PERFORM 1000-WF-ADD-FOLDER-SETUP
           END-IF.

      *** THE FOLLOWING LINES OF CODE MUST BE PERFORMED ONCE PER 
      *** EXECUTION OF THE PROGRAM AFTER ALL WRITING OF THE PRINTFILES.
      *** THIS WILL WILL RELEASE THE REPORT(S) TO BE PUBLISHED.

           INITIALIZE WFAPI-INPUT.
           MOVE WFAPI-O-WORKUNIT           TO WFAPI-I-WORKUNIT.
           INITIALIZE WFAPI-OUTPUT.
           PERFORM 1000-WF-RELEASE-SETUP.

       605-SECTION-END.
      
      ****************************************************************
       610-BEG-ELEMENT-TAG          SECTION 72.
      ****************************************************************
       610-START.
      
      *** END TAG
      *** THE FOLLOWING CODE SHOULD BE INSERTED FOR EACH LEVEL BREAK BEIN
      *** USED TO BURST THE REPORT IN BROADCAST (THREE LEVEL MAXIMUM)
      *** PROVIDE THE ELEMENT NAME AND VALUE. 
      
           IF (WFAPI-O-RETURN-CODE    NOT = ZEROS)
               GO TO 610-SECTION-END.
      
           MOVE AR137WS-ELEMENT-TYPE   TO CRT-MSG-NBR.
           MOVE "GLCHN"                TO CRT-ERROR-CAT.
           PERFORM 790-GET-MSG.
      
           MOVE CRT-MESSAGE            TO WFCHWS-ELEMENT.
           MOVE AR137WS-ELEMENT-VALUE  TO WFCHWS-VALUE.
           MOVE WFCHWS-BEG-TAG         TO WFCHWS-TAG-NAME.
           MOVE WFCHWS-TAG-STRING      TO CHN1-TAG-LINE.
           
           IF (AR137WS-DO-BASE-BROADCAST  = "Y")
               MOVE BROADCAST-TAG-LINE-R1   TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.
      
       610-SECTION-END.
      
      ****************************************************************
       615-END-ELEMENT-TAG           SECTION 72.
      ****************************************************************
       615-START.
      *** END TAG
      *** THE FOLLOWING CODE SHOULD BE INSERTED FOR EACH LEVEL BREAK BEIN
      *** USED TO BURST THE REPORT IN BROADCAST (THREE LEVEL MAXIMUM)
      *** PROVIDE THE ELEMENT NAME AND VALUE. 
      
           IF (WFAPI-O-RETURN-CODE    NOT = ZEROS)
               GO TO 615-SECTION-END.
      
           MOVE AR137WS-ELEMENT-TYPE   TO CRT-MSG-NBR.
           MOVE "GLCHN"                TO CRT-ERROR-CAT.
           PERFORM 790-GET-MSG.
      
           MOVE CRT-MESSAGE            TO WFCHWS-ELEMENT.
           MOVE AR137WS-ELEMENT-VALUE  TO WFCHWS-VALUE.
           MOVE WFCHWS-END-TAG         TO WFCHWS-TAG-NAME.
           MOVE WFCHWS-TAG-STRING      TO CHN1-TAG-LINE.
       
           IF (AR137WS-DO-BASE-BROADCAST  = "Y")
               MOVE BROADCAST-TAG-LINE-R1   TO RPT-GROUP-REQUEST
               PERFORM 700-PRINT-RPT-GRP.
      
       615-SECTION-END.
      
      ******************************************************************
       8100-RECREATE-AR137W1          SECTION.
      ******************************************************************
       8100-START.

           REWRITE AR137W1-REC            INVALID KEY
               MOVE WS-TRUE            TO AR137WS-FILE.

      ******************************************************************
       8400-READ-AR137W1              SECTION.
      ******************************************************************
       8400-START.

           MOVE WS-FALSE               TO AR137WS-FILE.

           READ AR137W1-FILE          KEY WF-AR137-KEY
               INVALID KEY
                   MOVE WS-TRUE        TO AR137WS-FILE.

      ******************************************************************
       8500-READ-NLT-AR137W1          SECTION.
      ******************************************************************
       8500-START.

           MOVE WS-FALSE               TO AR137WS-FILE.

           START AR137W1-FILE   KEY NOT < WF-AR137-KEY
               INVALID KEY
                   MOVE WS-TRUE        TO AR137WS-FILE.

           IF (AR137-FOUND)
               PERFORM 8600-READ-NEXT-AR137W1.

      ******************************************************************
       8600-READ-NEXT-AR137W1         SECTION.
      ******************************************************************
       8600-START.

           MOVE WS-FALSE               TO AR137WS-FILE.

           READ AR137W1-FILE NEXT RECORD
               AT END
                   MOVE WS-TRUE        TO AR137WS-FILE.

      ******************************************************************
       8300-DELETE-AR137W1                  SECTION.
      ******************************************************************
       8300-START.

           DELETE AR137W1-FILE RECORD    INVALID KEY

               MOVE WS-FALSE          TO AR137WS-FILE.

      ******************************************************************

