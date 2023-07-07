******* HRHDBPD 1.28.1.8 <184707185>
      ******************************************************************
      *                            HRHDBPD                             *
      ******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  311494 | J11494 | Audit Trail for processing of pending de-   *
      *         |        | pendent benefits.                           *
      *  ------  -------   ------------------------------------------  *
      *  548549 | J48549 | Fix change on dependent's HIPAA reason      *
      *         |        | creating blank effective date on BN53.      *
      *  ------  -------   ------------------------------------------  *
      * 1257096 | 257096 | Populate USER-ID from HRHDB-CREATE-USER-ID  *
      *         |        | when data was loaded in BNBATCH only.       *
      * -------  -------   ------------------------------------------  *
      * 1587700 | 587700 | SKIP READ TO BENEFIT TABLE FOR HR13.5       *
      * -------  -------   ------------------------------------------  *
ACS002******************************************************************
ACS002*CHANGE LOG:
ACS001*
ACS001* 12/09/2010 KShields  Changed to used default stop date for Change
ACS001*                      as well as Add FC
ACS002* 03/18/2011 MHUNTER   MODIFIED TO WRITE RECORD TO ZN325WK1 FILE
ACS002*                      WHEN HRDEPBEN RECORD IS DELETED.  FILE IS 
ACS002*                      USED FOR REPORTING IN THE ZN325 PROGRAM.
      * 09/15/2011 MHUNTER   REAPPLIED ALL ABOVE MODS AFTER 9.0.1 APPS
      *                      UPGRADE.
ACS003* 11/29/2011 M. HUNTER MODIFIED TO ONLY WRITE TO ZN325WK1 WHEN ESMS
ACS003*                      BENEFIT RECORD IS DELETED.
000100******************************************************************
000200*                             HRHDB                              *
000300******************************************************************
000400******************************************************************
000500 2000-HRHDB-EDIT-TRAN            SECTION.         
000600******************************************************************
000700 2000-START.  
000800
000900     MOVE "HRHDB"                        TO CRT-ERROR-CAT.
001000     MOVE  ZERO                          TO CRT-ERROR-NBR.

           MOVE CRT-PROGRAM-CODE           TO HRHDB-HIPAA-DEFAULT-SW.
001100
001200     PERFORM 2100-HRHDB-EDIT-ACCESS
001300     THRU    2100-END.
001400
001500      IF (ERROR-FOUND)
001600          GO TO 2000-HRHDB-RESET-CAT.
001700
J10377     IF (HRHDB-FC = "C" OR "D")
001900         PERFORM 7000-HRHDB-DEFAULT.
002000
002100     IF (HRHDB-FC = "A" OR "C")
002200         PERFORM 2300-EDIT-DATA
002300         THRU    2300-END
002400         GO TO   2000-HRHDB-RESET-CAT.
002500
002100     IF (HRHDB-FC = "D")
P45271         MOVE HRHDB-COMPANY          TO DB-COMPANY
P45271         MOVE HRHDB-PLAN-TYPE        TO DB-PLAN-TYPE
P45271         MOVE HRHDB-PLAN-CODE        TO DB-PLAN-CODE
P45271         PERFORM 840-FIND-PLNSET1
P45271
               PERFORM 2310-EDIT-HIPAA
               THRU    2310-END

002400         GO TO   2000-HRHDB-RESET-CAT.

002600 2000-HRHDB-RESET-CAT.
002700
002800     IF (NO-ERROR-FOUND)
002900         MOVE SPACES                     TO CRT-ERROR-CAT.
003000
003100     GO TO 2000-END.
003200
003300******************************************************************
003400 2100-HRHDB-EDIT-ACCESS.
003500******************************************************************
003600
003700     MOVE HRHDB-COMPANY              TO DB-COMPANY.
003800     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
003900     MOVE HRHDB-DEPENDENT            TO DB-DEPENDENT.
004000     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
004100     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
004200     MOVE HRHDB-EMP-START            TO DB-EMP-START.
004300     MOVE HRHDB-START-DATE           TO DB-START-DATE.
004400
004500     PERFORM 840-FIND-HDBSET1.
004600
004700     IF  (HRDEPBEN-NOTFOUND)
004800     AND (HRHDB-FC NOT = "A")
004900         MOVE 105                             TO CRT-ERROR-NBR
005000         MOVE HRHDB-FC-FN                     TO CRT-FIELD-NBR
005100         GO TO 2100-END.
005200
005300     IF  (HRDEPBEN-FOUND)
005400     AND (HRHDB-FC     = "A")
005500         MOVE 106                             TO CRT-ERROR-NBR
005600         MOVE HRHDB-FC-FN                     TO CRT-FIELD-NBR
005700         GO TO 2100-END.
005800
005900 2100-END. 
006000
006100******************************************************************
006200 2300-EDIT-DATA.
006300******************************************************************
006400
006500     MOVE HRHDB-COMPANY          TO DB-COMPANY.
006600     MOVE HRHDB-EMPLOYEE         TO DB-EMPLOYEE.
006700     MOVE HRHDB-DEPENDENT        TO DB-SEQ-NBR.
006800     PERFORM 840-FIND-EMDSET1.
006900     IF (EMDEPEND-NOTFOUND)
007000         MOVE 109                            TO CRT-ERROR-NBR
007100         MOVE HRHDB-FC-FN                    TO CRT-FIELD-NBR
007200         GO TO 2300-END.
007300
007400     IF (EMD-BIRTHDATE = ZEROES)
007500         MOVE 108                            TO CRT-ERROR-NBR
007600         MOVE HRHDB-FC-FN                    TO CRT-FIELD-NBR
007700         GO TO 2300-END.
007800
           IF (EMD-ACTIVE-FLAG NOT = "A")
               MOVE 119                            TO CRT-ERROR-NBR
               MOVE HRHDB-FC-FN                    TO CRT-FIELD-NBR
               GO TO 2300-END.

007900     IF  (HRHDB-START-DATE = ZEROES)
008000     AND (HRHDB-FC         NOT = SPACES)
008100         MOVE 101                            TO CRT-ERROR-NBR
008200         MOVE HRHDB-START-DATE-FN            TO CRT-FIELD-NBR
008300         GO TO 2300-END.
008400         
008500     IF  (EMD-DEP-TYPE = "D")
008600     AND (HRHDB-START-DATE < EMD-BIRTHDATE)
008700         MOVE 107                            TO CRT-ERROR-NBR
008800         MOVE HRHDB-START-DATE-FN            TO CRT-FIELD-NBR
008900         GO TO 2300-END.
009000
009100     IF  (HRHDB-START-DATE NOT = ZEROES)
009200     AND (HRHDB-START-DATE < HRHDB-BEN-START-DATE)
009300         MOVE 111                            TO CRT-ERROR-NBR
009400         MOVE HRHDB-START-DATE-FN            TO CRT-FIELD-NBR
009500         GO TO 2300-END.
009600
           IF  (HRHDB-START-DATE    NOT = ZEROES)
           AND (HRHDB-BEN-STOP-DATE NOT = ZEROES)
           AND (HRHDB-START-DATE    > HRHDB-BEN-STOP-DATE)
               MOVE 117                            TO CRT-ERROR-NBR
               MOVE HRHDB-START-DATE-FN            TO CRT-FIELD-NBR
               GO TO 2300-END.

009700     IF  (HRHDB-STOP-DATE     NOT = ZEROES)
009800     AND (HRHDB-BEN-STOP-DATE NOT = ZEROES)
009900     AND (HRHDB-STOP-DATE > HRHDB-BEN-STOP-DATE)
010000         MOVE 112                            TO CRT-ERROR-NBR
010100         MOVE HRHDB-STOP-DATE-FN             TO CRT-FIELD-NBR
010200         GO TO 2300-END.
010300
           IF  (HRHDB-START-DATE    NOT = ZEROES)
           AND (HRHDB-STOP-DATE    NOT = ZEROES)
           AND (HRHDB-STOP-DATE     < HRHDB-START-DATE)
               MOVE 118                            TO CRT-ERROR-NBR
               MOVE HRHDB-STOP-DATE-FN             TO CRT-FIELD-NBR
               GO TO 2300-END.

010400     IF  (HRHDB-STOP-DATE = ZEROES)
010500     AND (HRHDB-BEN-STOP-DATE NOT = ZEROES)
010600         MOVE 110                            TO CRT-ERROR-NBR
010700         MOVE HRHDB-STOP-DATE-FN             TO CRT-FIELD-NBR
010800         GO TO 2300-END.
010900
011000     MOVE HRHDB-COMPANY              TO DB-COMPANY.
011100     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
011200     MOVE HRHDB-PARTICIPNT           TO DB-PARTICIPNT.
011300     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
011400     MOVE HRHDB-EMP-START            TO DB-START-DATE.
011500     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
011600     PERFORM 840-FIND-PLNSET1.
011700     IF (HRHDB-COVER-TYPE            = "C" OR "R")
011800         IF (HRHDB-PARTICIPNT NOT = ZEROS)
011900             INITIALIZE DB-EMPLOYEE
012000         END-IF
012100         PERFORM 840-FIND-PTBSET1
012200     ELSE
587700         IF (CRT-SCREEN-CODE NOT= "HR135")
012300             PERFORM 840-FIND-BENSET1
587700         END-IF
587700     END-IF.
012400
           IF  (HRHDB-COVER-TYPE           = "C" OR "R")
               IF (PARTBEN-NOTFOUND)
                   MOVE 103                    TO CRT-ERROR-NBR
                   MOVE HRHDB-FC-FN            TO CRT-FIELD-NBR
                   GO TO 2300-END
               END-IF
           ELSE
               IF (BENEFIT-NOTFOUND)
                   MOVE 104                    TO CRT-ERROR-NBR
                   MOVE HRHDB-FC-FN            TO CRT-FIELD-NBR
                   GO TO 2300-END.

           IF  (HRHDB-COVER-TYPE    NOT = "C" OR "R")
               IF  (HRHDB-STOP-DATE     NOT = ZEROES)
               AND (BEN-STOP-DATE       NOT = ZEROES)
               AND (HRHDB-STOP-DATE     > BEN-STOP-DATE)
                   MOVE 112                    TO CRT-ERROR-NBR
                   MOVE HRHDB-STOP-DATE-FN     TO CRT-FIELD-NBR
                   GO TO 2300-END
               END-IF
           ELSE
               IF  (HRHDB-STOP-DATE     NOT = ZEROES)
               AND (PTB-STOP-DATE       NOT = ZEROES)
               AND (HRHDB-STOP-DATE     > PTB-STOP-DATE)
                   MOVE 112                    TO CRT-ERROR-NBR
                   MOVE HRHDB-STOP-DATE-FN     TO CRT-FIELD-NBR
                   GO TO 2300-END.


           IF  (HRHDB-COVER-TYPE    NOT = "C" OR "R")
               IF  (HRHDB-START-DATE    NOT = ZEROES)
               AND (HRHDB-START-DATE    < BEN-START-DATE)
                   MOVE 111                    TO CRT-ERROR-NBR
                   MOVE HRHDB-START-DATE-FN    TO CRT-FIELD-NBR
                   GO TO 2300-END
               END-IF
           ELSE
               IF  (HRHDB-START-DATE    NOT = ZEROES)
               AND (HRHDB-START-DATE    < PTB-START-DATE)
                   MOVE 111                    TO CRT-ERROR-NBR
                   MOVE HRHDB-START-DATE-FN    TO CRT-FIELD-NBR
                   GO TO 2300-END.

           SET WS-EDITS-ON              TO TRUE.

           PERFORM 6000-CALC-DEP-AGE-DATE.
           IF ((ERROR-FOUND)
587700     AND (CRT-SCREEN-CODE NOT= "HR135")
587700     AND (CRT-ERROR-NBR   NOT= 114 AND 116))
               GO TO 2300-END.

           PERFORM 2310-EDIT-HIPAA
           THRU    2310-END.

018800 2300-END.
018900
067500******************************************************************
       2310-EDIT-HIPAA.
067500******************************************************************

           IF  (HRHDB-CREATE-TRANS         = SPACES)
           OR  ((HRHDB-CREATE-TRANS        = "Y")
           AND  (HRHDB-HIPAA-DEFAULT))
               MOVE PLN-CREATE-TRANS       TO HRHDB-CREATE-TRANS.

           IF  (HRHDB-CREATE-TRANS         = "Y")
           AND (HRHDB-HIPAA-DEFAULT)
               MOVE HRHDB-HIPAA-REASON     TO HRHDB-REASON.

           IF  (PLN-CREATE-TRANS           = "N")
           AND (HRHDB-CREATE-TRANS         = "Y")
      ******** Plan not defined for HIPAA transactions
               MOVE 126                                 TO CRT-ERROR-NBR
               MOVE HRHDB-CREATE-TRANS-FN               TO CRT-FIELD-NBR
               GO TO 2310-END.

           IF (HRHDB-CREATE-TRANS          = "N")
               IF (HRHDB-REASON            NOT = SPACES)
      ************ Do not enter reason if create transaction = "N"
                   MOVE 120                             TO CRT-ERROR-NBR
                   MOVE HRHDB-REASON-FN                 TO CRT-FIELD-NBR
                   GO TO 2310-END
               END-IF
               IF (HRHDB-MEMBER-ID         NOT = ZEROES)
      ************ Do not specify member id if create transaction = "N"
                   MOVE 121                             TO CRT-ERROR-NBR
                   MOVE HRHDB-MEMBER-ID-FN              TO CRT-FIELD-NBR
                   GO TO 2310-END.

           IF (HRHDB-CREATE-TRANS          = "Y")
               IF (HRHDB-REASON            = SPACES)
      ************ Must enter reason if create transaction = "Y"
                   MOVE 122                             TO CRT-ERROR-NBR
                   MOVE HRHDB-REASON-FN                 TO CRT-FIELD-NBR
                   GO TO 2310-END
               END-IF
               IF (HRHDB-MEMBER-ID         = ZEROES)
                   MOVE PLN-MEMBER-ID      TO HRHDB-MEMBER-ID.

           IF (HRHDB-REASON                NOT = SPACES)
               MOVE HRHDB-BT               TO DB-TYPE
               MOVE HRHDB-REASON           TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-NOTFOUND)
      ************ Reason code does not exist
                   MOVE 124                             TO CRT-ERROR-NBR
                   MOVE HRHDB-REASON-FN                 TO CRT-FIELD-NBR
                   GO TO 2310-END.

           IF  (HRHDB-MEMBER-ID            NOT = ZEROES)
           AND (HRHDB-MEMBER-ID            NOT = 1 AND 2)
      ******** Invalid member id
               MOVE 125                                 TO CRT-ERROR-NBR
               MOVE HRHDB-MEMBER-ID-FN                  TO CRT-FIELD-NBR
               GO TO 2310-END.

       2310-END.

019000******************************************************************
019100 2000-END.
019200******************************************************************
019300******************************************************************
019400 3000-HRHDB-PROCESS-TRAN             SECTION.
019500******************************************************************
019600 3000-START.
019700
019800* THE "S" FC ORIGINATES FROM THE EMPLOYEE BENEFIT PROGRAMS.
019900* WHEN EMPLOYEE STOPS A BENEFIT, DEFAULT TO DEPENDENT BENEFITS.
020000
020100* THE "R" FC ORIGINATES FROM THE PLAN TRANSFER PROGRAM.
020200* NEEDS TO CHECK DEPENDENT FOR POSSIBLE STOP DATE.
020300
           MOVE CRT-PROGRAM-CODE           TO HRHDB-HIPAA-DEFAULT-SW.
001100
020400     IF (HRHDB-FC = "S")
020500         PERFORM 3050-STOP-DATE
020600         THRU    3050-END
020700     ELSE
020800     IF (HRHDB-FC = "A" OR "R")
020900         PERFORM 3100-ADD
021000         THRU    3100-END
021100     ELSE
021200     IF (HRHDB-FC = "C")
021300         PERFORM 3200-CHANGE
021400         THRU    3200-END
021500     ELSE
021600     IF (HRHDB-FC = "D")
021700         PERFORM 3300-DELETE
021800         THRU    3300-END. 
021900
020400     IF (HRHDB-FC = "S")
               GO TO 3000-END.

           IF  (HRHDB-CREATE-TRANS         = SPACES)
           OR  ((HRHDB-CREATE-TRANS        = "Y")
           AND  (HRHDB-HIPAA-DEFAULT))
               PERFORM 5200-DEFAULT-HIPAA-FIELDS.

           IF (HRHDB-CREATE-TRANS          = "Y")
               PERFORM 5100-CREATE-HRUTILITY.

022000     GO TO 3000-END.
022100
022200******************************************************************
022300 3050-STOP-DATE.
022400******************************************************************
022500
           INITIALIZE HRHDBWS-SV-DEP
                      HRHDBWS-FUTURE-HDB-TBL.

           MOVE HRHDB-COMPANY              TO DB-COMPANY.
           MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
           MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
           MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
           MOVE HRHDB-BEN-START-DATE       TO DB-EMP-START.
           MOVE HDBSET3-EMP-START          TO WS-DB-BEG-RNG.
           PERFORM 850-FIND-BEGRNG-HDBSET3.
           IF (HRDEPBEN-FOUND)
           AND (HRHDB-PARTICIPNT = HRHDB-PARTICIPNT)
               MOVE HDB-DEPENDENT          TO HRHDBWS-SV-DEP
               PERFORM
                   VARYING I5 FROM 1 BY 1
                   UNTIL (HRDEPBEN-NOTFOUND)
                   OR    (I5 > 30)

                   PERFORM 860-FIND-NXTRNG-HDBSET3

                   IF  (HRDEPBEN-FOUND)
                   AND (HDB-DEPENDENT = HRHDBWS-SV-DEP)
                       MOVE "Y" TO HRHDBWS-FUT-HDB (I5)
                   END-IF
                   IF  (HRDEPBEN-FOUND)
                       MOVE HDB-DEPENDENT  TO HRHDBWS-SV-DEP.

022600     MOVE HRHDB-COMPANY              TO DB-COMPANY.
022700     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
022800     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
022900     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
023000     MOVE HRHDB-BEN-START-DATE       TO DB-EMP-START.
023100     MOVE HDBSET3-EMP-START          TO WS-DB-BEG-RNG.
023200     PERFORM 850-MODIFY-BEGRNG-HDBSET3.
023300     PERFORM
               VARYING I5 FROM 1 BY 1
023400         UNTIL (HRDEPBEN-NOTFOUND)
               OR    (I5 > 30)
023500         IF (HRHDB-PARTICIPNT NOT = HDB-PARTICIPNT)
                   CONTINUE
               ELSE
                   IF  (CRT-PROGRAM-CODE = 
                                "BN100" OR "BN102" OR "BN105")
                   AND (HDB-STOP-DATE    = HRHDB-BEN-STOP-DATE)
                       CONTINUE
                   ELSE
023600                 PERFORM 3060-DEFAULT-STOP-DATE
023700                 THRU    3060-END
023800
                       IF  (HRHDB-CREATE-TRANS     = SPACES)
                       OR  ((HRHDB-CREATE-TRANS    = "Y")
                       AND  (HRHDB-HIPAA-DEFAULT))
                           PERFORM 5200-DEFAULT-HIPAA-FIELDS
                       END-IF

                       IF (HRHDB-CREATE-TRANS      = "Y")
                           MOVE HDB-DEPENDENT      TO HRHDB-DEPENDENT
                           MOVE HDB-START-DATE     TO HRHDB-START-DATE
                           MOVE HDB-STOP-DATE      TO HRHDB-STOP-DATE
J46436                     IF (HRHDB-HIPAA-REASON NOT = SPACES)
P82128                        MOVE HRHDB-HIPAA-REASON TO HRHDB-REASON
J46436                     END-IF
P82128 
                           PERFORM 5100-CREATE-HRUTILITY
                       END-IF
                   END-IF
               END-IF
023900         PERFORM 860-MODIFY-NXTRNG-HDBSET3
024000
024100     END-PERFORM.
024200
024300 3050-END.
024400******************************************************************
024500 3060-DEFAULT-STOP-DATE.
024600******************************************************************
024700
024800     MOVE HDB-COMPANY                TO DB-COMPANY.
024900     MOVE HDB-EMPLOYEE               TO DB-EMPLOYEE.
025000     MOVE HDB-DEPENDENT              TO DB-SEQ-NBR.
025100     PERFORM 840-FIND-EMDSET1.
025200
P47004     MOVE HDB-PLAN-TYPE              TO DB-PLAN-TYPE.
P47004     MOVE HDB-PLAN-CODE              TO DB-PLAN-CODE.
P47004     PERFORM 840-FIND-PLNSET1.
P47004
P47004     MOVE HRHDB-PARTICIPNT           TO DB-PARTICIPNT.
P47004     MOVE HDB-EMPLOYEE               TO DB-EMPLOYEE.
P47004     MOVE HDB-EMP-START              TO DB-START-DATE.
P47004     PERFORM 840-FIND-PLNSET1.
P47004     IF (HRHDB-COVER-TYPE            = "C" OR "R")
P47004         IF (HRHDB-PARTICIPNT NOT = ZEROS)
P47004             INITIALIZE DB-EMPLOYEE
P47004         END-IF
P47004         PERFORM 840-FIND-PTBSET1
P47004     ELSE
P47004         PERFORM 840-FIND-BENSET1.
P47004
           IF  (CRT-PROGRAM-CODE = "BN105" OR "BN102" OR "BN100")
           AND (EMDEPEND-FOUND)
           AND (HDB-STOP-DATE > HRHDB-BEN-STOP-DATE)
               ADD 1                       TO I4
               IF (I4 <= 25)
                   MOVE HDB-DEPENDENT      TO HRHDB-SV-SPOUSE (I4)
                   MOVE HDB-STOP-DATE      TO HRHDB-SV-SP-END-DT (I4)
                   MOVE HDB-PLAN-TYPE      TO HRHDB-SV-SP-PLN-TP (I4)
                   MOVE HDB-PLAN-CODE      TO HRHDB-SV-SP-PLN-CD (I4).

025300     IF  (HDB-STOP-DATE            = ZEROES)
025400     OR  ((HDB-STOP-DATE       NOT = ZEROES)
025500     AND (HRHDB-BEN-STOP-DATE      < HDB-STOP-DATE)
P81935     AND (HRHDB-BEN-STOP-DATE  NOT = ZEROES))
      *    AND (HRHDBWS-FUT-HDB (I5) NOT = "Y"))
      ******** If coming from following batch programs and benefit is
      ******** stopped and added, we don't want to delete future dated
      ******** HRDEPBEN. We will change EmpStart field on these benefits
      ******** to the benefit that is added on next day in these batch
      ******** batch programs
               IF  (CRT-PROGRAM-CODE = "BN100" OR "BN102" OR "BN105")
               AND (HRHDBWS-BATCH-FC = "CS")
               AND (HDB-START-DATE   > HRHDB-BEN-STOP-DATE)
025800             GO TO 3060-END
               ELSE
025600             MOVE HRHDB-BEN-STOP-DATE    TO HDB-STOP-DATE
               END-IF
025700     ELSE
025800         GO TO 3060-END.
025900
026000     IF  (HDB-STOP-DATE < HDB-START-DATE)
           AND (HDB-STOP-DATE NOT = ZEROES)
026100         PERFORM 830-DELETE-HRDEPBEN
ACS003*        IF (HRHDB-PLAN-CODE = "ESMS" OR "ESMX")
ACS003         IF (HRHDB-PLAN-CODE = "ESMS")
ACS002             SET HRDEPBEN-DELETED    TO TRUE
ACS002         END-IF
026200         GO TO 3060-END.
026300
026400     MOVE HRHDB-COMPANY              TO DB-COMPANY.
026500     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
026600     MOVE HRHDB-PARTICIPNT           TO DB-PARTICIPNT.
026700     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
026800     MOVE HRHDB-EMP-START            TO DB-START-DATE.
026900     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
027000     MOVE HRHDB-COVER-TYPE           TO DB-COVER-TYPE.
027100
           SET WS-UPDATES-ON               TO TRUE.

           PERFORM 6000-CALC-DEP-AGE-DATE.

P34548*--- Fix for PT134548, If the new Stop Date calculated is prior
P34548*--- to Start Date, make Stop date equal to Start Date.
P34548     IF  (HDB-STOP-DATE          < HDB-START-DATE)
P44261     AND (HDB-STOP-DATE          NOT = ZEROES)
P34548         MOVE HDB-START-DATE     TO HDB-STOP-DATE.

           MOVE WS-SYSTEM-DATE-YMD     TO HDB-UPD-DATE.
           MOVE HHMMSS                 TO HDB-TIME-STAMP.

           IF (CRT-PROGRAM-CODE        = "BN100")
           OR (CRT-PROGRAM-CODE        = "BN101")
           OR (CRT-PROGRAM-CODE        = "BN102")
           OR (CRT-PROGRAM-CODE        = "BN103")
           OR (CRT-PROGRAM-CODE        = "BN104")
           OR (CRT-PROGRAM-CODE        = "BN105")
J11494*    OR (CRT-PROGRAM-CODE        = "BN532")
               MOVE CRT-PROGRAM-CODE   TO HDB-USER-ID
           ELSE
           IF (HRHDB-USER-ID           NOT = SPACES)
               MOVE HRHDB-USER-ID      TO HDB-USER-ID
           ELSE
               MOVE CRT-USER-NAME      TO HDB-USER-ID.

J23201     IF  (CRT-PROGRAM-CODE           = "BN532")
257096     AND (HRHDB-CREATE-USER-ID   NOT = SPACES)
J23201         MOVE HRHDB-CREATE-USER-ID  TO HDB-USER-ID
J23201     END-IF.

032100     PERFORM 820-STORE-HRDEPBEN.
032200
032300 3060-END.
032400******************************************************************
032500 3100-ADD.
032600******************************************************************
032700
032800     MOVE HRHDB-COMPANY              TO DB-COMPANY.
032900     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
033000     MOVE HRHDB-DEPENDENT            TO DB-DEPENDENT.
033100     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
033200     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
033300     MOVE HRHDB-EMP-START            TO DB-EMP-START.
033400     MOVE HRHDB-START-DATE           TO DB-START-DATE.
033500
033600     PERFORM 850-MODIFY-NLT-HDBSET4.
033700
033800     IF  (HRDEPBEN-FOUND)
033900     AND (HDB-COMPANY  = DB-COMPANY)
034000     AND (HDB-EMPLOYEE  = DB-EMPLOYEE)
034100     AND (HDB-DEPENDENT = DB-DEPENDENT)
034200     AND (HDB-PLAN-TYPE = DB-PLAN-TYPE)
034300     AND (HDB-PLAN-CODE = DB-PLAN-CODE)
034300     AND (HDB-EMP-START = DB-EMP-START)
034400     AND (HDB-STOP-DATE = ZEROES)
034500         PERFORM 4010-STOP-EXISTING-HRDEPBEN
034600         THRU    4010-END.
034700
034800     PERFORM 800-CREATE-HRDEPBEN.
034900
035000     IF (HRHDB-FC = "R")
           OR (HRHDB-STOP-DATE = ZEROES)
035100         PERFORM 4000-DEFAULT-STOP-DATE
035200         THRU    4000-END.
035300
035400     PERFORM 5000-MOVE-DATA
035500     THRU    5000-END.
035600
035700     PERFORM 820-STORE-HRDEPBEN.
035800
035900     MOVE CRT-HILITE                 TO HRHDB-HILIGHT.
036000
036100 3100-END.
036200
036300******************************************************************
036400 3200-CHANGE.
036500******************************************************************
036600
           IF (HRHDB-STOP-DATE = ZEROES)
               PERFORM 4000-DEFAULT-STOP-DATE
               THRU    4000-END.

036700     MOVE HRHDB-COMPANY              TO DB-COMPANY.
036800     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
036900     MOVE HRHDB-DEPENDENT            TO DB-DEPENDENT.
037000     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
037100     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
037200     MOVE HRHDB-EMP-START            TO DB-EMP-START.
037300     MOVE HRHDB-START-DATE           TO DB-START-DATE.
037400
037500     PERFORM 840-MODIFY-HDBSET1.
037600
037700     PERFORM 5000-MOVE-DATA
037800     THRU    5000-END.
037900
038000     PERFORM 820-STORE-HRDEPBEN.
038100
038200     MOVE CRT-HILITE                 TO HRHDB-HILIGHT.
038300
038400 3200-END.
038500
038600******************************************************************
038700 3300-DELETE.
038800******************************************************************
038900
039000     MOVE HRHDB-COMPANY              TO DB-COMPANY.
039100     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
039200     MOVE HRHDB-DEPENDENT            TO DB-DEPENDENT.
039300     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
039400     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
039500     MOVE HRHDB-EMP-START            TO DB-EMP-START.
039600     MOVE HRHDB-START-DATE           TO DB-START-DATE.
039700
039800     PERFORM 840-MODIFY-HDBSET1.
039900
040000     PERFORM 830-DELETE-HRDEPBEN.
ACS003*    IF (HRHDB-PLAN-CODE = "ESMS" OR "ESMX")
ACS003     IF (HRHDB-PLAN-CODE = "ESMS")
ACS002         SET HRDEPBEN-DELETED        TO TRUE.
040100
040200 3300-END.
040300
040400******************************************************************
040500 4000-DEFAULT-STOP-DATE.
040600******************************************************************
040700
040800     MOVE HRHDB-COMPANY          TO DB-COMPANY.
040900     MOVE HRHDB-EMPLOYEE         TO DB-EMPLOYEE.
041000     MOVE HRHDB-DEPENDENT        TO DB-SEQ-NBR.
041100     PERFORM 840-FIND-EMDSET1.
041200
P47004     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
P47004     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
P47004     PERFORM 840-FIND-PLNSET1.
P47004
P47004     MOVE HRHDB-PARTICIPNT           TO DB-PARTICIPNT.
P47004     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
P47004     MOVE HRHDB-EMP-START            TO DB-START-DATE.
P47004     PERFORM 840-FIND-PLNSET1.
P47004     IF (HRHDB-COVER-TYPE            = "C" OR "R")
P47004         IF (HRHDB-PARTICIPNT NOT = ZEROS)
P47004             INITIALIZE DB-EMPLOYEE
P47004         END-IF
P47004         PERFORM 840-FIND-PTBSET1
P47004     ELSE
P47004         PERFORM 840-FIND-BENSET1.
P47004
041300     MOVE HRHDB-COMPANY              TO DB-COMPANY.
041400     MOVE HRHDB-PLAN-TYPE            TO DB-PLAN-TYPE.
041500     MOVE HRHDB-PARTICIPNT           TO DB-PARTICIPNT.
041600     MOVE HRHDB-EMPLOYEE             TO DB-EMPLOYEE.
041700     MOVE HRHDB-EMP-START            TO DB-START-DATE.
041800     MOVE HRHDB-PLAN-CODE            TO DB-PLAN-CODE.
041900     MOVE HRHDB-COVER-TYPE           TO DB-COVER-TYPE.
P59156
P59156     PERFORM 840-FIND-BENSET1.
P59156     PERFORM 840-FIND-PTBSET1.
042000
           SET WS-UPDATES-ON               TO TRUE.

           PERFORM 6000-CALC-DEP-AGE-DATE.

           IF (HRHDB-COVER-TYPE = "C" OR "R")
               IF  (PTB-STOP-DATE     NOT = ZEROES)
               AND (HRHDB-STOP-DATE   > PTB-STOP-DATE)
               OR ((PTB-STOP-DATE     NOT = ZEROES)
               AND (HRHDB-STOP-DATE   = ZEROES))
                   MOVE PTB-STOP-DATE TO HRHDB-STOP-DATE
               END-IF
           ELSE
               IF  (BEN-STOP-DATE     NOT = ZEROES)
               AND (HRHDB-STOP-DATE   > BEN-STOP-DATE)
               OR ((BEN-STOP-DATE     NOT = ZEROES)
               AND (HRHDB-STOP-DATE   = ZEROES))
                   MOVE BEN-STOP-DATE TO HRHDB-STOP-DATE.
046700
046800 4000-END.
046900
047000******************************************************************
047100 4010-STOP-EXISTING-HRDEPBEN.
047200******************************************************************
047300
047400     MOVE HRHDB-START-DATE       TO WSDR-FR-DATE.
047500     PERFORM 900-DATE-TO-JULIAN.
047600     SUBTRACT 1                  FROM WSDR-JULIAN-DAYS.
047700     PERFORM 900-JULIAN-TO-DATE.
047800     MOVE WSDR-FR-DATE           TO HDB-STOP-DATE.
047900
           MOVE WS-SYSTEM-DATE-YMD     TO HDB-UPD-DATE.
           MOVE HHMMSS                 TO HDB-TIME-STAMP.
           IF (CRT-PROGRAM-CODE        = "BN100")
           OR (CRT-PROGRAM-CODE        = "BN101")
           OR (CRT-PROGRAM-CODE        = "BN102")
           OR (CRT-PROGRAM-CODE        = "BN103")
           OR (CRT-PROGRAM-CODE        = "BN104")
           OR (CRT-PROGRAM-CODE        = "BN105")
J11494*    OR (CRT-PROGRAM-CODE        = "BN532")
               MOVE CRT-PROGRAM-CODE   TO HDB-USER-ID
           ELSE
           IF (HRHDB-USER-ID           NOT = SPACES)
               MOVE HRHDB-USER-ID      TO HDB-USER-ID
           ELSE
               MOVE CRT-USER-NAME      TO HDB-USER-ID.

J23201    IF  (CRT-PROGRAM-CODE           = "BN532")
257096    AND (HRHDB-CREATE-USER-ID   NOT = SPACES)
J23201        MOVE HRHDB-CREATE-USER-ID  TO HDB-USER-ID
J23201    END-IF.

048000     PERFORM 820-STORE-HRDEPBEN.
048100 
048200 4010-END.
048300
048400******************************************************************
048500 5000-MOVE-DATA.
048600******************************************************************
048700
048800     MOVE HRHDB-COMPANY          TO HDB-COMPANY.
048900     MOVE HRHDB-EMPLOYEE         TO HDB-EMPLOYEE.
049000     MOVE HRHDB-PLAN-TYPE        TO HDB-PLAN-TYPE.
049100     MOVE HRHDB-PLAN-CODE        TO HDB-PLAN-CODE.
049200     MOVE HRHDB-EMP-START        TO HDB-EMP-START.
049300     MOVE HRHDB-DEPENDENT        TO HDB-DEPENDENT.
049400     MOVE HRHDB-START-DATE       TO HDB-START-DATE.
049500     MOVE HRHDB-STOP-DATE        TO HDB-STOP-DATE.
049600
           IF (HRHDB-FC                     = "A")
               IF (HRHDB-CREATION-DATE      NOT = ZEROES)
                   MOVE HRHDB-CREATION-DATE TO HDB-CREATION-DATE
               ELSE
                   MOVE WS-SYSTEM-DATE-YMD  TO HDB-CREATION-DATE
               END-IF
               
J67329         IF (HRHDB-TIME-STAMP      NOT = ZEROES)
J67329             MOVE HRHDB-TIME-STAMP    TO HDB-CREATE-TIME
J67329         ELSE
J67329             MOVE HHMMSS              TO HDB-CREATE-TIME
J67329         END-IF
               

J67329         IF (CRT-PROGRAM-CODE        = "BN100")
J67329         OR (CRT-PROGRAM-CODE        = "BN101")
J67329         OR (CRT-PROGRAM-CODE        = "BN102")
J67329         OR (CRT-PROGRAM-CODE        = "BN103")
J67329         OR (CRT-PROGRAM-CODE        = "BN104")
J67329         OR (CRT-PROGRAM-CODE        = "BN105")
J67329             MOVE CRT-PROGRAM-CODE    TO HDB-CREATE-USER-ID
J67329         ELSE
J67329             IF (HRHDB-USER-ID   NOT = SPACES)
J67329                 MOVE HRHDB-USER-ID   TO HDB-CREATE-USER-ID
J67329             ELSE
J67329                 MOVE CRT-USER-NAME   TO HDB-CREATE-USER-ID
J67329             END-IF
J67329         END-IF                         
J23201         IF  (CRT-PROGRAM-CODE           = "BN532")
257096         AND (HRHDB-CREATE-USER-ID   NOT = SPACES)
J23201             MOVE HRHDB-CREATE-USER-ID  TO HDB-CREATE-USER-ID
J23201         END-IF
           END-IF.

           IF (HRHDB-UPD-DATE          NOT = ZEROES)
               MOVE HRHDB-UPD-DATE     TO HDB-UPD-DATE
           ELSE
               MOVE WS-SYSTEM-DATE-YMD TO HDB-UPD-DATE
           END-IF.

           IF (HRHDB-TIME-STAMP        NOT = ZEROES)
               MOVE HRHDB-TIME-STAMP   TO HDB-TIME-STAMP
           ELSE
               MOVE HHMMSS             TO HDB-TIME-STAMP
           END-IF.

           IF (CRT-PROGRAM-CODE        = "BN100")
           OR (CRT-PROGRAM-CODE        = "BN101")
           OR (CRT-PROGRAM-CODE        = "BN102")
           OR (CRT-PROGRAM-CODE        = "BN103")
           OR (CRT-PROGRAM-CODE        = "BN104")
           OR (CRT-PROGRAM-CODE        = "BN105")
J11494*    OR (CRT-PROGRAM-CODE        = "BN532")
               MOVE CRT-PROGRAM-CODE   TO HDB-USER-ID
           ELSE
           IF (HRHDB-USER-ID           NOT = SPACES)
               MOVE HRHDB-USER-ID      TO HDB-USER-ID
           ELSE
               MOVE CRT-USER-NAME      TO HDB-USER-ID.
J23201     IF  (CRT-PROGRAM-CODE           = "BN532")
257096     AND (HRHDB-CREATE-USER-ID   NOT = SPACES)
J23201         MOVE HRHDB-CREATE-USER-ID  TO HDB-USER-ID
J23201     END-IF.
               
J10377     MOVE HRHDB-PARTICIPNT       TO HDB-PARTICIPNT.

049700 5000-END.
049800
049900******************************************************************
050000 3000-END.
050100******************************************************************

194000******************************************************************
       5100-CREATE-HRUTILITY               SECTION.
194000******************************************************************
       5100-START.

027800     MOVE HRHDB-COMPANY              TO BNSNWS-COMPANY.
027800     MOVE HRHDB-PLAN-TYPE            TO BNSNWS-PLAN-TYPE.
027800     MOVE HRHDB-PLAN-CODE            TO BNSNWS-PLAN-CODE.
027800     MOVE HRHDB-EMPLOYEE             TO BNSNWS-EMPLOYEE.
           INITIALIZE BNSNWS-EMPLOYEE
                      BNSNWS-PARTICIPNT.
           IF (HRHDB-COVER-TYPE            = "C")
               MOVE HRHDB-PARTICIPNT       TO BNSNWS-PARTICIPNT
           ELSE
               MOVE HRHDB-EMPLOYEE         TO BNSNWS-EMPLOYEE.
027800     MOVE HRHDB-DEPENDENT            TO BNSNWS-DEPENDENT.
027800     MOVE HRHDB-START-DATE           TO BNSNWS-START-DATE.
           IF (HRHDB-FC                    = "A" OR "R" OR "D")
               MOVE HRHDB-START-DATE       TO BNSNWS-EFFECT-DATE
           ELSE
P84295     IF (HRHDB-FC                    = "C" OR "S")
               MOVE HRHDB-STOP-DATE     TO BNSNWS-EFFECT-DATE.
           PERFORM 5000-GET-BNT-TRAN-SEQ-NBR.
           MOVE BNSNWS-TRAN-SEQ-NBR        TO HRHDB-TRAN-SEQ-NBR.

           PERFORM 800-CREATE-BNTRANS.

           MOVE HRHDB-COMPANY              TO BNT-COMPANY.
           MOVE HRHDB-PLAN-TYPE            TO BNT-PLAN-TYPE.
           MOVE HRHDB-PLAN-CODE            TO BNT-PLAN-CODE.

124100     IF (HRHDB-COVER-TYPE            = "C")
               MOVE HRHDB-PARTICIPNT       TO BNT-PARTICIPNT
           ELSE
               MOVE HRHDB-EMPLOYEE         TO BNT-EMPLOYEE.

           MOVE HRHDB-DEPENDENT            TO BNT-DEPENDENT.
           MOVE HRHDB-START-DATE           TO BNT-START-DATE.
           MOVE HRHDB-TRAN-SEQ-NBR         TO BNT-TRAN-SEQ-NBR.
           MOVE HRHDB-EMP-START            TO BNT-EMP-START.
           INITIALIZE BNT-EVENT-CODE.

           IF (HRHDB-COVER-TYPE            = "C")
               MOVE HRHDB-PARTICIPNT       TO DB-PARTICIPNT
               PERFORM 840-FIND-PARSET1

               MOVE PAR-COMPANY            TO DB-COMPANY
               MOVE PAR-OCCUR-TYPE         TO DB-OCCUR-TYPE
               PERFORM 840-FIND-OCCSET1
               IF (OCCURTYPE-FOUND)
                   MOVE OCC-EVENT-CODE     TO BNT-EVENT-CODE.

           MOVE HRHDB-COVER-TYPE           TO BNT-COVER-TYPE.
           MOVE 1                          TO BNT-TRAN-STATUS.
           MOVE HRHDB-FC                   TO BNT-TRAN-ACTION.
           IF (BNT-TRAN-ACTION             = "R")
               MOVE "A"                    TO BNT-TRAN-ACTION
           ELSE
               IF  (BNT-TRAN-ACTION        = "C")
J81265         AND (HRHDB-STOP-DATE    NOT = ZEROES)
                   MOVE "S"                TO BNT-TRAN-ACTION
J81265         END-IF
J81265     END-IF.
           MOVE HRHDB-REASON               TO BNT-TRAN-REASON.
           MOVE HRHDB-MEMBER-ID            TO BNT-MEMBER-ID.

           IF (BNT-TRAN-ACTION             = "A" OR "D")
               MOVE HRHDB-START-DATE       TO BNT-EFFECT-DATE
           ELSE
           IF (BNT-TRAN-ACTION             = "S")
               MOVE HRHDB-STOP-DATE        TO BNT-EFFECT-DATE
J48549     ELSE
J48549     IF  (BNT-TRAN-ACTION            = "C")
J48549     AND (HRHDB-STOP-DATE            = ZEROES)
J48549         MOVE HRHDB-START-DATE       TO BNT-EFFECT-DATE
J48549     END-IF.

           MOVE HRHDB-STOP-DATE            TO BNT-STOP-DATE.

           INITIALIZE BNT-HIPAA-FILE-NBR
                      BNT-FILE-DATE.

           MOVE WS-SYSTEM-DATE-YMD         TO BNT-DATE-STAMP.
           MOVE HHMMSS                     TO BNT-TIME-STAMP.

           IF (CRT-PROGRAM-CODE            = "BN100")
           OR (CRT-PROGRAM-CODE            = "BN101")
           OR (CRT-PROGRAM-CODE            = "BN102")
           OR (CRT-PROGRAM-CODE            = "BN103")
           OR (CRT-PROGRAM-CODE            = "BN105")
               MOVE CRT-PROGRAM-CODE       TO BNT-USER-ID
           ELSE
               MOVE CRT-USER-NAME          TO BNT-USER-ID.

           PERFORM 820-STORE-BNTRANS.

050100******************************************************************
       5100-END.
050100******************************************************************

194000******************************************************************
       5200-DEFAULT-HIPAA-FIELDS           SECTION.
194000******************************************************************
       5200-START.

           MOVE PLN-CREATE-TRANS           TO HRHDB-CREATE-TRANS.

P45271     IF (HRHDB-CREATE-TRANS          = SPACES)
P45271         MOVE "N"                    TO HRHDB-CREATE-TRANS.
P45271
           IF (HRHDB-CREATE-TRANS          = "Y")
               MOVE HRHDB-HIPAA-REASON     TO HRHDB-REASON
               MOVE PLN-MEMBER-ID          TO HRHDB-MEMBER-ID.

050100******************************************************************
       5200-END.
050100******************************************************************

050200******************************************************************
050300 6000-CALC-DEP-AGE-DATE          SECTION.
050400******************************************************************
050500 6000-START.
050600
           MOVE HRHDB-COVER-TYPE           TO HRDEP-COVER-TYPE.
           MOVE "Y"                        TO HRDEP-CALC-AGE-DATE.
           PERFORM 5110-FIND-DEP-N-STUD-AGE.
           IF (ERROR-FOUND)
               GO TO 6000-END
           ELSE
               MOVE HRDEP-DEP-AGE-DATE     TO HRHDBWS-DEP-AGE-DATE
               MOVE HRDEP-STUD-AGE-DATE    TO HRHDBWS-STUDENT-AGE-DATE
               MOVE HRDEP-DEP-END-DATE     TO HRHDBWS-DEP-END-DATE.

           IF (WS-EDITS-ON)
               GO TO 6000-EDITS.
J84266
J84266     IF  (EMD-DEP-TYPE = "D")
J84266     AND (EMD-DISABLED = "N")
J84266         IF  (EMD-STUDENT             = "N")
J84266         OR ((EMD-STUDENT             = "P")
J84266         AND (HRHDBWS-PT-STUDENTS NOT = "Y"))
J84266             MOVE HRHDBWS-DEP-AGE-DATE      TO HRHDB-STOP-DATE
J84266         END-IF
J84266     END-IF.
J84266
J84266     IF  (EMD-DEP-TYPE = "D")
J84266     AND (EMD-DISABLED = "N")
J84266         IF  (EMD-STUDENT         = "Y")
J84266         OR  (EMD-STUDENT         = "F")
J84266         OR ((EMD-STUDENT         = "P")
J84266         AND (HRHDBWS-PT-STUDENTS = "Y"))
J84266             MOVE HRHDBWS-STUDENT-AGE-DATE  TO HRHDB-STOP-DATE
J84266         END-IF
J84266     END-IF.

           IF  ((HRHDB-FC                   = "S")
030500     AND ((HRHDBWS-DEP-END-DATE      < HRHDB-BEN-STOP-DATE)
           OR   (HRHDB-BEN-STOP-DATE        = ZEROES)))
           OR   (HRHDB-FC                    = "A")
ACS001     OR  (HRHDB-FC                    = "C")
030600         MOVE HRHDBWS-DEP-END-DATE   TO HRHDB-STOP-DATE.

           GO TO 6000-END.

       6000-EDITS.
015800     IF (HRHDBWS-DEP-END-DATE        < HRHDB-START-DATE)
               IF (HRHDBWS-DEP-AGE-DATE    NOT = ZEROES)
015900             MOVE 113                TO CRT-ERROR-NBR
               ELSE
               IF (HRHDBWS-STUDENT-AGE-DATE NOT = ZEROES)
015900             MOVE 115                TO CRT-ERROR-NBR
               END-IF
               END-IF
016000         MOVE HRHDB-START-DATE-FN    TO CRT-FIELD-NBR
016100         GO TO 6000-END
016200     ELSE
016300     IF (HRHDBWS-DEP-END-DATE        < HRHDB-STOP-DATE)
               IF (HRHDBWS-DEP-AGE-DATE    NOT = ZEROES)
015900             MOVE 114                TO CRT-ERROR-NBR
               ELSE
               IF (HRHDBWS-STUDENT-AGE-DATE NOT = ZEROES)
015900             MOVE 116                TO CRT-ERROR-NBR
               END-IF
               END-IF
016500         MOVE HRHDB-STOP-DATE-FN     TO CRT-FIELD-NBR
016600         GO TO 6000-END.
018700
050100******************************************************************
       6000-END.
050100******************************************************************

050200******************************************************************
050300 7000-HRHDB-DEFAULT              SECTION.
050400******************************************************************
050500 7000-START.
050600
050700     IF (HRHDB-COMPANY-FN    = ZEROS)
050800         MOVE HDB-COMPANY            TO HRHDB-COMPANY.
050900     IF (HRHDB-EMPLOYEE-FN   = ZEROS)
051000         MOVE HDB-EMPLOYEE           TO HRHDB-EMPLOYEE.
051100     IF (HRHDB-DEPENDENT-FN  = ZEROS)
051200         MOVE HDB-DEPENDENT          TO HRHDB-DEPENDENT.
051300     IF (HRHDB-PLAN-TYPE-FN  = ZEROS)
051400         MOVE HDB-PLAN-TYPE          TO HRHDB-PLAN-TYPE.
051500     IF (HRHDB-PLAN-CODE-FN  = ZEROS)
051600         MOVE HDB-PLAN-CODE          TO HRHDB-PLAN-CODE.
051700     IF (HRHDB-EMP-START-FN  = ZEROS)
051800         MOVE HDB-EMP-START          TO HRHDB-EMP-START.
051900     IF (HRHDB-START-DATE-FN = ZEROS)
052000         MOVE HDB-START-DATE         TO HRHDB-START-DATE.
052100     IF (HRHDB-STOP-DATE-FN  = ZEROS)
052200         MOVE HDB-STOP-DATE          TO HRHDB-STOP-DATE.
J10377     IF (HRHDB-PARTICIPNT-FN = ZEROS)
J10377         MOVE HDB-PARTICIPNT         TO HRHDB-PARTICIPNT
J10377     END-IF.
052300
052400 7000-END.
