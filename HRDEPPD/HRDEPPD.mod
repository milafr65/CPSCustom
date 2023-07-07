******* HRDEPPD 8.1.26.1.14 <1332587408>
000100******************************************************************
000200* HRDEPPD                                                        *
000300*                                                                *
000400*  HR DEPENDENT ROUTINE. ADDS/CHANGES/DELETES AN EMDEPEND RECORD.*
000500*                                                                *
000600*  INPUTS:                                                       *
000700*              HRDEP-BATCH-PGM-SW   (DEFAULT 0 (FALSE))          *
000800*              HRDEP-FC             (I,A,C,D)                    *
000900*              HRDEP-UPDATE-OPTION  (R/U, DEFAULT U)             *
001000*                                                                *
001100*  KEY FIELDS: HRDEP-EMP-COMPANY    (REQUIRED)                   *
001200*              HRDEP-EMP-EMPLOYEE   (REQUIRED)                   *
001300*              HRDEP-EMP-SEQ-NBR    (REQUIRED) (DEPENDENT NBR)   *
001400*                                                                *
001500*  OTHER REQUIRED FIELDS:                                        *
001600*              HRDEP-EMD-LAST-NAME                               *
001700*              HRDEP-EMD-FIRST-NAME                              *
001800*              HRDEP-EMD-EMP-ADDRESS   (H=HOME,S=SUPPL,N=NO)     *
001900*              HRDEP-EMD-REL-CODE      (FROM PCODES)             *
002000*              HRDEP-EMD-DEP-TYPE      (S=SPOUSE,D=DEPENDANT)    *
002100*              HRDEP-EMD-SEX           (F/M)                     *
002200*                                                                *
002300*  OUTPUTS:    HRDEP-BATCH-MSG         (BATCH PGMS ONLY)         *
002400*              HRDEP-EMD-CUR-AGE                                 *
002500*                                                                *
002600*  BATCH PROGRAMS:  THE ARRAY HRDEP-MESSAGE-TABLE IS LOADED WITH *
002700*                   ALL ERRORS (HRDEP-BATCH-MSG).                *
002800*  ONLINE PROGRAMS: PROCESS ERRORS SINGLY WITH CRT-ERROR-NBR.    *
002900******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  312945 | J12945 | HR13 Change to Student Status from Y to N,  *
      *         |        | receives warning message: Warning: Dep Ben  *
      *         |        | will not be updated, ok to continue, and    *
      *         |        | HR13.3 dates do not change.                 *
      *  ------   ------   ------------------------------------------  *
      *  307540 | J07540 | DOMESTIC PARTNER                            *
      *  ------   ------   ------------------------------------------  *
      *  316547 | J16547 | SEPARATE RELATIONSHIP CODES TO DEPENDENTS   *
      *         |        | AND BENEFICIARY.                            *
      *  ------   ------   ------------------------------------------  *
      *  583589 | J83589 | INDIA PAYROLL LOCALIZATION, COUNT NUMBER    *
      *                    OF STUDENT AND HOSTELITES ON ADD/UPDATE/DEL *
      *  ------   ------   ------------------------------------------  *
      *  613204 | J13204 | ADDED FIELDS NEEDED FOR INDIA PAYROLL.      *
      *  ------   ------   ------------------------------------------  *
      *  583589 | J83589 | ADDED FIELD NEEDED FOR DEPENDENT(C611.4)    *
      *  ------   ------   ------------------------------------------  *
      *  660925 | J60925 | CHANGED HRLOG CRITERIA FOR STD, HOS FIELDS  *
      * -------   ------   ------------------------------------------  *
      * 1368464 | J68464 | EVALULATE FC IF INTERFACED FROM GHR         *
      * -------   ------   ------------------------------------------  *
      * 1673640 | J73640 | CHANGE GENDER EDIT                          *
      * -------   ------   ------------------------------------------  *
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by Analysts International,MN                         *
      ******************************************************************
SN0406*  SN0406  04/03/06  MODIFY TO ALLOW 4 MORE FIELDS: DISABILITY   *
SN0406*                    APPROVED FOR CDSP, DISABILITY APPROVED FOR  *
SN0406*                    CDSP DATE, DISABILITY APPROVED FOR CHP,     *
SN0406*                    DISABILITY APPROVED FOR CHP DATE.           *
SN0406*                                                                *
SN0306*  SN0306  03/29/06  MODIFY TO ALLOW HIC NUMBER.                 *
SN0306*                                                                *
      *  AI0080  03/25/03  MODIFY TO ALLOW 3 MORE DATES: DIVORCE,      *
      *                    MARRIAGE, AND STUDENT STATUS REVIEW DATES.  *
      *  ACS001  10/27/10  M. HUNTER - ACS - READDED ABOVE MODS AFTER  *
      *                    9.0 UPGRADE. THE MODS MARKED WITH ACS001    *
      *                    WERE EXISTING MODS BUT NOT PREVIOUSLY MARKED*
      *  ACS002  11/15/10  M. HUNTER - ACS - COMMENTED OUT CODE.       *
      ******************************************************************
003100******************************************************************
003200 5000-HR-DEPENDENT               SECTION.
003300******************************************************************
003400 5000-START.
003500
003600     MOVE "HRDEP"                TO CRT-ERROR-CAT.
003700     MOVE 100                    TO CRT-MSG-NBR.
003800     PERFORM 790-GET-MSG.
003900     MOVE CRT-MESSAGE            TO HRDEP-WARNING-ONLY.
004000
004100     MOVE "HRDEP"                TO CRT-ERROR-CAT.
004200
004300     INITIALIZE                  HRDEP-ERROR-SW
                                       HRDEP-WARNING-SW
004400                                 HRDEP-MESSAGE-TABLE
004500                                 SUB1
004600                                 CRT-MSG-NBR
004700                                 CRT-MESSAGE
                                       HRDEP-CHANGE-COUNTER
                                       HRDEP-ADD-COUNTER.
004800
004900     MOVE ZEROES                 TO HRDEP-EMD-CUR-AGE.
005000
005100     PERFORM 200-EDIT-TRAN
005200     THRU    200-END.
005300
005400     IF  (NOT HRDEP-ERROR)
005500         PERFORM 400-PROCESS-TRAN
005600         THRU    400-END.
005700
005800     GO TO 5000-END.
005900
006000******************************************************************
006100 200-EDIT-TRAN.
006200******************************************************************
006300
006400     PERFORM 210-EDIT-ACCESS
006500     THRU    210-END.
006600
006700     IF (HRDEP-ERROR)
006800         GO TO 200-END.
006900
007000     IF (HRDEP-FC = "D")
007100         PERFORM 220-EDIT-DELETE
007200         THRU    220-END.
007300
007400     IF (HRDEP-FC = "A" OR "C")
007500         PERFORM 230-EDIT-DATA
007600         THRU    230-END
007700         GO TO 200-END.
007800
007900 200-END.
008000
008100******************************************************************
008200 210-EDIT-ACCESS.
008300******************************************************************
008400
008500     MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY.
008600     MOVE SPACES                 TO DB-PROCESS-LEVEL.
008700
008800     PERFORM 840-FIND-PRSSET1.
008900     IF (PRSYSTEM-NOTFOUND)
009000         MOVE WS-TRUE                  TO HRDEP-ERROR-SW
009100         IF (HRDEP-BATCH-PGM)
009200             MOVE 111                  TO CRT-MSG-NBR
009300             MOVE HRDEP-EMD-COMPANY    TO HRDEP-FIELD-VALUE-N
009400             PERFORM 1200-BATCH-MSG
009500             THRU    1200-END
009600         ELSE
009700             MOVE 111                  TO CRT-ERROR-NBR
009800             MOVE HRDEP-EMD-COMPANY-FN TO CRT-FIELD-NBR
009900             GO TO 210-END.
010000
010100     MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY.
010200     MOVE HRDEP-EMD-EMPLOYEE     TO DB-EMPLOYEE.
010300     PERFORM 840-FIND-EMPSET1.
010400     IF (EMPLOYEE-NOTFOUND)
010500         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
010600         IF (HRDEP-BATCH-PGM)
010700             MOVE 102                   TO CRT-MSG-NBR
010800             MOVE HRDEP-EMD-EMPLOYEE    TO HRDEP-FIELD-VALUE-N
010900             PERFORM 1200-BATCH-MSG
011000             THRU    1200-END
011100         ELSE
011200             MOVE 102                   TO CRT-ERROR-NBR
011300             MOVE HRDEP-EMD-EMPLOYEE-FN TO CRT-FIELD-NBR
011400             GO TO 210-END.
011500
011600     MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY.
011700     MOVE HRDEP-EMD-EMPLOYEE     TO DB-EMPLOYEE.
011800     MOVE HRDEP-EMD-SEQ-NBR      TO DB-SEQ-NBR.
011900
012000     PERFORM 840-FIND-EMDSET1.
012100
012200     IF  (EMDEPEND-NOTFOUND)
012300     AND (HRDEP-FC NOT = "A")
012400         MOVE WS-TRUE                  TO HRDEP-ERROR-SW
012500         IF (HRDEP-BATCH-PGM)
012600             MOVE 103                  TO CRT-MSG-NBR
012700             MOVE HRDEP-EMD-SEQ-NBR    TO HRDEP-FIELD-VALUE-N
012800             PERFORM 1200-BATCH-MSG
012900             THRU    1200-END
013000         ELSE
013100             MOVE 103                  TO CRT-ERROR-NBR
013200             MOVE HRDEP-EMD-SEQ-NBR-FN TO CRT-FIELD-NBR
013300             GO TO 210-END.
013400
013500     MOVE EMP-COMPANY            TO CRT-COMPANY.
013600     MOVE EMP-PROCESS-LEVEL      TO CRT-PROCESS-LEVEL.
013700     PERFORM 700-HR-EMP-SECURITY.
013800     IF (HRWS-EMP-SECURED)
013900         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
014000         IF (HRDEP-BATCH-PGM)
014100             MOVE 101                   TO CRT-MSG-NBR
014200             PERFORM 1200-BATCH-MSG
014300             THRU    1200-END
014400         ELSE
014500             MOVE 101                   TO CRT-ERROR-NBR
014600             MOVE HRDEP-EMD-EMPLOYEE-FN TO CRT-FIELD-NBR
014700             GO TO 210-END.
014800 
014900     IF  (EMDEPEND-NOTFOUND)
015000     AND (HRDEP-FC = "C" OR "D")
015100         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
015200         IF (HRDEP-BATCH-PGM)
015300             MOVE 103                   TO CRT-MSG-NBR
015400             PERFORM 1200-BATCH-MSG
015500             THRU    1200-END
015600             GO TO 210-END
015700         ELSE
015800             MOVE 103                   TO CRT-ERROR-NBR
015900             MOVE HRDEP-EMD-SEQ-NBR-FN  TO CRT-FIELD-NBR
016000             GO TO 210-END.
016100
016200     IF  (EMDEPEND-FOUND)
016300     AND (HRDEP-FC = "A")
016400         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
016500         IF (HRDEP-BATCH-PGM)
016600             MOVE 104                   TO CRT-MSG-NBR
016700             PERFORM 1200-BATCH-MSG
016800             THRU    1200-END
016900         ELSE
017000             MOVE 104                   TO CRT-ERROR-NBR
017100             MOVE HRDEP-EMD-SEQ-NBR-FN  TO CRT-FIELD-NBR
017200             GO TO 210-END.
017300
017400 210-END.
017500
017600******************************************************************
017700 220-EDIT-DELETE.
017800******************************************************************
017900
018000     MOVE EMD-COMPANY            TO DB-COMPANY.
018100     MOVE EMD-EMPLOYEE           TO DB-EMPLOYEE.
018200     MOVE EMD-SEQ-NBR            TO DB-DEPENDENT.
018300     MOVE HDBSET1-DEPENDENT      TO WS-DB-BEG-RNG.
018700     PERFORM 850-FIND-BEGRNG-HDBSET1.
018800     IF (HRDEPBEN-FOUND)
019200         MOVE WS-TRUE            TO HRDEP-ERROR-SW
019300         IF (HRDEP-BATCH-PGM)
019400             MOVE 201            TO CRT-MSG-NBR
019500             PERFORM 1200-BATCH-MSG
019600             THRU    1200-END
019700             GO TO 220-END
019800         ELSE
019900             MOVE 201            TO CRT-ERROR-NBR
020000             GO TO 220-END.
020100
018000     MOVE EMD-COMPANY            TO DB-COMPANY.
018100     MOVE EMD-EMPLOYEE           TO DB-EMPLOYEE.
018200     MOVE EMD-SEQ-NBR            TO DB-DEPENDENT.
018500     MOVE EMVSET1-DEPENDENT      TO WS-DB-BEG-RNG.
018700     PERFORM 850-FIND-BEGRNG-EMVSET1.
018800     IF (EMPVISA-FOUND)
019200         MOVE WS-TRUE            TO HRDEP-ERROR-SW
019300         IF (HRDEP-BATCH-PGM)
019400             MOVE 202            TO CRT-MSG-NBR
019500             PERFORM 1200-BATCH-MSG
019600             THRU    1200-END
019700             GO TO 220-END
019800         ELSE
019900             MOVE 202            TO CRT-ERROR-NBR
020000             GO TO 220-END.
020100
020200 220-END.
020300******************************************************************
020400 230-EDIT-DATA.
020500******************************************************************
020600
020700     IF (HRDEP-EMD-LAST-NAME = SPACES)
020800         MOVE WS-TRUE                    TO HRDEP-ERROR-SW
020900         IF (HRDEP-BATCH-PGM)
021000             MOVE 122                    TO CRT-MSG-NBR
021100             PERFORM 1200-BATCH-MSG
021200             THRU    1200-END
021300         ELSE
021400             MOVE 122                    TO CRT-ERROR-NBR
021500             MOVE HRDEP-EMD-LAST-NAME-FN TO CRT-FIELD-NBR
021600             GO TO 230-END.
021700
021800     IF (HRDEP-EMD-FIRST-NAME = SPACES)
021900         MOVE WS-TRUE                     TO HRDEP-ERROR-SW
022000         IF (HRDEP-BATCH-PGM)
022100             MOVE 123                     TO CRT-MSG-NBR
022200             PERFORM 1200-BATCH-MSG
022300             THRU    1200-END
022400         ELSE
022500             MOVE 123                     TO CRT-ERROR-NBR
022600             MOVE HRDEP-EMD-FIRST-NAME-FN TO CRT-FIELD-NBR
022700             GO TO 230-END.
022800
           IF (HRDEP-EMD-NAME-SUFFIX NOT = SPACES)
               MOVE "SU"                           TO DB-TYPE
               MOVE HRDEP-EMD-NAME-SUFFIX          TO DB-HRCTRY-CODE
P58410         MOVE CTCSET2-HRCTRY-CODE            TO WS-DB-BEG-RNG
P58410         PERFORM 850-FIND-BEGRNG-CTCSET2
               IF (HRCTRYCODE-NOTFOUND)
                   MOVE WS-TRUE                  TO HRDEP-ERROR-SW
022000             IF (HRDEP-BATCH-PGM)
199200                 MOVE 118                      TO CRT-MSG-NBR
199500                 PERFORM 1200-BATCH-MSG
199600                 THRU    1200-END
199700             ELSE
022500                 MOVE 118                      TO CRT-ERROR-NBR
022600                 MOVE HRDEP-EMD-NAME-SUFFIX-FN TO CRT-FIELD-NBR
022700                 GO TO 230-END
                   END-IF
               ELSE
               IF (CTC-ACTIVE-FLAG = "I")
                   MOVE WS-TRUE                  TO HRDEP-ERROR-SW
022000             IF (HRDEP-BATCH-PGM)
199200                 MOVE 128                      TO CRT-MSG-NBR
199500                 PERFORM 1200-BATCH-MSG
199600                 THRU    1200-END
                   ELSE
022500                 MOVE 128                      TO CRT-ERROR-NBR
022600                 MOVE HRDEP-EMD-NAME-SUFFIX-FN TO CRT-FIELD-NBR
022700                 GO TO 230-END.
199900
736685     IF (HRDEP-EMD-SEX NOT = "M" AND "F" AND "X" AND SPACE)
023000         MOVE WS-TRUE                TO HRDEP-ERROR-SW
023100         IF (HRDEP-BATCH-PGM)
023200             MOVE 121                TO CRT-MSG-NBR
023300             MOVE HRDEP-EMD-SEX      TO HRDEP-FIELD-VALUE
023400             PERFORM 1200-BATCH-MSG
023500             THRU    1200-END
023600         ELSE
023700             MOVE 121                TO CRT-ERROR-NBR
023800             MOVE HRDEP-EMD-SEX-FN   TO CRT-FIELD-NBR
023900             GO TO 230-END.
024000
           IF (HRDEP-EMD-COUNTRY-CODE      NOT = SPACES)
               MOVE HRDEP-EMD-COUNTRY-CODE TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-NOTFOUND)
                   MOVE WS-TRUE                     TO HRDEP-ERROR-SW
023100             IF (HRDEP-BATCH-PGM)
023200                 MOVE 108                     TO CRT-MSG-NBR
023300                 MOVE HRDEP-EMD-COUNTRY-CODE  TO HRDEP-FIELD-VALUE
023400                 PERFORM 1200-BATCH-MSG
023500                 THRU    1200-END
023600             ELSE
                       MOVE 108                       TO CRT-ERROR-NBR
                       MOVE HRDEP-EMD-COUNTRY-CODE-FN TO CRT-FIELD-NBR
                       GO TO 230-END.

024100     IF (HRDEP-EMD-SMOKER = SPACES)
024200         MOVE "N" TO HRDEP-EMD-SMOKER.
024300
024400     IF (HRDEP-EMD-SMOKER NOT = "Y" AND "N")
024500         MOVE WS-TRUE                  TO HRDEP-ERROR-SW
024600         IF (HRDEP-BATCH-PGM)
024700             MOVE 124                  TO CRT-MSG-NBR
024800             MOVE HRDEP-EMD-SMOKER     TO HRDEP-FIELD-VALUE
024900             PERFORM 1200-BATCH-MSG
025000             THRU    1200-END
025100         ELSE
025200             MOVE 124                  TO CRT-ERROR-NBR
025300             MOVE HRDEP-EMD-SMOKER-FN  TO CRT-FIELD-NBR
025400             GO TO 230-END.
025500
025600     IF (HRDEP-EMD-STUDENT = SPACES)
025700         MOVE "N" TO HRDEP-EMD-STUDENT.
025800
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
J83589*---HRDEPPD#203: Student must be Y or N or H
J83589         IF (HRDEP-EMD-STUDENT NOT = "N" AND "Y" AND "H")
J83589             MOVE WS-TRUE                  TO HRDEP-ERROR-SW
J83589             IF (HRDEP-BATCH-PGM)
J83589                 MOVE 203                  TO CRT-MSG-NBR
J83589                 MOVE HRDEP-EMD-STUDENT    TO HRDEP-FIELD-VALUE
J83589                 PERFORM 1200-BATCH-MSG
J83589                 THRU    1200-END
J83589             ELSE
J83589                 MOVE 203                  TO CRT-ERROR-NBR
J83589                 MOVE HRDEP-EMD-STUDENT-FN TO CRT-FIELD-NBR
J83589                 GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     ELSE
025900         IF (HRDEP-EMD-STUDENT NOT = "Y" AND "N" AND "F" AND "P")
026000             MOVE WS-TRUE                  TO HRDEP-ERROR-SW
026100             IF (HRDEP-BATCH-PGM)
026200                 MOVE 125                  TO CRT-MSG-NBR
026300                 MOVE HRDEP-EMD-STUDENT    TO HRDEP-FIELD-VALUE
026400                 PERFORM 1200-BATCH-MSG
026500                 THRU    1200-END
026600             ELSE
026700                 MOVE 125                  TO CRT-ERROR-NBR
026800                 MOVE HRDEP-EMD-STUDENT-FN TO CRT-FIELD-NBR
026900                 GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     END-IF.
027000
027100     IF (HRDEP-EMD-DISABLED = SPACES)
027200         MOVE "N" TO HRDEP-EMD-DISABLED.
027300
027400     IF (HRDEP-EMD-DISABLED NOT = "Y" AND "N")
027500         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
027600         IF (HRDEP-BATCH-PGM)
027700             MOVE 126                   TO CRT-MSG-NBR
027800             MOVE HRDEP-EMD-DISABLED    TO HRDEP-FIELD-VALUE
027900             PERFORM 1200-BATCH-MSG
028000             THRU    1200-END
028100         ELSE
028200             MOVE 126                   TO CRT-ERROR-NBR
028300             MOVE HRDEP-EMD-DISABLED-FN TO CRT-FIELD-NBR
028400             GO TO 230-END.
028500
           IF (HRDEP-EMD-CONSENT NOT = SPACES AND "Y" AND "N")
               MOVE WS-TRUE                    TO HRDEP-ERROR-SW
               IF (HRDEP-BATCH-PGM)
                   MOVE 135                    TO CRT-MSG-NBR
                   MOVE HRDEP-EMD-CONSENT      TO HRDEP-FIELD-VALUE
                   PERFORM 1200-BATCH-MSG
                   THRU    1200-END.

           IF (HRDEP-DECEASED NOT = SPACES AND "Y" AND "N")
               MOVE WS-TRUE                    TO HRDEP-ERROR-SW
               IF (HRDEP-BATCH-PGM)
                   MOVE 148                    TO CRT-MSG-NBR
                   MOVE HRDEP-DECEASED         TO HRDEP-FIELD-VALUE
                   PERFORM 1200-BATCH-MSG
                   THRU 1200-END.

           IF  (HRDEP-DECEASED  = "Y")
           AND (HRDEP-XMIT-D    = 0)
              IF ((EMD-ACTIVE-FLAG        = "A")
J83406        AND (CRT-PROGRAM-CODE NOT   = "HR13"))
J83406        OR ((EMD-ACTIVE-FLAG        = "A")
J83406        AND (CRT-PROGRAM-CODE       = "HR13")
J83406        AND (HRDEP-EMD-ACTIVE-FLAG  = "A"))
                  MOVE WS-TRUE              TO HRDEP-WARNING-SW
P53472            IF (HRDEP-BATCH-PGM)
P53472                MOVE 147                  TO CRT-MSG-NBR
P53472                MOVE SPACES               TO HRDEP-FIELD-VALUE
P53472                PERFORM 1200-BATCH-MSG
P53472                THRU    1200-END
P53472            ELSE
                      MOVE WS-TRUE                  TO HRDEP-ERROR-SW
026700                MOVE 166                      TO CRT-ERROR-NBR
026800                MOVE HRDEP-EMD-ACTIVE-FLAG-FN TO CRT-FIELD-NBR
                      MOVE 1                        TO HRDEP-XMIT-D
026900                GO TO 230-END
                  END-IF.
                     
           IF (HRDEP-ESTAB-PATIENT NOT = ZEROES AND 1 AND 2 AND 3)
              IF (HRDEP-BATCH-PGM)
                  MOVE WS-TRUE                   TO HRDEP-ERROR-SW
                  MOVE 143                       TO CRT-MSG-NBR 
                  PERFORM 1200-BATCH-MSG
                  THRU 1200-END.

028600     IF (HRDEP-EMD-HL-COV-FLAG = SPACES)
028700         MOVE "N" TO HRDEP-EMD-HL-COV-FLAG.
028800
028900     IF (HRDEP-EMD-DN-COV-FLAG = SPACES)
029000         MOVE "N" TO HRDEP-EMD-DN-COV-FLAG.
029100
029200     IF (HRDEP-EMD-DL-COV-FLAG = SPACES)
029300         MOVE "N" TO HRDEP-EMD-DL-COV-FLAG.
029400
029500     IF (HRDEP-EMD-HL-COV-FLAG NOT = "Y" AND "N")
029600     OR (HRDEP-EMD-DN-COV-FLAG NOT = "Y" AND "N")
029700     OR (HRDEP-EMD-DL-COV-FLAG NOT = "Y" AND "N")
029800         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
029900         IF (HRDEP-BATCH-PGM)
030000             MOVE 127                   TO CRT-MSG-NBR
030100             PERFORM 1200-BATCH-MSG
030200             THRU    1200-END.
030300
030400     IF (HRDEP-EMD-REL-CODE = SPACES)
030500         MOVE WS-TRUE                     TO HRDEP-ERROR-SW
030600         IF (HRDEP-BATCH-PGM)
030700             MOVE 120                     TO CRT-MSG-NBR
030800             MOVE HRDEP-EMD-REL-CODE      TO HRDEP-FIELD-VALUE
030900             PERFORM 1200-BATCH-MSG
031000             THRU    1200-END
031100         ELSE
031200             MOVE 120                     TO CRT-ERROR-NBR
031300             MOVE HRDEP-EMD-REL-CODE-FN   TO CRT-FIELD-NBR
031400             GO TO 230-END
031500         END-IF
031600     ELSE
031700         MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY
031800         MOVE "DP"                   TO DB-TYPE
031900         MOVE HRDEP-EMD-REL-CODE     TO DB-CODE
032000         PERFORM 840-FIND-PCOSET1
032100         IF   (PCODES-NOTFOUND)
032200         AND ((HRDEP-FC                   = "A") 
032300         OR  ((HRDEP-FC                   = "C") 
032400         AND  (HRDEP-EMD-REL-CODE    NOT = EMD-REL-CODE)))
032500             MOVE WS-TRUE                     TO HRDEP-ERROR-SW
032600             IF (HRDEP-BATCH-PGM)
032700                 MOVE 106                     TO CRT-MSG-NBR
032800                 MOVE HRDEP-EMD-REL-CODE      TO HRDEP-FIELD-VALUE
032900                 PERFORM 1200-BATCH-MSG
033000                 THRU    1200-END
033100             ELSE
033200                 MOVE 106                     TO CRT-ERROR-NBR
033300                 MOVE HRDEP-EMD-REL-CODE-FN   TO CRT-FIELD-NBR
033400                 GO TO 230-END
033500             END-IF
033600         ELSE
033700             IF   (PCO-ACTIVE-FLAG     NOT = "A")
033800             AND ((HRDEP-FC               = "A") 
033900             OR  ((HRDEP-FC               = "C")
034000             AND  (HRDEP-EMD-REL-CODE NOT = EMD-REL-CODE)))
034100                 MOVE WS-TRUE                 TO HRDEP-ERROR-SW
034200                 IF (HRDEP-BATCH-PGM)
034300                     MOVE 107                 TO CRT-MSG-NBR
034400                     MOVE HRDEP-EMD-REL-CODE  TO HRDEP-FIELD-VALUE
034500                     PERFORM 1200-BATCH-MSG
034600                     THRU    1200-END
034700                 ELSE
034800                     MOVE 107                  TO CRT-ERROR-NBR
034900                     MOVE HRDEP-EMD-REL-CODE-FN TO CRT-FIELD-NBR
035000                     GO TO 230-END 
J16547                 END-IF
J16547             ELSE
J16547* CHECK TO VERIFY DEPENDENT IS NOT A BENEFICIARY
J16547             IF  (PCO-DP-REL-TYPE         = 2)
J16547                 MOVE WS-TRUE                  TO HRDEP-ERROR-SW
J16547                 IF (HRDEP-BATCH-PGM)
J16547                     MOVE 106                  TO CRT-MSG-NBR
J16547                     MOVE HRDEP-EMD-REL-CODE  TO HRDEP-FIELD-VALUE
J16547                     PERFORM 1200-BATCH-MSG
J16547                     THRU    1200-END
J16547                 ELSE
J16547                     MOVE 106                  TO CRT-ERROR-NBR
J16547                     MOVE HRDEP-EMD-REL-CODE-FN TO CRT-FIELD-NBR
J16547                     GO TO 230-END
J16547                 END-IF
J16547             END-IF  
J16547             END-IF
J16547         END-IF
J16547     END-IF.
035100              
035200     IF (HRDEP-EMD-PRIMARY-CARE   NOT = SPACES)
           AND ((HRDEP-FC                   = "A") 
           OR  ((HRDEP-FC                   = "C") 
           AND  (HRDEP-EMD-PRIMARY-CARE NOT = EMD-PRIMARY-CARE)))
035300         MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY
035400         MOVE "PC"                   TO DB-TYPE
035500         MOVE HRDEP-EMD-PRIMARY-CARE TO DB-CODE
035600         PERFORM 840-FIND-PCOSET1
035700         IF (PCODES-NOTFOUND)
035900             MOVE WS-TRUE                    TO HRDEP-ERROR-SW
036000             IF (HRDEP-BATCH-PGM)
036100                 MOVE 116                    TO CRT-MSG-NBR
036200                 MOVE HRDEP-EMD-PRIMARY-CARE TO HRDEP-FIELD-VALUE
036300                 PERFORM 1200-BATCH-MSG
036400                 THRU    1200-END
036500             ELSE
036600                 MOVE 116                       TO CRT-ERROR-NBR
036700                 MOVE HRDEP-EMD-PRIMARY-CARE-FN TO CRT-FIELD-NBR
036800                 GO TO 230-END
036900             END-IF
037000         ELSE
037100             IF  (PCO-ACTIVE-FLAG     NOT = "A")
037300               MOVE WS-TRUE                  TO HRDEP-ERROR-SW
037400               IF (HRDEP-BATCH-PGM)
037500                 MOVE 117                    TO CRT-MSG-NBR
037600                 MOVE HRDEP-EMD-PRIMARY-CARE TO HRDEP-FIELD-VALUE
037700                 PERFORM 1200-BATCH-MSG
037800                 THRU    1200-END
037900               ELSE
038000                 MOVE 117                       TO CRT-ERROR-NBR
038100                 MOVE HRDEP-EMD-PRIMARY-CARE-FN TO CRT-FIELD-NBR
038200                 GO TO 230-END.

           IF  (HRDEP-EMD-PRIMARY-CARE = SPACES)
           AND (HRDEP-ESTAB-PATIENT NOT = ZEROES)
035900         MOVE WS-TRUE                    TO HRDEP-ERROR-SW
               IF (HRDEP-BATCH-PGM)
                  MOVE 146                        TO CRT-MSG-NBR
                  MOVE HRDEP-ESTAB-PATIENT        TO HRDEP-FIELD-VALUE 
                  PERFORM 1200-BATCH-MSG
                  THRU 1200-END
               ELSE
                  MOVE 146                        TO CRT-MSG-NBR
                  MOVE HRDEP-ESTAB-PATIENT-FN    TO CRT-FIELD-NBR
                  GO TO 230-END.

           IF  (HRDEP-DEATH-DATE NOT = ZEROES)
           AND (HRDEP-DECEASED NOT = "Y")
035900         MOVE WS-TRUE                    TO HRDEP-ERROR-SW
               IF (HRDEP-BATCH-PGM)
                  MOVE 144                        TO CRT-MSG-NBR
                  MOVE HRDEP-DECEASED             TO HRDEP-FIELD-VALUE 
                  PERFORM 1200-BATCH-MSG
                  THRU 1200-END
               ELSE
                  MOVE 144                       TO CRT-MSG-NBR
                  MOVE HRDEP-DECEASED-FN         TO CRT-FIELD-NBR
                  GO TO 230-END.
                    
038300
038400     IF (HRDEP-EMD-BIRTHDATE    > WS-SYSTEM-DATE-YMD)
038500         MOVE WS-TRUE                       TO HRDEP-ERROR-SW
038600         IF (HRDEP-BATCH-PGM)
038700             MOVE 109                       TO CRT-MSG-NBR
038800             MOVE HRDEP-EMD-BIRTHDATE       TO HRDEP-FIELD-VALUE-D
038900             PERFORM 1200-BATCH-MSG
039000             THRU    1200-END
039100         ELSE
039200             MOVE 109                       TO CRT-ERROR-NBR
039300             MOVE HRDEP-EMD-BIRTHDATE-FN    TO CRT-FIELD-NBR
039400             GO TO 230-END.
039500
039600     IF  (HRDEP-EMD-ADOPTION-DATE < HRDEP-EMD-BIRTHDATE)
039700     AND (HRDEP-EMD-ADOPTION-DATE NOT = ZEROES)
039800         MOVE WS-TRUE                       TO HRDEP-ERROR-SW
039900         IF (HRDEP-BATCH-PGM)
040000             MOVE 131                       TO CRT-MSG-NBR
040100             MOVE HRDEP-EMD-ADOPTION-DATE   TO HRDEP-FIELD-VALUE-D
040200             PERFORM 1200-BATCH-MSG
040300             THRU    1200-END
040400         ELSE
040500             MOVE 131                        TO CRT-ERROR-NBR
040600             MOVE HRDEP-EMD-ADOPTION-DATE-FN TO CRT-FIELD-NBR
040700             GO TO 230-END.
040800
040900     IF  (HRDEP-EMD-PLACEMENT-DATE < HRDEP-EMD-BIRTHDATE) 
041000     AND (HRDEP-EMD-PLACEMENT-DATE NOT = ZEROES)    
041100         MOVE WS-TRUE                       TO HRDEP-ERROR-SW
041200         IF (HRDEP-BATCH-PGM)
041300             MOVE 132                       TO CRT-MSG-NBR
041400             MOVE HRDEP-EMD-PLACEMENT-DATE  TO HRDEP-FIELD-VALUE-D
041500             PERFORM 1200-BATCH-MSG
041600             THRU    1200-END
041700         ELSE
041800             MOVE 132                       TO CRT-ERROR-NBR
041900             MOVE HRDEP-EMD-PLACEMENT-DATE-FN
042000                                            TO CRT-FIELD-NBR
042100             GO TO 230-END.
042200
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589         IF (HRDEP-EMD-EMP-ADDRESS = SPACES)
J83589             MOVE "1"                   TO HRDEP-EMD-EMP-ADDRESS
J83589         END-IF
J83589     ELSE
               IF (HRDEP-EMD-EMP-ADDRESS = SPACES)
                   MOVE "H"                   TO HRDEP-EMD-EMP-ADDRESS
J83589         END-IF
J83589     END-IF.

J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
J83589*---HRDEP#204: Employee Address must be 1 or 2 or 3 
J83589         IF (HRDEP-EMD-EMP-ADDRESS NOT = "1" AND "2" AND "3")
J83589             MOVE WS-TRUE                    TO HRDEP-ERROR-SW
J83589             IF (HRDEP-BATCH-PGM)
J83589                 MOVE 204                    TO CRT-MSG-NBR
J83589                 MOVE HRDEP-EMD-EMP-ADDRESS  TO HRDEP-FIELD-VALUE
J83589                 PERFORM 1200-BATCH-MSG
J83589                 THRU 1200-END
J83589             ELSE
J83589                 MOVE 204                    TO CRT-ERROR-NBR
J83589                 MOVE HRDEP-EMD-EMP-ADDRESS-FN
J83589                                             TO CRT-FIELD-NBR
J83589                 GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     ELSE 
               IF (HRDEP-EMD-EMP-ADDRESS NOT = "H" AND "S" AND "N")
                   MOVE WS-TRUE                    TO HRDEP-ERROR-SW
                   IF (HRDEP-BATCH-PGM)
                       MOVE 142                    TO CRT-MSG-NBR
                       MOVE HRDEP-EMD-EMP-ADDRESS  TO HRDEP-FIELD-VALUE
                       PERFORM 1200-BATCH-MSG
                       THRU 1200-END
                   ELSE
                       MOVE 142                    TO CRT-ERROR-NBR
                       MOVE HRDEP-EMD-EMP-ADDRESS-FN
                                                   TO CRT-FIELD-NBR
                       GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     END-IF.

J83589*---HRDEP#205: Employee permanent address 1 or 2 does not exist
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
J83589         IF (HRDEP-EMD-EMP-ADDRESS = "2")
J83589             MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE
J83589             PERFORM 840-FIND-EMPSET1
J83589             IF  (EMP-ADDR1 = SPACES)
J83589             AND (EMP-ADDR2 = SPACES)
J83589                 MOVE WS-TRUE            TO HRDEP-ERROR-SW
J83589                 IF (HRDEP-BATCH-PGM)
J83589                     MOVE 205            TO CRT-MSG-NBR
J83589                     PERFORM 1200-BATCH-MSG
J83589                     THRU    1200-END
J83589                 ELSE
J83589                     MOVE 205                     TO CRT-ERROR-NBR
J83589                     MOVE HRDEP-EMD-EMP-ADDRESS-FN
J83589                                                  TO CRT-FIELD-NBR
J83589                     GO TO 230-END 
J83589                 END-IF
J83589             END-IF
J83589         END-IF
J83589     ELSE
J83589*---HRDEP#114: Employee home address 1 or 2 does not exist
042300         IF (HRDEP-EMD-EMP-ADDRESS = "H")
042400             MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE
042500             PERFORM 840-FIND-EMPSET1
042600             IF  (EMP-ADDR1 = SPACES)
042700             AND (EMP-ADDR2 = SPACES)
042800                 MOVE WS-TRUE            TO HRDEP-ERROR-SW
042900                 IF (HRDEP-BATCH-PGM)
043000                     MOVE 114            TO CRT-MSG-NBR
043100                     PERFORM 1200-BATCH-MSG
043200                     THRU    1200-END
043300                 ELSE
043400                     MOVE 114                     TO CRT-ERROR-NBR
043500                     MOVE HRDEP-EMD-EMP-ADDRESS-FN
J83589                                                  TO CRT-FIELD-NBR
043600                     GO TO 230-END
J83589                 END-IF
J83589             END-IF
J83589         END-IF
J83589     END-IF.
043700
J83589*---HRDEP#206: Employee present address 1 or 2 does not exist
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
J83589         IF (HRDEP-EMD-EMP-ADDRESS = "1")
J83589             MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE
J83589             PERFORM 840-FIND-PEMSET1
J83589             IF  (PEM-SUPP-ADDR1 = SPACES)
J83589             AND (PEM-SUPP-ADDR2 = SPACES)
J83589                 MOVE WS-TRUE            TO HRDEP-ERROR-SW
J83589                 IF (HRDEP-BATCH-PGM)
J83589                     MOVE 206            TO CRT-MSG-NBR
J83589                     PERFORM 1200-BATCH-MSG
J83589                     THRU    1200-END
J83589                 ELSE
J83589                     MOVE 206                     TO CRT-ERROR-NBR
J83589                     MOVE HRDEP-EMD-EMP-ADDRESS-FN
J83589                                                  TO CRT-FIELD-NBR
J83589                    GO TO 230-END 
J83589                 END-IF
J83589             END-IF
J83589         END-IF
J83589     ELSE
J83589*---HRDEP#115: Employee supplemental address 1 or 2 does not exist
043800         IF (HRDEP-EMD-EMP-ADDRESS = "S")
043900             MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE
044000             PERFORM 840-FIND-PEMSET1
044100             IF  (PEM-SUPP-ADDR1 = SPACES)
044200             AND (PEM-SUPP-ADDR2 = SPACES)
044300                 MOVE WS-TRUE            TO HRDEP-ERROR-SW
044400                 IF (HRDEP-BATCH-PGM)
044500                     MOVE 115            TO CRT-MSG-NBR
044600                     PERFORM 1200-BATCH-MSG
044700                     THRU    1200-END
044800                 ELSE
044900                     MOVE 115                     TO CRT-ERROR-NBR
045000                     MOVE HRDEP-EMD-EMP-ADDRESS-FN
J83589                                                  TO CRT-FIELD-NBR
045100                     GO TO 230-END
J83589                 END-IF
J83589             END-IF
J83589         END-IF
J83589     END-IF.
045200
045300      IF  (HRDEP-EMD-EMP-ADDRESS = "H" OR "S")
045400      AND (EMD-EMP-ADDRESS = "N")
045500          MOVE SPACES            TO HRDEP-EMD-ADDR1 
045600                                    HRDEP-EMD-ADDR2 
045700                                    HRDEP-EMD-ADDR3 
045800                                    HRDEP-EMD-ADDR4 
045900                                    HRDEP-EMD-CITY 
046000                                    HRDEP-EMD-STATE 
046100                                    HRDEP-EMD-ZIP 
046200                                    HRDEP-EMD-COUNTRY-CODE
046300                                    HRDEP-EMD-HM-PHONE-CNTRY 
046400                                    HRDEP-EMD-HM-PHONE-NBR.
046500
J83589*--HRDEP#113: Emp address used; Cannot maintain dep address
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
046600         IF ((HRDEP-EMD-ADDR1        NOT = SPACES)
046700         OR (HRDEP-EMD-ADDR2         NOT = SPACES)
046800         OR (HRDEP-EMD-ADDR3         NOT = SPACES)
046900         OR (HRDEP-EMD-ADDR4         NOT = SPACES)
047000         OR (HRDEP-EMD-CITY          NOT = SPACES)
047100         OR (HRDEP-EMD-STATE         NOT = SPACES)
047200         OR (HRDEP-EMD-ZIP           NOT = SPACES)
047300         OR (HRDEP-EMD-COUNTRY-CODE  NOT = SPACES))
047400         AND (HRDEP-EMD-EMP-ADDRESS    = "1" OR "2")
047500             MOVE SPACES            TO HRDEP-EMD-ADDR1 
047600                                       HRDEP-EMD-ADDR2 
047700                                       HRDEP-EMD-ADDR3 
047800                                       HRDEP-EMD-ADDR4 
047900                                       HRDEP-EMD-CITY 
048000                                       HRDEP-EMD-STATE 
048100                                       HRDEP-EMD-ZIP 
048200                                       HRDEP-EMD-COUNTRY-CODE
048300                                       HRDEP-EMD-HM-PHONE-CNTRY 
048400                                       HRDEP-EMD-HM-PHONE-NBR
048500             MOVE WS-TRUE           TO HRDEP-ERROR-SW
048600             IF (HRDEP-BATCH-PGM)
048700                 MOVE 113            TO CRT-MSG-NBR
048800                 PERFORM 1200-BATCH-MSG
048900                 THRU    1200-END
049000             ELSE
049100                 MOVE 113                       TO CRT-ERROR-NBR
049200                 MOVE HRDEP-EMD-EMP-ADDRESS-FN  TO CRT-FIELD-NBR
049300                 GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     ELSE
J83589*--HRDEP#113: Emp address used; Cannot maintain dep address
046600         IF ((HRDEP-EMD-ADDR1        NOT = SPACES)
046700         OR (HRDEP-EMD-ADDR2         NOT = SPACES)
046800         OR (HRDEP-EMD-ADDR3         NOT = SPACES)
046900         OR (HRDEP-EMD-ADDR4         NOT = SPACES)
047000         OR (HRDEP-EMD-CITY          NOT = SPACES)
047100         OR (HRDEP-EMD-STATE         NOT = SPACES)
047200         OR (HRDEP-EMD-ZIP           NOT = SPACES)
047300         OR (HRDEP-EMD-COUNTRY-CODE  NOT = SPACES))
047400         AND (HRDEP-EMD-EMP-ADDRESS    = "H" OR "S")
047500             MOVE SPACES            TO HRDEP-EMD-ADDR1 
047600                                       HRDEP-EMD-ADDR2 
047700                                       HRDEP-EMD-ADDR3 
047800                                       HRDEP-EMD-ADDR4 
047900                                       HRDEP-EMD-CITY 
048000                                       HRDEP-EMD-STATE 
048100                                       HRDEP-EMD-ZIP 
048200                                       HRDEP-EMD-COUNTRY-CODE
048300                                       HRDEP-EMD-HM-PHONE-CNTRY 
048400                                       HRDEP-EMD-HM-PHONE-NBR
048500             MOVE WS-TRUE           TO HRDEP-ERROR-SW
048600             IF (HRDEP-BATCH-PGM)
048700                 MOVE 113            TO CRT-MSG-NBR
048800                 PERFORM 1200-BATCH-MSG
048900                 THRU    1200-END
049000             ELSE
049100                 MOVE 113                       TO CRT-ERROR-NBR
049200                 MOVE HRDEP-EMD-EMP-ADDRESS-FN  TO CRT-FIELD-NBR
049300                 GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     END-IF.
049400             
J83589*HRDEP#105: Employee address is No; Supply dependent address 1
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
J83589         IF  (HRDEP-EMD-EMP-ADDRESS = "3")
J83589             AND (HRDEP-EMD-ADDR1       = SPACES)
J83589             MOVE WS-TRUE                TO HRDEP-ERROR-SW
J83589             IF (HRDEP-BATCH-PGM)
J83589                 MOVE 105                TO CRT-MSG-NBR
J83589                 PERFORM 1200-BATCH-MSG
J83589                 THRU    1200-END
J83589             ELSE
J83589                 MOVE 105                TO CRT-ERROR-NBR
J83589                 MOVE HRDEP-EMD-ADDR1-FN TO CRT-FIELD-NBR
J83589                 GO TO 230-END
J83589             END-IF
J83589         END-IF
J83589     ELSE
049500         IF  (HRDEP-EMD-EMP-ADDRESS = "N")
049600         AND (HRDEP-EMD-ADDR1       = SPACES)
049700             MOVE WS-TRUE                TO HRDEP-ERROR-SW
049800             IF (HRDEP-BATCH-PGM)
049900                 MOVE 105                TO CRT-MSG-NBR
050000                 PERFORM 1200-BATCH-MSG
050100                 THRU    1200-END
050200             ELSE
050300                 MOVE 105                TO CRT-ERROR-NBR
050400                 MOVE HRDEP-EMD-ADDR1-FN TO CRT-FIELD-NBR
050500                 GO TO 230-END 
J83589             END-IF
J83589         END-IF 
J83589     END-IF.

           IF  (HRDEP-EMD-EMP-ADDRESS = "N")
           AND (HRDEP-EMD-STATE NOT = SPACES)
               IF (HRDEP-EMD-COUNTRY-CODE = "CA")
                  MOVE HRDEP-EMD-STATE        TO DB-PROVINCE
                  PERFORM 840-KFIND-PPVSET1
                  IF (PRPROVINCE-KNOTFOUND)
                      MOVE 150                TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-STATE-FN TO CRT-FIELD-NBR
                      IF (HRDEP-BATCH-PGM)
                          PERFORM 1200-BATCH-MSG
                          THRU    1200-END
                      ELSE
                          GO TO 230-END
                      END-IF.

J83589*---HRDEP#149: Invalid State
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589     OR ((CRT-PROGRAM-CODE = "HR513")
J83589     AND (HRDEP-EMD-COUNTRY-CODE = "IN"))
J83589         IF (HRDEP-EMD-STATE NOT = SPACES)
J83589             MOVE HRDEP-EMD-STATE        TO DB-STATE   
J83589             PERFORM 840-KFIND-C6SSET1
J83589             IF (C6STATE-KNOTFOUND)
J83589                 MOVE WS-TRUE                TO HRDEP-ERROR-SW
J83589                 IF (HRDEP-BATCH-PGM)
J83589                     MOVE 149                TO CRT-MSG-NBR  
J83589                     MOVE HRDEP-EMD-EMP-ADDRESS
J83589                                             TO HRDEP-FIELD-VALUE
J83589                     PERFORM 1200-BATCH-MSG
J83589                     THRU    1200-END
J83589                 ELSE
J83589                     MOVE 149                TO CRT-ERROR-NBR
J83589                     MOVE HRDEP-EMD-STATE-FN TO CRT-FIELD-NBR
J83589                     GO TO 230-END
J83589                 END-IF 
J83589             END-IF
J83589         END-IF
J83589     ELSE         
               IF (HRDEP-EMD-STATE NOT = SPACES)
                   IF  (HRDEP-EMD-COUNTRY-CODE = "US")
      *  AA, AE, and AP are military addresses
                   AND (HRDEP-EMD-STATE NOT = "AA" AND "AE" AND "AP")
                       MOVE HRDEP-EMD-STATE        TO DB-STATE   
                       PERFORM 840-KFIND-PSASET1
                       IF (PRSTATE-KNOTFOUND)
                           MOVE 149                TO CRT-ERROR-NBR
                           MOVE HRDEP-EMD-STATE-FN TO CRT-FIELD-NBR
                           IF (HRDEP-BATCH-PGM)
                               PERFORM 1200-BATCH-MSG
                               THRU    1200-END
                           ELSE
                               GO TO 230-END
                           END-IF
                       END-IF
                   END-IF
J83589         END-IF
J83589     END-IF.

           IF  (HRDEP-PT-XMIT-NBR1 = ZEROES)
               INITIALIZE CRT-ERROR-NBR
               MOVE HRDEP-EMD-FICA-NBR         TO HRWS-SOC-NBR
               MOVE HRDEP-EMD-COUNTRY-CODE TO HRWS-WORK-COUNTRY
               IF  (HRWS-WORK-COUNTRY = SPACES)
                   MOVE EMP-WORK-COUNTRY   TO HRWS-WORK-COUNTRY
               END-IF
               MOVE HRDEP-EMD-BIRTHDATE    TO HRWS-DATE-8-FIELD

               PERFORM 760-HR-FORMAT-SOC-NBR
               IF (ERROR-FOUND)
                   MOVE "HRDEP"                TO CRT-ERROR-CAT
                   MOVE HRWS-WORK-COUNTRY      TO CRT-ERR-VAR1
                   IF  (HRDEP-BATCH-PGM)
                       MOVE WS-TRUE            TO HRDEP-WARNING-SW
                       MOVE 133                TO CRT-MSG-NBR
                       MOVE HRDEP-EMD-FICA-NBR TO HRDEP-FIELD-VALUE
                       PERFORM 1200-BATCH-MSG
                       THRU    1200-END
                   ELSE
                       MOVE WS-TRUE            TO HRDEP-ERROR-SW
                       MOVE HRDEP-EMD-FICA-NBR-FN  TO CRT-FIELD-NBR
                       MOVE 134                TO CRT-ERROR-NBR
                       MOVE 1                  TO HRDEP-PT-XMIT-NBR1
                       GO TO 230-END
                   END-IF
               END-IF
               MOVE HRWS-SOC-NBR           TO HRDEP-EMD-FICA-NBR
           END-IF.
052300
           IF (HRDEP-EMD-ACTIVE-FLAG           = SPACES)
               MOVE "A"                        TO HRDEP-EMD-ACTIVE-FLAG.

052400     IF (HRDEP-EMD-ACTIVE-FLAG NOT = "A" AND "I")
052500         MOVE WS-TRUE                      TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE 129                      TO CRT-MSG-NBR
052800             MOVE HRDEP-EMD-ACTIVE-FLAG    TO HRDEP-FIELD-VALUE
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
053100         ELSE
053200             MOVE 129                      TO CRT-ERROR-NBR
053300             MOVE HRDEP-EMD-ACTIVE-FLAG-FN TO CRT-FIELD-NBR
053400             GO TO 230-END.
053500
           IF  (HRDEP-EMD-ACTIVE-FLAG          = "I")
           AND (EMD-ACTIVE-FLAG                = "A")
           AND (HRDEP-PT-XMIT-NBR2             = ZEROES)
               INITIALIZE CRT-ERROR-NBR
018000         MOVE HRDEP-EMD-COMPANY          TO DB-COMPANY
018100         MOVE HRDEP-EMD-EMPLOYEE         TO DB-EMPLOYEE
018200         MOVE HRDEP-EMD-SEQ-NBR          TO DB-DEPENDENT
018500         MOVE DEPSET1-DEPENDENT          TO WS-DB-BEG-RNG
018700         PERFORM 850-FIND-BEGRNG-DEPSET1
018800         IF (DEPBEN-FOUND)
                   IF (HRDEP-BATCH-PGM)
                       MOVE WS-TRUE            TO HRDEP-WARNING-SW
                       INITIALIZE CRT-ERROR-NBR
                       MOVE 139                TO CRT-MSG-NBR
                       MOVE HRDEP-EMD-ACTIVE-FLAG
                                               TO HRDEP-FIELD-VALUE
                       PERFORM 1200-BATCH-MSG
                       THRU    1200-END
                   ELSE
                       MOVE WS-TRUE            TO HRDEP-ERROR-SW
                       MOVE HRDEP-EMD-ACTIVE-FLAG-FN
                                               TO CRT-FIELD-NBR
                       MOVE 140                TO CRT-ERROR-NBR
                       MOVE 1                  TO HRDEP-PT-XMIT-NBR2
                       GO TO 230-END.
052300
J07540     IF (HRDEP-EMD-DEP-TYPE NOT = "S" AND "D" AND "P")
052500         MOVE WS-TRUE                   TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE 119                   TO CRT-MSG-NBR
052800             MOVE HRDEP-EMD-DEP-TYPE    TO HRDEP-FIELD-VALUE
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
053100         ELSE
053200             MOVE 119                   TO CRT-ERROR-NBR
053300             MOVE HRDEP-EMD-DEP-TYPE-FN TO CRT-FIELD-NBR
053400             GO TO 230-END.
053500
          IF (HRDEP-FC                          = "A")
              IF  (HRDEP-EMD-ACTIVE-FLAG        = "I")
              AND (HRDEP-EMD-HL-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 136                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 136                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-HL-COV-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END
                  END-IF
              END-IF

              IF  (HRDEP-EMD-ACTIVE-FLAG        = "I")
              AND (HRDEP-EMD-DN-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 136                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 136                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-DN-COV-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END
                  END-IF
              END-IF

              IF  (HRDEP-EMD-ACTIVE-FLAG        = "I")
              AND (HRDEP-EMD-DL-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 136                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 136                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-DL-COV-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END.

          IF  (HRDEP-FC                         = "C")
          AND (EMD-ACTIVE-FLAG                  = "A")
          AND (HRDEP-EMD-ACTIVE-FLAG            = "I")
              IF  (EMD-HL-COV-FLAG              = "Y")
              AND (HRDEP-EMD-HL-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 137                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 137                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-ACTIVE-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END
                  END-IF
              END-IF

              IF  (EMD-DN-COV-FLAG              = "Y")
              AND (HRDEP-EMD-DN-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 137                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 137                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-ACTIVE-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END
                  END-IF
              END-IF

              IF  (EMD-DL-COV-FLAG              = "Y")
              AND (HRDEP-EMD-DL-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 137                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 137                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-ACTIVE-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END.

          IF  (HRDEP-FC                         = "C")
          AND (EMD-ACTIVE-FLAG                  = "I")
          AND (HRDEP-EMD-ACTIVE-FLAG            = "I")
              IF  (EMD-HL-COV-FLAG              = "N")
              AND (HRDEP-EMD-HL-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 138                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 138                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-HL-COV-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END
                  END-IF
              END-IF

              IF  (EMD-DN-COV-FLAG              = "N")
              AND (HRDEP-EMD-DN-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 138                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 138                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-DN-COV-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END
                  END-IF
              END-IF

              IF  (EMD-DL-COV-FLAG              = "N")
              AND (HRDEP-EMD-DL-COV-FLAG        = "Y")
052500            MOVE WS-TRUE                  TO HRDEP-ERROR-SW
052600            IF (HRDEP-BATCH-PGM)
052700                MOVE 138                  TO CRT-MSG-NBR
052900                PERFORM 1200-BATCH-MSG
053000                THRU    1200-END
053100            ELSE
                      MOVE 138                      TO CRT-ERROR-NBR
                      MOVE HRDEP-EMD-DL-COV-FLAG-FN TO CRT-FIELD-NBR
                      GO TO 230-END.
                      
J83494     IF  (HRDEP-EMD-STUDENT = "N" OR SPACES)
J83494     AND (HRDEP-VERIFY-DATE NOT = ZEROS)
J83494         MOVE 167                    TO CRT-ERROR-NBR
J83494         SET HRDEP-ERROR             TO TRUE
J83494         IF (HRDEP-BATCH-PGM)
J83494             MOVE WS-TRUE            TO HRDEP-ERROR-SW     
J83494             PERFORM 1200-BATCH-MSG
J83494             THRU    1200-END
J83494         ELSE
J83494             MOVE HRDEP-VERIFY-DATE-FN
J83494                                     TO CRT-FIELD-NBR
J83494             GO TO 230-END
J83494         END-IF
J83494     END-IF.

J83494     IF (HRDEP-VERIFY-DATE > WS-SYSTEM-DATE-YMD)
J83494         MOVE 168                    TO CRT-ERROR-NBR
J83494         SET HRDEP-ERROR             TO TRUE
J83494         IF (HRDEP-BATCH-PGM)
J83494             MOVE WS-TRUE            TO HRDEP-ERROR-SW     
J83494             PERFORM 1200-BATCH-MSG
J83494             THRU    1200-END
J83494         ELSE
J83494             MOVE HRDEP-VERIFY-DATE-FN
J83494                                     TO CRT-FIELD-NBR
J83494             GO TO 230-END
J83494         END-IF               
J83494     END-IF.                      

           IF  (HRDEP-FC               = "C")
           AND (HRDEP-EMD-ACTIVE-FLAG  = "I")
           AND (EMD-ACTIVE-FLAG        = "A")
018000         MOVE EMD-COMPANY        TO DB-COMPANY
018100         MOVE EMD-EMPLOYEE       TO DB-EMPLOYEE
018200         MOVE EMD-SEQ-NBR        TO DB-DEPENDENT
018300         MOVE HDBSET1-DEPENDENT  TO WS-DB-BEG-RNG
018700         PERFORM 850-FIND-BEGRNG-HDBSET1
018800         IF (HRDEPBEN-FOUND)
                   MOVE 141            TO CRT-MSG-NBR
                   IF (HRDEP-BATCH-PGM)
                       MOVE WS-TRUE    TO HRDEP-WARNING-SW
                       PERFORM 1200-BATCH-MSG
                       THRU    1200-END
                   ELSE
                       MOVE HRDEP-EMD-EMPLOYEE-FN
                                       TO CRT-FIELD-NBR
                       GO TO 230-END.

ACS001****
ACS001     IF (HRDEP-FC                    = "C")
ACS001         MOVE HRDEP-EMD-STUDENT      TO EMD-STUDENT.


HIPAA      IF (HRDEP-EMD-MEDICARE-IND NOT = "A" AND "B" AND "C" AND
53920                                       "D" AND "E" AND "F" AND
53920                                       "G" AND "H" AND "I"
53920                                        AND SPACES)
HIPAA          MOVE WS-TRUE                     TO HRDEP-ERROR-SW
HIPAA          IF (HRDEP-BATCH-PGM)
HIPAA              MOVE 151                     TO CRT-MSG-NBR
HIPAA              MOVE HRDEP-EMD-MEDICARE-IND  TO HRDEP-FIELD-VALUE
HIPAA              PERFORM 1200-BATCH-MSG
HIPAA              THRU    1200-END
HIPAA          ELSE
HIPAA              MOVE 151                        TO CRT-ERROR-NBR
HIPAA              MOVE HRDEP-EMD-MEDICARE-IND-FN  TO CRT-FIELD-NBR
HIPAA              GO TO 230-END.

           IF  (HRDEP-FC                   = "C")
           AND (HRDEP-EMD-STUDENT          NOT = EMD-STUDENT)
           AND (HRDEP-EMD-DISABLED         = "N")
               IF  (EMD-STUDENT            = "Y" OR "F")
               AND (HRDEP-EMD-STUDENT      = "Y" OR "F")
      *----------- If Student Flag is changed from Y to F or vice versa
      *------ no need to process HRDEPBENs because coverage won't change
                   IF (HRDEP-EFF-DT-XMIT           = ZEROES)
                       MOVE 1                      TO HRDEP-EFF-DT-XMIT
052500                 MOVE WS-TRUE                TO HRDEP-ERROR-SW
                       MOVE HRDEP-EFFECT-DATE-FN   TO CRT-FIELD-NBR
                       IF  (HRDEP-CHANGE-COUNTER   = ZEROES)
                       AND (HRDEP-ADD-COUNTER      = ZEROES)
      *------- Warning: Dep Ben will not be updated;Press OK to continue
                           MOVE WS-TRUE            TO HRDEP-ERROR-SW
                           MOVE 163                TO CRT-ERROR-NBR
                           MOVE 1                  TO HRDEP-PT-XMIT-NBR1
                       END-IF
                   END-IF
               ELSE
                   PERFORM 232-EDIT-STUD-STAT-CHG
                   THRU    232-END
                   IF (ERROR-FOUND)
                       GO TO 230-END.                       


053600 230-END.
053700
053800******************************************************************
       232-EDIT-STUD-STAT-CHG.
053800******************************************************************

           MOVE HRDEP-EMD-COMPANY          TO DB-COMPANY.
           MOVE HRDEP-EMD-EMPLOYEE         TO DB-EMPLOYEE.
           MOVE HRDEP-EMD-SEQ-NBR          TO DB-DEPENDENT.
           MOVE HDBSET1-DEPENDENT          TO WS-DB-BEG-RNG.
           PERFORM 850-KFIND-BEGRNG-HDBSET1.
           IF (HRDEPBEN-KNOTFOUND)
               GO TO 232-END.

           IF (HRDEP-EFFECT-DATE           = ZEROES)
      *------- Student flag changed; Must enter effective date
052500         MOVE WS-TRUE                TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE 158                TO CRT-MSG-NBR
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
053100         ELSE
                   MOVE 158                  TO CRT-ERROR-NBR
                   MOVE HRDEP-EFFECT-DATE-FN TO CRT-FIELD-NBR
                   GO TO 232-END.

           IF (HRDEP-CREATE-TRANS          = SPACES)
      *------- Student flag changed; Must enter create transactions
052500         MOVE WS-TRUE                TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE 159                TO CRT-MSG-NBR
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
053100         ELSE
                   MOVE 159                   TO CRT-ERROR-NBR
                   MOVE HRDEP-CREATE-TRANS-FN TO CRT-FIELD-NBR
                   GO TO 232-END.

           IF  (HRDEP-CREATE-TRANS         = "Y")
           AND (HRDEP-HIPAA-REASON         = SPACES)
      *------- Create transaticon = 'Y'; Must enter hipaa reason code
052500         MOVE WS-TRUE                TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE 160                TO CRT-MSG-NBR
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
053100         ELSE
                   MOVE 160                   TO CRT-ERROR-NBR
                   MOVE HRDEP-HIPAA-REASON-FN TO CRT-FIELD-NBR
                   GO TO 232-END.

           IF  (HRDEP-CREATE-TRANS         = "N")
           AND (HRDEP-HIPAA-REASON         NOT = SPACES)
      *------- Cannot enter HIPAA reason when create transaction = 'N'
052500         MOVE WS-TRUE                TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE 161                TO CRT-MSG-NBR
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
053100         ELSE
                   MOVE 161                   TO CRT-ERROR-NBR
                   MOVE HRDEP-HIPAA-REASON-FN TO CRT-FIELD-NBR
                   GO TO 232-END.

           IF (HRDEP-HIPAA-REASON          NOT = SPACES)
               MOVE HRHDB-BT               TO DB-TYPE
               MOVE HRDEP-HIPAA-REASON     TO DB-CODE
               PERFORM 840-FIND-PCOSET1
               IF (PCODES-NOTFOUND)
      *----------- HIPAA reason code {0} does not exist
052500             MOVE WS-TRUE            TO HRDEP-ERROR-SW
052600             IF (HRDEP-BATCH-PGM)
052700                 MOVE 162                TO CRT-MSG-NBR
023300                 MOVE HRDEP-HIPAA-REASON TO HRDEP-FIELD-VALUE
052900                 PERFORM 1200-BATCH-MSG
053000                 THRU    1200-END
053100             ELSE
                       MOVE 162                TO CRT-ERROR-NBR
                       MOVE HRDEP-HIPAA-REASON-FN
                                               TO CRT-FIELD-NBR
                       MOVE HRDEP-HIPAA-REASON TO CRT-ERR-VAR1
                       GO TO 232-END
                   END-IF
               ELSE
                   MOVE HRDEP-HIPAA-REASON TO HRHDB-HIPAA-REASON.

           SET WS-EDIT-ONLY                TO TRUE.

           PERFORM 6000-PROC-STAT-CHG-ON-HDB.
           IF (ERROR-FOUND)
052500         MOVE WS-TRUE                TO HRDEP-ERROR-SW
052600         IF (HRDEP-BATCH-PGM)
052700             MOVE CRT-ERROR-NBR      TO CRT-MSG-NBR
052900             PERFORM 1200-BATCH-MSG
053000             THRU    1200-END
               END-IF
               GO TO 232-END
           ELSE
           IF (HRDEP-EFF-DT-XMIT           = ZEROES)
               MOVE HRDEP-EFFECT-DATE-FN   TO CRT-FIELD-NBR
               IF  (HRDEP-CHANGE-COUNTER   = ZEROES)
               AND (HRDEP-ADD-COUNTER      = ZEROES)
      *------- Warning: Dep Ben will not be updated;Press OK to continue
052600             IF (HRDEP-BATCH-PGM)
                       MOVE WS-TRUE            TO HRDEP-WARNING-SW
                       INITIALIZE CRT-ERROR-NBR
052700                 MOVE 164                TO CRT-MSG-NBR
                       MOVE SPACES             TO HRDEP-FIELD-VALUE
052900                 PERFORM 1200-BATCH-MSG
053000                 THRU    1200-END
                   ELSE
                       MOVE 163                TO CRT-ERROR-NBR
052500                 MOVE WS-TRUE            TO HRDEP-ERROR-SW
                       MOVE 1                  TO HRDEP-EFF-DT-XMIT
                   END-IF
               ELSE
      *---------- Warning: Dep Ben will be updated; Press OK to continue
052600             IF (HRDEP-BATCH-PGM)
                       MOVE WS-TRUE            TO HRDEP-WARNING-SW
                       INITIALIZE CRT-ERROR-NBR
052700                 MOVE 165                TO CRT-MSG-NBR
                       MOVE SPACES             TO HRDEP-FIELD-VALUE
052900                 PERFORM 1200-BATCH-MSG
053000                 THRU    1200-END
                   ELSE
                       MOVE WS-TRUE            TO HRDEP-ERROR-SW
                       MOVE 152                TO CRT-ERROR-NBR
                       MOVE 1                  TO HRDEP-EFF-DT-XMIT
                   END-IF
               END-IF
               GO TO 232-END.

       232-END.

053800******************************************************************
053900 400-PROCESS-TRAN.
054000******************************************************************
054100
           INITIALIZE HRDEP-EFF-DT-XMIT.

J68464     IF  (HRDEP-FC = "A")
J68464         INITIALIZE EMDEPEND.

054200     IF (HRDEP-FC = "I")
054300         PERFORM 480-INQUIRE
054400         THRU    480-END.
054500
054600     IF (HRDEP-UPDATE)
054700         IF (HRDEP-FC = "A")
054800             PERFORM 410-ADD
054900             THRU    410-END
055000         ELSE
055100         IF (HRDEP-FC = "C")
055200             PERFORM 420-CHANGE
055300             THRU    420-END
055400         ELSE
055500         IF (HRDEP-FC = "D")
055600             PERFORM 440-DELETE
055700             THRU    440-END.
055800
055900 400-END.
056000
056100******************************************************************
056200 410-ADD.
056300******************************************************************
056400
056500     IF (NOT HRDEP-BATCH-PGM)
056600         PERFORM 910-AUDIT-BEGIN
056700         IF (DMS-ABORTED)
056800             GO TO 410-END.
056900
057000     PERFORM 840-MODIFY-PEMSET1.
057100     PERFORM 450-UPDATE-COVERAGE-COUNTS
057200     THRU    450-END.
057300     PERFORM 820-STORE-PAEMPLOYEE.
057400
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589         MOVE HRDEP-EMD-COMPANY     TO DB-COMPANY
J83589         MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE
J83589         PERFORM 840-FIND-CEMSET1
J83589         PERFORM 840-MODIFY-CEMSET1
J13204         PERFORM 510-HOLD-IN-CEM-GRP1
J13204         THRU    510-END
J83589         PERFORM 460-UPDATE-DEPENDENT-COUNTS
J83589         THRU    460-END
J13204         PERFORM 515-INEMPLOYEE-HRLOG-GRP1
J13204         THRU    515-END
J83589         PERFORM 820-STORE-INEMPLOYEE
J83589     END-IF.

J02152     PERFORM 600-UPDATE-HISTORY-LOG
           THRU    600-END.
057500 
057500     PERFORM 800-CREATE-EMDEPEND.

AI0080     MOVE HRDEP-EMD-COMPANY         TO DB-COMPANY.
AI0080     MOVE HRDEP-EMD-EMPLOYEE        TO DB-EMPLOYEE.
AI0080     MOVE HRDEP-EMD-SEQ-NBR         TO DB-SEQ-NBR.
AI0080     PERFORM 840-FIND-WBDSET1.
AI0080     IF (WBPDEPEND-FOUND)
AI0080         PERFORM 840-MODIFY-WBDSET1   
AI0080     ELSE
AI0080         PERFORM 800-CREATE-WBPDEPEND.
057600
057700     PERFORM 500-MOVE-HRDEPWS-TO-DB
057800     THRU    500-END.
057900
058000     PERFORM 820-STORE-EMDEPEND.
AI0080     PERFORM 820-STORE-WBPDEPEND.
058100
058200     IF (NOT HRDEP-BATCH-PGM)
058300         PERFORM 920-AUDIT-END.
058400
058500     PERFORM 481-MOVE-TO-HRDEPWS
058600     THRU    481-END.
058700
J07540     IF (HRDEP-EMD-DEP-TYPE = "S" OR "P")
058900         MOVE HRDEP-EMD-COMPANY  TO DB-COMPANY
059000         MOVE HRDEP-EMD-EMPLOYEE TO DB-EMPLOYEE
059100         MOVE ZEROES             TO DB-SEQ-NBR
059200         PERFORM 850-FIND-NLT-EMDSET1
059300         PERFORM
059400             UNTIL (EMDEPEND-NOTFOUND)
059500             OR    (EMD-COMPANY  NOT = DB-COMPANY)
059600             OR    (EMD-EMPLOYEE NOT = DB-EMPLOYEE)
J07540                 IF  (EMD-DEP-TYPE = "S" OR "P")
059800                 AND (EMD-SEQ-NBR  NOT = HRDEP-EMD-SEQ-NBR)
                       AND (EMD-ACTIVE-FLAG  = "A")
059900                    IF (HRDEP-BATCH-PGM)
060000                      MOVE 130                TO CRT-MSG-NBR
060100                      MOVE HRDEP-WARNING-ONLY TO HRDEP-FIELD-VALUE
060200                      PERFORM 1200-BATCH-MSG
060300                      THRU    1200-END
060400                    ELSE
060500                        MOVE 130              TO CRT-ERROR-NBR
060600                    END-IF
060700                 END-IF
060800                 PERFORM 860-FIND-NEXT-EMDSET1
060900         END-PERFORM.
061000
061100     MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE.
061200
061300 410-END.
061400
061500******************************************************************
061600 420-CHANGE.
061700******************************************************************
061800
061900     IF (NOT HRDEP-BATCH-PGM)
062000         PERFORM 910-AUDIT-BEGIN.
062100         IF (DMS-ABORTED)
062200             GO TO 420-END.
062300
062400     MOVE HRDEP-EMD-COMPANY     TO DB-COMPANY.
062500     MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE.
062600     MOVE HRDEP-EMD-SEQ-NBR     TO DB-SEQ-NBR.
062700
062800     PERFORM 840-MODIFY-EMDSET1.

AI0080     MOVE HRDEP-EMD-COMPANY         TO DB-COMPANY.
AI0080     MOVE HRDEP-EMD-EMPLOYEE        TO DB-EMPLOYEE.
AI0080     MOVE HRDEP-EMD-SEQ-NBR         TO DB-SEQ-NBR.
AI0080     PERFORM 840-FIND-WBDSET1.
AI0080     IF (WBPDEPEND-FOUND)
AI0080         PERFORM 840-MODIFY-WBDSET1   
AI0080     ELSE
AI0080         PERFORM 800-CREATE-WBPDEPEND.

062900     PERFORM 840-MODIFY-PEMSET1.
063000     PERFORM 450-UPDATE-COVERAGE-COUNTS
063100     THRU    450-END.
063200     PERFORM 820-STORE-PAEMPLOYEE.
063300
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589         PERFORM 840-FIND-CEMSET1
J83589         PERFORM 840-MODIFY-CEMSET1
J13204         PERFORM 510-HOLD-IN-CEM-GRP1
J13204         THRU    510-END
J83589         PERFORM 460-UPDATE-DEPENDENT-COUNTS
J83589         THRU    460-END
J13204         PERFORM 515-INEMPLOYEE-HRLOG-GRP1
J13204         THRU    515-END
J83589         PERFORM 820-STORE-INEMPLOYEE
J83589     END-IF.

           IF  (HRDEP-EMD-ACTIVE-FLAG  = "I")
           AND (EMD-ACTIVE-FLAG        = "A")
               MOVE HRDEP-EMD-COMPANY  TO DB-COMPANY
               MOVE HRDEP-EMD-EMPLOYEE TO DB-EMPLOYEE
               MOVE HRDEP-EMD-SEQ-NBR  TO DB-DEPENDENT
               MOVE DEPSET1-DEPENDENT  TO WS-DB-BEG-RNG
               PERFORM 830-DELETERNG-DEPSET1.

           IF  (HRDEP-FC                   = "C")
           AND (HRDEP-EMD-STUDENT          NOT = EMD-STUDENT)
           AND (HRDEP-EMD-DISABLED         = "N")
               IF  (EMD-STUDENT            = "Y" OR "F")
               AND (HRDEP-EMD-STUDENT      = "Y" OR "F")
      *----------- If Student Flag is changed from Y to F or vice versa
      *------ no need to process HRDEPBENs because coverage won't change
                   CONTINUE
               ELSE
                   SET WS-UPDATE           TO TRUE

ACS002*            PERFORM 6000-PROC-STAT-CHG-ON-HDB.

J02152     PERFORM 600-UPDATE-HISTORY-LOG
           THRU    600-END.

063400     PERFORM 500-MOVE-HRDEPWS-TO-DB
063500     THRU    500-END.
063600
063700     PERFORM 820-STORE-EMDEPEND.
AI0080     PERFORM 820-STORE-WBPDEPEND.
063800
063900     IF (NOT HRDEP-BATCH-PGM)
064000         PERFORM 920-AUDIT-END.
064100
064200     PERFORM 481-MOVE-TO-HRDEPWS
064300     THRU    481-END.
064400
064500     MOVE CRT-CHG-COMPLETE   TO CRT-MESSAGE.
064600
064700 420-END.
064800
064900******************************************************************
065000 440-DELETE.
065100******************************************************************
065200
065300     IF (NOT HRDEP-BATCH-PGM)
065400         PERFORM 910-AUDIT-BEGIN.
065500         IF (DMS-ABORTED)
065600             GO TO 440-END.
065700
065800     MOVE HRDEP-EMD-COMPANY     TO DB-COMPANY.
065900     MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE.
066000     MOVE HRDEP-EMD-SEQ-NBR     TO DB-SEQ-NBR.
066100
066200     PERFORM 840-MODIFY-EMDSET1.
AI0080     PERFORM 840-MODIFY-WBDSET1.
066300     PERFORM 840-MODIFY-PEMSET1.
066400     PERFORM 450-UPDATE-COVERAGE-COUNTS
066500     THRU    450-END.
066600     PERFORM 820-STORE-PAEMPLOYEE.
066700
J83589     IF (CRT-PROGRAM-CODE = "C611")
J83589         PERFORM 840-FIND-CEMSET1
J83589         PERFORM 840-MODIFY-CEMSET1
J13204         PERFORM 510-HOLD-IN-CEM-GRP1
J13204         THRU    510-END
J83589         PERFORM 460-UPDATE-DEPENDENT-COUNTS
J83589         THRU    460-END
J13204         PERFORM 515-INEMPLOYEE-HRLOG-GRP1
J13204         THRU    515-END
J83589         PERFORM 820-STORE-INEMPLOYEE
J83589     END-IF.

066800     MOVE EMD-COMPANY            TO DB-COMPANY.
066900     MOVE ZEROS                  TO DB-EMP-APP.
067000     MOVE "DP"                   TO DB-CMT-TYPE.
067100     MOVE EMD-EMPLOYEE           TO DB-EMPLOYEE.
067200     MOVE SPACES                 TO DB-JOB-CODE.
067300     MOVE SPACES                 TO DB-ACTION-CODE.
067400     MOVE EMD-SEQ-NBR            TO DB-LN-NBR.
067500     MOVE ZEROS                  TO DB-SEQ-NBR.
067600     PERFORM 850-MODIFY-NLT-PACSET1.
067700     PERFORM
067800         UNTIL (PACOMMENTS-NOTFOUND)
067900         OR    (PAC-COMPANY     NOT = DB-COMPANY)
068000         OR    (PAC-EMP-APP     NOT = DB-EMP-APP)
068100         OR    (PAC-CMT-TYPE    NOT = DB-CMT-TYPE)
068200         OR    (PAC-EMPLOYEE    NOT = DB-EMPLOYEE)
068300         OR    (PAC-JOB-CODE    NOT = DB-JOB-CODE)
068400         OR    (PAC-ACTION-CODE NOT = DB-ACTION-CODE)
068500         OR    (PAC-LN-NBR      NOT = DB-LN-NBR)
068600
068700         PERFORM 830-DELETE-PACOMMENTS
068800         PERFORM 860-MODIFY-NEXT-PACSET1
068900     END-PERFORM.

           MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY.
           MOVE HRDEP-EMD-EMPLOYEE     TO DB-EMPLOYEE.
           MOVE HRDEP-EMD-SEQ-NBR      TO DB-DEPENDENT.
           MOVE DEPSET1-DEPENDENT      TO WS-DB-BEG-RNG.
           PERFORM 830-DELETERNG-DEPSET1.
069000
069100     PERFORM 830-DELETE-EMDEPEND.
AI0080     IF (WBPDEPEND-FOUND)
AI0080        PERFORM 830-DELETE-WBPDEPEND.
069200
J02152     MOVE HRDEP-EMD-SEQ-NBR          TO DB-OBJ-ID.
           MOVE HRHSET1-OBJ-ID             TO WS-DB-BEG-RNG.
           PERFORM 830-DELETERNG-HRHSET1.

069300     IF (NOT HRDEP-BATCH-PGM)
069400         PERFORM 920-AUDIT-END.
069500
069600     MOVE CRT-RECS-DELETED       TO CRT-MESSAGE.
069700
069800 440-END.
069900
070000******************************************************************
070100 450-UPDATE-COVERAGE-COUNTS.
070200******************************************************************
070300
070400     IF  (HRDEP-FC                  = "A")
070500     AND (HRDEP-EMD-HL-COV-FLAG NOT = "Y")
070600     AND (HRDEP-EMD-DN-COV-FLAG NOT = "Y")
070700         GO TO 450-END.
070800
070900     IF  (HRDEP-FC              = "C")
071000     AND (HRDEP-EMD-HL-COV-FLAG = EMD-HL-COV-FLAG)
071100     AND (HRDEP-EMD-DN-COV-FLAG = EMD-DN-COV-FLAG)
071200         GO TO 450-END.
071300
071400     IF  (HRDEP-FC       = "D")
071500     AND (EMD-HL-COV-FLAG NOT = "Y")
071600     AND (EMD-DN-COV-FLAG NOT = "Y")
071700         GO TO 450-END.
071800
071900     IF  (HRDEP-FC              = "A")
072000     AND (HRDEP-EMD-HL-COV-FLAG = "Y")
072100         ADD 1               TO PEM-NBR-HL-DEP.
072200
072300     IF  (HRDEP-FC              = "A")
072400     AND (HRDEP-EMD-DN-COV-FLAG = "Y")
072500         ADD 1               TO PEM-NBR-DN-DEP.
072600
072700     IF  (HRDEP-FC              = "C")
072800     AND (HRDEP-EMD-HL-COV-FLAG NOT = EMD-HL-COV-FLAG)
072900         IF (HRDEP-EMD-HL-COV-FLAG = "Y")
073000             ADD 1               TO PEM-NBR-HL-DEP
073100         ELSE
073200         IF  (EMD-HL-COV-FLAG = "Y")
073300         AND (PEM-NBR-HL-DEP  > ZEROES)
073400             SUBTRACT 1          FROM PEM-NBR-HL-DEP.
073500
073600     IF  (HRDEP-FC              = "C")
073700     AND (HRDEP-EMD-DN-COV-FLAG NOT = EMD-DN-COV-FLAG)
073800         IF (HRDEP-EMD-DN-COV-FLAG = "Y")
073900             ADD 1               TO PEM-NBR-DN-DEP
074000         ELSE
074100         IF  (EMD-DN-COV-FLAG = "Y")
074200         AND (PEM-NBR-DN-DEP  > ZEROES)
074300             SUBTRACT 1          FROM PEM-NBR-DN-DEP.
074400
074500     IF  (HRDEP-FC       = "D")
074600     AND (EMD-HL-COV-FLAG = "Y")
074700     AND (PEM-NBR-HL-DEP  > ZEROES)
074800         SUBTRACT 1              FROM PEM-NBR-HL-DEP.
074900
075000     IF  (HRDEP-FC       = "D")
075100     AND (EMD-DN-COV-FLAG = "Y")
075200     AND (PEM-NBR-DN-DEP  > ZEROES)
075300         SUBTRACT 1              FROM PEM-NBR-DN-DEP.
075400
075500 450-END.
J83589******************************************************************
J83589 460-UPDATE-DEPENDENT-COUNTS.
J83589******************************************************************
J83589     IF  (HRDEP-FC              = "A")
J83589     AND (HRDEP-EMD-STUDENT NOT = "Y")
J83589     AND (HRDEP-EMD-STUDENT NOT = "H")
J83589         GO TO 460-END
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC              = "C")
J83589     AND (HRDEP-EMD-STUDENT = EMD-STUDENT)
J83589         GO TO 460-END
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC        = "D")
J83589     AND (EMD-STUDENT NOT = "Y")
J83589     AND (EMD-STUDENT NOT = "H")
J83589         GO TO 460-END
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC              = "A")
J83589     AND (HRDEP-EMD-STUDENT     = "Y")
J83589         ADD 1               TO CEM-NBR-STD-DEP
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC              = "A")
J83589     AND (HRDEP-EMD-STUDENT     = "H")
J83589         ADD 1               TO CEM-NBR-HOS-DEP
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC              = "C")
J83589     AND (HRDEP-EMD-STUDENT NOT = EMD-STUDENT)
J83589         IF (HRDEP-EMD-STUDENT  = "Y")
J83589             ADD 1               TO CEM-NBR-STD-DEP
J83589         ELSE
J83589         IF  (EMD-STUDENT       = "Y")
J83589         AND (CEM-NBR-STD-DEP  > ZEROES)
J83589             SUBTRACT 1          FROM CEM-NBR-STD-DEP
J83589         END-IF
J83589         END-IF
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC              = "C")
J83589     AND (HRDEP-EMD-STUDENT NOT = EMD-STUDENT)
J83589         IF (HRDEP-EMD-STUDENT = "H")
J83589             ADD 1               TO CEM-NBR-HOS-DEP
J83589         ELSE
J83589         IF  (EMD-STUDENT = "H")
J83589         AND (CEM-NBR-HOS-DEP  > ZEROES)
J83589             SUBTRACT 1          FROM CEM-NBR-HOS-DEP
J83589         END-IF
J83589         END-IF
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC    = "D")
J83589     AND (EMD-STUDENT = "Y")
J83589     AND (CEM-NBR-STD-DEP  > ZEROES)
J83589         SUBTRACT 1              FROM CEM-NBR-STD-DEP
J83589     END-IF.
J83589
J83589     IF  (HRDEP-FC    = "D")
J83589     AND (EMD-STUDENT = "H")
J83589     AND (CEM-NBR-HOS-DEP  > ZEROES)
J83589         SUBTRACT 1              FROM CEM-NBR-HOS-DEP
J83589     END-IF.
J83589
J83589 460-END.
075600
075700******************************************************************
075800 480-INQUIRE.
075900******************************************************************
076000
076100     PERFORM 481-MOVE-TO-HRDEPWS
076200     THRU    481-END.
076300
076400     MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE.
076500
076600 480-END.
076700
076800******************************************************************
076900 481-MOVE-TO-HRDEPWS.
077000******************************************************************
077100
077200     MOVE EMD-COMPANY            TO HRDEP-EMD-COMPANY.
077300     MOVE EMD-EMPLOYEE           TO HRDEP-EMD-EMPLOYEE.
077400     MOVE EMD-SEQ-NBR            TO HRDEP-EMD-SEQ-NBR.
077500     MOVE EMD-EMP-ADDRESS        TO HRDEP-EMD-EMP-ADDRESS.
077600     MOVE EMD-DEP-TYPE           TO HRDEP-EMD-DEP-TYPE.
077700     MOVE EMD-REL-CODE           TO HRDEP-EMD-REL-CODE.
           MOVE EMD-CONSENT            TO HRDEP-EMD-CONSENT.
077800     MOVE EMD-LAST-NAME          TO HRDEP-EMD-LAST-NAME.
077900     MOVE EMD-FIRST-NAME         TO HRDEP-EMD-FIRST-NAME.
078000     MOVE EMD-MIDDLE-INIT        TO HRDEP-EMD-MIDDLE-INIT.
078000     MOVE EMD-LAST-NAME-PRE      TO HRDEP-EMD-LAST-NAME-PRE.
078000     MOVE EMD-NAME-SUFFIX        TO HRDEP-EMD-NAME-SUFFIX.
078100     MOVE EMD-BIRTHDATE          TO HRDEP-EMD-BIRTHDATE.
078200     MOVE EMD-ADOPTION-DATE      TO HRDEP-EMD-ADOPTION-DATE.
078300     MOVE EMD-PLACEMENT-DATE     TO HRDEP-EMD-PLACEMENT-DATE.
078400     MOVE EMD-FICA-NBR           TO HRDEP-EMD-FICA-NBR.
078500     MOVE EMD-SEX                TO HRDEP-EMD-SEX.
078600     MOVE EMD-SMOKER             TO HRDEP-EMD-SMOKER.
078700     MOVE EMD-STUDENT            TO HRDEP-EMD-STUDENT.
078800     MOVE EMD-DISABLED           TO HRDEP-EMD-DISABLED.
J83589     MOVE EMD-DISABILITY-SEV     TO HRDEP-EMD-DISABILITY-SEV.
078900     MOVE EMD-PRIMARY-CARE       TO HRDEP-EMD-PRIMARY-CARE.
079000     MOVE EMD-HL-COV-FLAG        TO HRDEP-EMD-HL-COV-FLAG.
079100     MOVE EMD-DN-COV-FLAG        TO HRDEP-EMD-DN-COV-FLAG.
079200     MOVE EMD-DL-COV-FLAG        TO HRDEP-EMD-DL-COV-FLAG.
079300
079400     MOVE EMD-ADDR1              TO HRDEP-EMD-ADDR1.
079500     MOVE EMD-ADDR2              TO HRDEP-EMD-ADDR2.
079600     MOVE EMD-ADDR3              TO HRDEP-EMD-ADDR3.
079700     MOVE EMD-ADDR4              TO HRDEP-EMD-ADDR4.
079800     MOVE EMD-CITY               TO HRDEP-EMD-CITY.
079900     MOVE EMD-STATE              TO HRDEP-EMD-STATE.
080000     MOVE EMD-ZIP                TO HRDEP-EMD-ZIP.
080100     MOVE EMD-COUNTRY-CODE       TO HRDEP-EMD-COUNTRY-CODE.
080200     MOVE EMD-HM-PHONE-CNTRY     TO HRDEP-EMD-HM-PHONE-CNTRY.
080300     MOVE EMD-HM-PHONE-NBR       TO HRDEP-EMD-HM-PHONE-NBR.
080400     MOVE EMD-WK-PHONE-CNTRY     TO HRDEP-EMD-WK-PHONE-CNTRY.
080500     MOVE EMD-WK-PHONE-NBR       TO HRDEP-EMD-WK-PHONE-NBR.
080600     MOVE EMD-WK-PHONE-EXT       TO HRDEP-EMD-WK-PHONE-EXT.
J81460     MOVE EMD-EMAIL-PERSONAL     TO HRDEP-EMD-EMAIL-PERSONAL.
J12776     MOVE EMD-HICN               TO HRDEP-EMD-HICN.
080600     MOVE EMD-ACTIVE-FLAG        TO HRDEP-EMD-ACTIVE-FLAG.
           MOVE EMD-DECEASED           TO HRDEP-DECEASED.
           MOVE EMD-DEATH-DATE         TO HRDEP-DEATH-DATE.
           MOVE EMD-ESTAB-PATIENT      TO HRDEP-ESTAB-PATIENT.
           MOVE EMD-PRIOR-COV-MO       TO HRDEP-PRIOR-COV-MO.
HIPAA      MOVE EMD-MEDICARE-IND       TO HRDEP-EMD-MEDICARE-IND.
080700
080800     MOVE EMD-BIRTHDATE          TO HRDEP-EMD-BIRTHDATE.
080900     PERFORM 5500-AGE-FROM-BIRTHDATE.

J83494     MOVE EMD-STUDENT-VERIFY     TO HRDEP-VERIFY-DATE.
081000
081100 481-END.
081200
081300******************************************************************
081400 500-MOVE-HRDEPWS-TO-DB.
081500******************************************************************
081600
081700     MOVE HRDEP-EMD-COMPANY         TO EMD-COMPANY.
081800     MOVE HRDEP-EMD-EMPLOYEE        TO EMD-EMPLOYEE.
081900     MOVE HRDEP-EMD-SEQ-NBR         TO EMD-SEQ-NBR.
082000     MOVE HRDEP-EMD-REL-CODE        TO EMD-REL-CODE.
           MOVE HRDEP-EMD-CONSENT         TO EMD-CONSENT.
082100     MOVE HRDEP-EMD-EMP-ADDRESS     TO EMD-EMP-ADDRESS.
082200     MOVE HRDEP-EMD-DEP-TYPE        TO EMD-DEP-TYPE.
082300     MOVE HRDEP-EMD-LAST-NAME       TO EMD-LAST-NAME.
082400     MOVE HRDEP-EMD-FIRST-NAME      TO EMD-FIRST-NAME.
082500     MOVE HRDEP-EMD-MIDDLE-INIT     TO EMD-MIDDLE-INIT.
082500     MOVE HRDEP-EMD-LAST-NAME-PRE   TO EMD-LAST-NAME-PRE.
082500     MOVE HRDEP-EMD-NAME-SUFFIX     TO EMD-NAME-SUFFIX.
082600     MOVE HRDEP-EMD-BIRTHDATE       TO EMD-BIRTHDATE.
082700     MOVE HRDEP-EMD-ADOPTION-DATE   TO EMD-ADOPTION-DATE.
082800     MOVE HRDEP-EMD-PLACEMENT-DATE  TO EMD-PLACEMENT-DATE.
082900     MOVE HRDEP-EMD-FICA-NBR        TO EMD-FICA-NBR.
083000     MOVE HRDEP-EMD-EMP-ADDRESS     TO EMD-EMP-ADDRESS.
083100     MOVE HRDEP-EMD-DEP-TYPE        TO EMD-DEP-TYPE.
083200     MOVE HRDEP-EMD-SEX             TO EMD-SEX.
083300     MOVE HRDEP-EMD-SMOKER          TO EMD-SMOKER.
083400     MOVE HRDEP-EMD-STUDENT         TO EMD-STUDENT.
083500     MOVE HRDEP-EMD-DISABLED        TO EMD-DISABLED.
J83589     MOVE HRDEP-EMD-DISABILITY-SEV  TO EMD-DISABILITY-SEV.
083600     MOVE HRDEP-EMD-PRIMARY-CARE    TO EMD-PRIMARY-CARE.
083700     MOVE HRDEP-EMD-HL-COV-FLAG     TO EMD-HL-COV-FLAG.
083800     MOVE HRDEP-EMD-DN-COV-FLAG     TO EMD-DN-COV-FLAG.
083900     MOVE HRDEP-EMD-DL-COV-FLAG     TO EMD-DL-COV-FLAG.
084000
084100     MOVE HRDEP-EMD-ADDR1           TO EMD-ADDR1.
084200     MOVE HRDEP-EMD-ADDR2           TO EMD-ADDR2.
084300     MOVE HRDEP-EMD-ADDR3           TO EMD-ADDR3.
084400     MOVE HRDEP-EMD-ADDR4           TO EMD-ADDR4.
084500     MOVE HRDEP-EMD-CITY            TO EMD-CITY.
084600     MOVE HRDEP-EMD-STATE           TO EMD-STATE.
084700     MOVE HRDEP-EMD-ZIP             TO EMD-ZIP.
084800     MOVE HRDEP-EMD-COUNTRY-CODE    TO EMD-COUNTRY-CODE.
084900     MOVE HRDEP-EMD-HM-PHONE-CNTRY  TO EMD-HM-PHONE-CNTRY.
085000     MOVE HRDEP-EMD-HM-PHONE-NBR    TO EMD-HM-PHONE-NBR.
085100     MOVE HRDEP-EMD-WK-PHONE-CNTRY  TO EMD-WK-PHONE-CNTRY.
085200     MOVE HRDEP-EMD-WK-PHONE-NBR    TO EMD-WK-PHONE-NBR.
085300     MOVE HRDEP-EMD-WK-PHONE-EXT    TO EMD-WK-PHONE-EXT.
J81460     MOVE HRDEP-EMD-EMAIL-PERSONAL  TO EMD-EMAIL-PERSONAL.
J12776     MOVE HRDEP-EMD-HICN            TO EMD-HICN.

085300     MOVE HRDEP-EMD-ACTIVE-FLAG     TO EMD-ACTIVE-FLAG.
           MOVE HRDEP-DEATH-DATE          TO EMD-DEATH-DATE.
           MOVE HRDEP-PRIOR-COV-MO        TO EMD-PRIOR-COV-MO.  
           MOVE HRDEP-ESTAB-PATIENT       TO EMD-ESTAB-PATIENT. 
           MOVE HRDEP-DECEASED            TO EMD-DECEASED. 
HIPAA      MOVE HRDEP-EMD-MEDICARE-IND    TO EMD-MEDICARE-IND.
085400
           MOVE WS-SYSTEM-DATE-YMD        TO EMD-DATE-STAMP.
           MOVE HHMMSS                    TO EMD-TIME-STAMP.
J68319     IF (HRDEP-EMD-USER-ID NOT = SPACES)
J68319         MOVE HRDEP-EMD-USER-ID     TO EMD-USER-ID
J68319     ELSE            
J68319         MOVE CRT-USER-NAME         TO EMD-USER-ID
J68319     END-IF.

J83494     IF (HRDEP-EMD-STUDENT NOT = "N" OR SPACES)
J83494         MOVE HRDEP-VERIFY-DATE     TO EMD-STUDENT-VERIFY
J83494     ELSE
J83494         INITIALIZE EMD-STUDENT-VERIFY
J83494     END-IF.

AI0080     MOVE HRDEP-EMD-COMPANY         TO WBD-COMPANY.
AI0080     MOVE HRDEP-EMD-EMPLOYEE        TO WBD-EMPLOYEE.
AI0080     MOVE HRDEP-EMD-SEQ-NBR         TO WBD-SEQ-NBR.
AI0080     MOVE HRDEP-DIVORCE-DATE        TO WBD-DIVORCE-DATE.
AI0080     MOVE HRDEP-MARRIAGE-DATE       TO WBD-MARRIAGE-DATE.
AI0080     MOVE HRDEP-DISABLED-DATE       TO WBD-DISABLED-DATE.
AI0080     MOVE HRDEP-STUDENT-DATE        TO WBD-STUDENT-DATE.
AI0080     MOVE WS-SYSTEM-DATE-YMD        TO WBD-DATE-STAMP.
AI0080     MOVE HHMMSS                    TO WBD-TIME-STAMP.
AI0080     MOVE CRT-USER-NAME             TO WBD-USER-ID.
SN0306     MOVE HRDEP-HIC-NUMBER          TO WBD-HIC-NUMBER.
SN0406     MOVE HRDEP-DIS-APR-CDSP        TO WBD-DIS-APR-CDSP.
SN0406     MOVE HRDEP-DIS-APR-CDSP-D      TO WBD-DIS-APR-CDSP-D.
SN0406     MOVE HRDEP-DIS-APR-CHP         TO WBD-DIS-APR-CHP.
SN0406     MOVE HRDEP-DIS-APR-CHP-D       TO WBD-DIS-APR-CHP-D.

085500 500-END.
J13204******************************************************************
J13204 510-HOLD-IN-CEM-GRP1.
J13204******************************************************************
J13204 510-START.
J13204
J13204     INITIALIZE HRDEP-CEM-HOLD-IN-GRP-1.
J13204
J13204     MOVE CEM-NBR-STD-DEP            TO HRDEP-IN-NBR-STD-DEP.
J13204     MOVE CEM-NBR-HOS-DEP            TO HRDEP-IN-NBR-HOS-DEP.
J13204
J13204 510-END.
J13204
J13204******************************************************************
J13204 515-INEMPLOYEE-HRLOG-GRP1.
J13204******************************************************************
J13204 515-START.   
J13204
J13204     INITIALIZE HRLOG-SCR-FIELDS.
J13204
J13204     MOVE HRDEP-EMD-COMPANY          TO DB-COMPANY.
J13204     MOVE HRDEP-EMD-EMPLOYEE         TO DB-EMPLOYEE.
J13204     MOVE HRHSET1-EMPLOYEE           TO WS-DB-BEG-RNG.
J13204     PERFORM 850-FIND-BEGRNG-HRHSET1.
J13204     MOVE ZEROES                     TO IFOBIWS-OBJ-ID.
J13204*** Get the OBJ-ID used for data tagged as "I"
J13204     PERFORM
J13204         UNTIL (HRHISTORY-NOTFOUND)
J13204         OR    (IFOBIWS-OBJ-ID NOT = ZEROES)
J13204         IF  (HRH-OBJ-ID       NOT = ZEROES)
J13204         AND (HRH-DATA-TYPE        = "I")
J13204             MOVE HRH-OBJ-ID         TO IFOBIWS-OBJ-ID
J13204         END-IF
J13204         PERFORM 860-FIND-NXTRNG-HRHSET1
J13204     END-PERFORM.
J13204
J13204     IF ((HRDEP-FC                 = "A")
J60925     AND (CEM-NBR-STD-DEP      NOT = HRDEP-IN-NBR-STD-DEP))
J13204     OR ((HRDEP-FC                 = "C")
J13204     AND (CEM-NBR-STD-DEP      NOT = HRDEP-IN-NBR-STD-DEP))
J13204     OR ((HRDEP-FC                 = "D")
J13204     AND (CEM-NBR-STD-DEP      NOT = HRDEP-IN-NBR-STD-DEP))
J13204         MOVE C6CEM-NBR-STD-DEP-DN   TO HRLOG-FLD-NBR
J13204         PERFORM 530-CHECK-FOR-LOG
J13204         THRU    530-END
J13204         IF (HRDEP-LOG-FLAG        = "X")
J13204*** Log the new NBR-STD-DEP value in HRHISTORY
J13204             MOVE IFOBIWS-OBJ-ID     TO HRLOG-OBJ-ID
J13204*            MOVE CEM-NBR-STD-DEP    TO HRLOG-NEW-VALUE
J60925             MOVE CEM-NBR-STD-DEP    TO HRLOG-NEW-DEC-FIELD
J13204             PERFORM 6000-CREATE-LOG-RECORD
J13204         END-IF
J13204     END-IF.
J13204
J13204     IF ((HRDEP-FC                 = "A")
J90925     AND (CEM-NBR-HOS-DEP      NOT = HRDEP-IN-NBR-HOS-DEP))
J13204     OR ((HRDEP-FC                 = "C")
J13204     AND (CEM-NBR-HOS-DEP      NOT = HRDEP-IN-NBR-HOS-DEP))
J13204     OR ((HRDEP-FC                 = "D")
J13204     AND (CEM-NBR-HOS-DEP      NOT = HRDEP-IN-NBR-HOS-DEP))
J13204         MOVE C6CEM-NBR-HOS-DEP-DN   TO HRLOG-FLD-NBR
J13204         PERFORM 530-CHECK-FOR-LOG
J13204         THRU    530-END
J13204         IF (HRDEP-LOG-FLAG        = "X")
J13204*** Log the new NBR-HOS-DEP value in HRHISTORY
J13204             MOVE IFOBIWS-OBJ-ID     TO HRLOG-OBJ-ID
J13204*            MOVE CEM-NBR-HOS-DEP    TO HRLOG-NEW-VALUE
J60925             MOVE CEM-NBR-HOS-DEP    TO HRLOG-NEW-DEC-FIELD
J13204             PERFORM 6000-CREATE-LOG-RECORD
J13204         END-IF
J13204     END-IF.
J13204                                                                  
J13204 515-END. 
J13204
J13204******************************************************************
J13204 530-CHECK-FOR-LOG.
J13204******************************************************************
J13204 530-START. 
J13204
J13204     MOVE HRDEP-EMD-COMPANY          TO DB-COMPANY.
J13204     INITIALIZE                         DB-COUNTRY-CD-REQ
J13204                                        DB-PROCESS-LEVEL.
J13204     MOVE HRLOG-FLD-NBR              TO DB-FLD-NBR.
J13204     PERFORM 840-FIND-PASSET1.
J13204     IF (PASCRTY-FOUND)
J13204         MOVE PAS-LOG-FLAG           TO HRDEP-LOG-FLAG
J13204         PERFORM 535-INIT-HRLOG
J13204         THRU    535-END 
J13204     ELSE
J13204         MOVE SPACES                 TO HRDEP-LOG-FLAG
J13204     END-IF.
J13204
J13204 530-END.
J13204
J13204******************************************************************
J13204 535-INIT-HRLOG.
J13204******************************************************************
J13204 535-START. 
J13204
J13204*** This data if for INEMPLOYEE changes
J13204     MOVE "I"                        TO HRLOG-DATA-TYPE.
J13204     MOVE HRDEP-EMD-COMPANY          TO HRLOG-COMPANY.
J13204     MOVE HRDEP-EMD-EMPLOYEE         TO HRLOG-EMPLOYEE.
J13204
J13204     IF  (HRLOG-USER-ID            = SPACES)
J13204     AND (HRDEP-EMD-USER-ID    NOT = SPACES)
J13204          MOVE HRDEP-EMD-USER-ID     TO HRLOG-USER-ID
J13204     END-IF.
J13204
J13204     IF  (HRDEP-EFFECT-DATE        = ZEROS)
J13204         MOVE WS-SYSTEM-DATE-YMD     TO HRLOG-EFFECT-DATE
J13204     ELSE
J13204         MOVE HRDEP-EFFECT-DATE      TO HRLOG-EFFECT-DATE
J13204     END-IF.
J13204
J13204 535-END.
085600
      ******************************************************************
       600-UPDATE-HISTORY-LOG.
      ******************************************************************

           MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY.
           MOVE SPACES                 TO DB-PROCESS-LEVEL.
         
           PERFORM 840-FIND-PRSSET1.

           INITIALIZE HRLOG-SCR-FIELDS.
           MOVE HRDEP-EMD-COMPANY      TO HRLOG-COMPANY.
           MOVE HRDEP-EMD-EMPLOYEE     TO HRLOG-EMPLOYEE.
           MOVE "F"                    TO HRLOG-DATA-TYPE.
           MOVE WS-SYSTEM-DATE-YMD     TO HRLOG-EFFECT-DATE.
           MOVE HRDEP-EMD-SEQ-NBR      TO HRLOG-OBJ-ID.
           MOVE 99                     TO HRLOG-POS-LEVEL.
       
           IF (HRDEP-EMD-REL-CODE NOT = EMD-REL-CODE)
               MOVE HREMD-REL-CODE-DN          TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-REL-CODE     TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-LAST-NAME NOT = EMD-LAST-NAME)
               MOVE HREMD-LAST-NAME-DN         TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-LAST-NAME    TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-FIRST-NAME NOT = EMD-FIRST-NAME)
               MOVE HREMD-FIRST-NAME-DN        TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-FIRST-NAME   TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-MIDDLE-INIT NOT = EMD-MIDDLE-INIT)
               MOVE HREMD-MIDDLE-INIT-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-MIDDLE-INIT  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-EMP-ADDRESS NOT = EMD-EMP-ADDRESS)
               MOVE HREMD-EMP-ADDRESS-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-EMP-ADDRESS  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ADDR1 NOT = EMD-ADDR1)
               MOVE HREMD-ADDR1-DN             TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ADDR1        TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ADDR2 NOT = EMD-ADDR2)
               MOVE HREMD-ADDR2-DN             TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ADDR2        TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ADDR3 NOT = EMD-ADDR3)
               MOVE HREMD-ADDR3-DN             TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ADDR3        TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ADDR4 NOT = EMD-ADDR4)
               MOVE HREMD-ADDR4-DN             TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ADDR4        TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-CITY NOT = EMD-CITY)
               MOVE HREMD-CITY-DN              TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-CITY         TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-STATE NOT = EMD-STATE)
               MOVE HREMD-STATE-DN             TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-STATE        TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ZIP NOT = EMD-ZIP)
               MOVE HREMD-ZIP-DN               TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ZIP          TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-FICA-NBR NOT = EMD-FICA-NBR)
               MOVE HREMD-FICA-NBR-DN          TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-FICA-NBR     TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-HM-PHONE-CNTRY NOT = EMD-HM-PHONE-CNTRY)
               MOVE HREMD-HM-PHONE-CNTRY-DN        TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-HM-PHONE-CNTRY   TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-HM-PHONE-NBR NOT = EMD-HM-PHONE-NBR)
               MOVE HREMD-HM-PHONE-NBR-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-HM-PHONE-NBR TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-WK-PHONE-CNTRY NOT = EMD-WK-PHONE-CNTRY)
               MOVE HREMD-WK-PHONE-CNTRY-DN        TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-WK-PHONE-CNTRY   TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-WK-PHONE-NBR NOT = EMD-WK-PHONE-NBR)
               MOVE HREMD-WK-PHONE-NBR-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-WK-PHONE-NBR TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-WK-PHONE-EXT NOT = EMD-WK-PHONE-EXT)
               MOVE HREMD-WK-PHONE-EXT-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-WK-PHONE-EXT TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-BIRTHDATE NOT = EMD-BIRTHDATE)
               MOVE HREMD-BIRTHDATE-DN         TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-BIRTHDATE    TO HRLOG-NEW-DATE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-HL-COV-FLAG NOT = EMD-HL-COV-FLAG)
               MOVE HREMD-HL-COV-FLAG-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-HL-COV-FLAG  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-DN-COV-FLAG NOT = EMD-DN-COV-FLAG)
               MOVE HREMD-DN-COV-FLAG-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-DN-COV-FLAG  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-DL-COV-FLAG NOT = EMD-DL-COV-FLAG)
               MOVE HREMD-DL-COV-FLAG-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-DL-COV-FLAG  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-DEP-TYPE NOT = EMD-DEP-TYPE)
               MOVE HREMD-DEP-TYPE-DN          TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-DEP-TYPE     TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-SEX NOT = EMD-SEX)
               MOVE HREMD-SEX-DN               TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-SEX          TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-STUDENT NOT = EMD-STUDENT)
               MOVE HREMD-STUDENT-DN           TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-STUDENT      TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-DISABLED NOT = EMD-DISABLED)
               MOVE HREMD-DISABLED-DN          TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-DISABLED     TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-SMOKER NOT = EMD-SMOKER)
               MOVE HREMD-SMOKER-DN            TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-SMOKER       TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-PRIMARY-CARE NOT = EMD-PRIMARY-CARE)
               MOVE HREMD-PRIMARY-CARE-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-PRIMARY-CARE TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ADOPTION-DATE NOT = EMD-ADOPTION-DATE)
               MOVE HREMD-EMD-ADOPTION-DATE-DN     TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ADOPTION-DATE    TO HRLOG-NEW-DATE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-PLACEMENT-DATE NOT = EMD-PLACEMENT-DATE)
               MOVE HREMD-EMD-PLACEMENT-DATE-DN    TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-PLACEMENT-DATE   TO HRLOG-NEW-DATE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-NAME-SUFFIX NOT = EMD-NAME-SUFFIX)
               MOVE HREMD-NAME-SUFFIX-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-NAME-SUFFIX  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-LAST-NAME-PRE NOT = EMD-LAST-NAME-PRE)
               MOVE HREMD-LAST-NAME-PRE-DN         TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-LAST-NAME-PRE    TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-COUNTRY-CODE NOT = EMD-COUNTRY-CODE)
               MOVE HREMD-COUNTRY-CODE-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-COUNTRY-CODE TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-CONSENT NOT = EMD-CONSENT)
               MOVE HREMD-EMD-CONSENT-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-CONSENT      TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-ACTIVE-FLAG NOT = EMD-ACTIVE-FLAG)
               MOVE HREMD-DPDNT-STATUS-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-ACTIVE-FLAG  TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-DECEASED NOT = EMD-DECEASED)
               MOVE HREMD-DECEASED-DN          TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-DECEASED         TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-DEATH-DATE NOT = EMD-DEATH-DATE)
               MOVE HREMD-DEATH-DATE-DN        TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-DEATH-DATE       TO HRLOG-NEW-DATE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-ESTAB-PATIENT NOT = EMD-ESTAB-PATIENT)
               MOVE HREMD-ESTAB-PATIENT-DN       TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-ESTAB-PATIENT  TO HRLOG-NEW-DEC-FIELD
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-PRIOR-COV-MO NOT = EMD-PRIOR-COV-MO)
               MOVE HREMD-PRIOR-COV-MO-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-PRIOR-COV-MO TO HRLOG-NEW-DEC-FIELD
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-MEDICARE-IND NOT = EMD-MEDICARE-IND)
               MOVE HREMD-MEDICARE-IND-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-MEDICARE-IND TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-VERIFY-DATE NOT = EMD-STUDENT-VERIFY)
               MOVE HREMD-STUDENT-VERIFY-DN    TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-VERIFY-DATE      TO HRLOG-NEW-DATE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-EMAIL-PERSONAL NOT = EMD-EMAIL-PERSONAL)
               MOVE HREMD-EMAIL-PERSONAL-DN      TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-EMAIL-PERSONAL TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

           IF (HRDEP-EMD-HICN NOT = EMD-HICN)
               MOVE HREMD-EMD-HICN-DN          TO HRLOG-FLD-NBR
               PERFORM 610-CHECK-FOR-LOG
               THRU    610-END
               IF (HRDEP-LOG-FLAG = "X")
                   MOVE HRDEP-EMD-HICN         TO HRLOG-NEW-VALUE
                   PERFORM 6000-CREATE-LOG-RECORD.

       600-END.
      ******************************************************************
       610-CHECK-FOR-LOG.
      ******************************************************************

           MOVE HRDEP-EMD-COMPANY      TO DB-COMPANY.
           INITIALIZE                     DB-COUNTRY-CD-REQ
                                          DB-PROCESS-LEVEL.
           MOVE HRLOG-FLD-NBR          TO DB-FLD-NBR.
           PERFORM 840-FIND-PASSET1.
           IF (PASCRTY-FOUND)
               MOVE PAS-LOG-FLAG       TO HRDEP-LOG-FLAG
           ELSE
               MOVE SPACES             TO HRDEP-LOG-FLAG.

       610-END.
085700******************************************************************
085800 1200-BATCH-MSG.
085900******************************************************************
086000
086100     MOVE "HRDEP"                TO CRT-ERROR-CAT.
086200     PERFORM 790-GET-MSG.
086300     ADD 1                       TO SUB1.
086400     IF (SUB NOT > HRDEP-MSG-TBL-SIZE)
086500         MOVE CRT-MESSAGE        TO HRDEP-BATCH-MSG (SUB1)
086600         MOVE HRDEP-FIELD-VALUE  TO HRDEP-FIELD-VAL (SUB1)
086700         MOVE SPACES             TO HRDEP-FIELD-VALUE.
086800
086900 1200-END.
087000
087100******************************************************************
087200 5000-END.
087300******************************************************************
087400
087500******************************************************************
087600* INPUT:  HRDEP-EMD-BIRTHDATE                                    *
087700*                                                                *
087800* OUTPUT: HRDEP-EMD-CUR-AGE                                      *
087900******************************************************************
088000 5500-AGE-FROM-BIRTHDATE         SECTION.
088100******************************************************************
088200 5500-START.
088300
088400     IF (HRDEP-EMD-BIRTHDATE NOT = ZEROES)
088900         MOVE EMD-BIRTHDATE      TO WSDR-FR-DATE
               PERFORM 5000-CALCULATE-AGE
               MOVE WSDR-NBR-DAYS      TO HRDEP-EMD-CUR-AGE
089800     ELSE
089900         MOVE ZEROES             TO HRDEP-EMD-CUR-AGE.
090000
087300******************************************************************
090100 5500-END.
087300******************************************************************

053800******************************************************************
       6000-PROC-STAT-CHG-ON-HDB           SECTION.
053800******************************************************************
       6000-START.

           INITIALIZE HRDEP-ADD-COUNTER
                      HRDEP-CHANGE-COUNTER.

           MOVE HRDEP-EMD-COMPANY          TO DB-COMPANY.
           MOVE HRDEP-EMD-EMPLOYEE         TO DB-EMPLOYEE.
           MOVE HRDEP-EMD-SEQ-NBR          TO DB-DEPENDENT.
           INITIALIZE DB-PLAN-TYPE.

           INITIALIZE HRDEP-DEP-AGE
                      HRDEP-STUD-AGE.

           MOVE "HL"                       TO DB-PLAN-TYPE.
           INITIALIZE DB-PLAN-CODE
                      DB-EMP-START
                      DB-START-DATE.
           PERFORM 850-FIND-NLT-HDBSET1.
           PERFORM 6100-PROC-HDB-PLAN-TYPE
           THRU    6100-END
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY          NOT = DB-COMPANY)
               OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (HDB-DEPENDENT        NOT = DB-DEPENDENT)
               OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
               OR    (ERROR-FOUND).

           INITIALIZE HRDEP-DEP-AGE
                      HRDEP-STUD-AGE.

           MOVE "DN"                       TO DB-PLAN-TYPE.
           INITIALIZE DB-PLAN-CODE
                      DB-EMP-START
                      DB-START-DATE.
           PERFORM 850-FIND-NLT-HDBSET1.
           PERFORM 6100-PROC-HDB-PLAN-TYPE
           THRU    6100-END
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY          NOT = DB-COMPANY)
               OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (HDB-DEPENDENT        NOT = DB-DEPENDENT)
               OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
               OR    (ERROR-FOUND).

           INITIALIZE HRDEP-DEP-AGE
                      HRDEP-STUD-AGE.

           MOVE "DL"                       TO DB-PLAN-TYPE.
           INITIALIZE DB-PLAN-CODE
                      DB-EMP-START
                      DB-START-DATE.
           PERFORM 850-FIND-NLT-HDBSET1.
           PERFORM 6100-PROC-HDB-PLAN-TYPE
           THRU    6100-END
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY          NOT = DB-COMPANY)
               OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (HDB-DEPENDENT        NOT = DB-DEPENDENT)
               OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
               OR    (ERROR-FOUND).

           GO TO 6000-END.

053800******************************************************************
       6100-PROC-HDB-PLAN-TYPE.
053800******************************************************************

011000     MOVE HDB-COMPANY                TO DB-COMPANY.
011100     MOVE HDB-PLAN-TYPE              TO DB-PLAN-TYPE.
011500     MOVE HDB-PLAN-CODE              TO DB-PLAN-CODE.
011600     PERFORM 840-FIND-PLNSET1.

           INITIALIZE HRHDB-CREATE-TRANS
                      HRHDB-REASON
                      HRHDB-MEMBER-ID.

           IF (HRDEP-CREATE-TRANS          = "Y")
               MOVE PLN-CREATE-TRANS       TO HRHDB-CREATE-TRANS

               IF (HRHDB-CREATE-TRANS      = "Y")
                   MOVE HRHDB-HIPAA-REASON TO HRHDB-REASON
                   MOVE PLN-MEMBER-ID      TO HRHDB-MEMBER-ID.

           MOVE HDB-PLAN-CODE              TO HRDEP-SV-PLAN-CODE.
           MOVE HDB-EMP-START              TO HRDEP-SV-EMP-START.
           MOVE HDB-START-DATE             TO HRDEP-SV-START-DATE.
           MOVE HDB-STOP-DATE              TO HRDEP-SV-STOP-DATE.

           PERFORM 
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY          NOT = DB-COMPANY)
               OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (HDB-DEPENDENT        NOT = DB-DEPENDENT)
               OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
               OR    (HDB-PLAN-CODE        NOT = HRDEP-SV-PLAN-CODE)
               OR    ((HDB-START-DATE      <= HRDEP-EFFECT-DATE)
               AND    (HDB-STOP-DATE       >= HRDEP-EFFECT-DATE))

               MOVE HDB-EMP-START          TO HRDEP-SV-EMP-START
               MOVE HDB-START-DATE         TO HRDEP-SV-START-DATE
               MOVE HDB-STOP-DATE          TO HRDEP-SV-STOP-DATE

               PERFORM 860-FIND-NEXT-HDBSET1
           END-PERFORM.

P78309     PERFORM 850-FIND-NLT-HDBSET1.
           IF (HRDEPBEN-NOTFOUND)
           OR (HDB-COMPANY                 NOT = DB-COMPANY)
           OR (HDB-EMPLOYEE                NOT = DB-EMPLOYEE)
           OR (HDB-DEPENDENT               NOT = DB-DEPENDENT)
           OR (HDB-PLAN-TYPE               NOT = DB-PLAN-TYPE)
           OR (HDB-PLAN-CODE               NOT = HRDEP-SV-PLAN-CODE)
               PERFORM 6200-FIND-BEN-ON-EFF-DT
               THRU    6200-END
               IF (ERROR-FOUND)
                   IF (CRT-ERROR-NBR       = 157)
      *------------- Error 157 is soft error indicating Benefit does not
      *--------------- exist on effective date so read next HRDEPBEN
                       INITIALIZE CRT-ERROR-NBR
                       GO TO 6100-NEXT-HRDEPBEN-PLAN
                   ELSE
                       GO TO 6100-END
                   END-IF
               END-IF
           ELSE
               PERFORM 5100-FIND-BEN-FOR-HDB
               IF (ERROR-FOUND)
                   GO TO 6100-END.

           IF (HRDEP-DEP-AGE               = HRDEP-STUD-AGE)
               GO TO 6100-NEXT-HRDEPBEN-PLAN.

           MOVE "B"                        TO HRDEP-CALC-DEP-N-STUD.
           PERFORM 5200-DEP-AGE-DATE-CALC.

           IF (HRDEP-EMD-STUDENT           = "Y")
               MOVE HRDEP-STUD-AGE-DATE    TO HRDEP-NEW-AGE-DATE
               MOVE HRDEP-DEP-AGE-DATE     TO HRDEP-OLD-AGE-DATE
           ELSE
               MOVE HRDEP-STUD-AGE-DATE    TO HRDEP-OLD-AGE-DATE
               MOVE HRDEP-DEP-AGE-DATE     TO HRDEP-NEW-AGE-DATE.

           IF  (HRDEP-EFFECT-DATE          > HRDEP-OLD-AGE-DATE)
           AND (HRDEP-EFFECT-DATE          > HRDEP-NEW-AGE-DATE)
               GO TO 6100-NEXT-HRDEPBEN-PLAN.

           IF (HRDEP-COVER-TYPE            = "C" OR "R")
               MOVE PTB-START-DATE         TO HRDEP-SV-EMP-START
           ELSE
               MOVE BEN-START-DATE         TO HRDEP-SV-EMP-START.

      *--- If HDB in memory is effective on OldStopDate not need to read
      *--- HDB & BEN
           IF  (HRDEPBEN-FOUND)
           AND (HDB-COMPANY                = DB-COMPANY)
           AND (HDB-EMPLOYEE               = DB-EMPLOYEE)
           AND (HDB-DEPENDENT              = DB-DEPENDENT)
           AND (HDB-PLAN-TYPE              = DB-PLAN-TYPE)
           AND (HDB-PLAN-CODE              = HRDEP-SV-PLAN-CODE)
           AND (HDB-EMP-START              = HRDEP-SV-EMP-START)
           AND (HDB-START-DATE             <= HRDEP-OLD-AGE-DATE)
           AND (HDB-STOP-DATE              >= HRDEP-OLD-AGE-DATE)
               CONTINUE
           ELSE
      *--- Find HDB effective Old Age Date or the last HDB if OldAgeDate
      *------- is in the future, this can happen because the benefit is
      *------- stopped before maturity of the Dep Age
               INITIALIZE DB-EMP-START
                          DB-START-DATE
                          HRDEP-SV-EMP-START
                          HRDEP-SV-START-DATE
                          HRDEP-SV-STOP-DATE
               PERFORM 850-FIND-NLT-HDBSET4
               IF  (HRDEPBEN-FOUND)
               AND (HDB-COMPANY            = DB-COMPANY)
               AND (HDB-EMPLOYEE           = DB-EMPLOYEE)
               AND (HDB-DEPENDENT          = DB-DEPENDENT)
               AND (HDB-PLAN-TYPE          = DB-PLAN-TYPE)
               AND (HDB-PLAN-CODE          = DB-PLAN-CODE)
                   MOVE HDB-STOP-DATE      TO HRDEP-SV-STOP-DATE
                   MOVE HDB-EMP-START      TO HRDEP-SV-EMP-START
                   MOVE HDB-START-DATE     TO HRDEP-SV-START-DATE
               END-IF
               PERFORM
                   UNTIL (HRDEPBEN-NOTFOUND)
                   OR    (HDB-COMPANY      NOT = DB-COMPANY)
                   OR    (HDB-EMPLOYEE     NOT = DB-EMPLOYEE)
                   OR    (HDB-DEPENDENT    NOT = DB-DEPENDENT)
                   OR    (HDB-PLAN-TYPE    NOT = DB-PLAN-TYPE)
                   OR    (HDB-PLAN-CODE    NOT = DB-PLAN-CODE)
J12945             OR    ((HDB-START-DATE  <  HRDEP-OLD-AGE-DATE)
J12945             AND    (HDB-STOP-DATE   >  HRDEP-OLD-AGE-DATE))

                   IF (HDB-STOP-DATE       > HRDEP-SV-STOP-DATE)
                       MOVE HDB-STOP-DATE  TO HRDEP-SV-STOP-DATE
                       MOVE HDB-EMP-START  TO HRDEP-SV-EMP-START
                       MOVE HDB-START-DATE TO HRDEP-SV-START-DATE
                   END-IF

                   PERFORM 860-FIND-NEXT-HDBSET4
               END-PERFORM

               IF (HRDEP-SV-STOP-DATE       NOT = ZEROES)
P81501         OR (HDB-COMPANY              NOT = DB-COMPANY)
P81501         OR (HDB-EMPLOYEE             NOT = DB-EMPLOYEE)
P81501         OR (HDB-DEPENDENT            NOT = DB-DEPENDENT)
P81501         OR (HDB-PLAN-TYPE            NOT = DB-PLAN-TYPE)
P81501         OR (HDB-PLAN-CODE            NOT = DB-PLAN-CODE)
                   MOVE HRDEP-SV-EMP-START  TO DB-EMP-START
                   MOVE HRDEP-SV-START-DATE TO DB-START-DATE
                   PERFORM 850-FIND-NLT-HDBSET1
               END-IF

               PERFORM 5100-FIND-BEN-FOR-HDB
               IF (ERROR-FOUND)
                   GO TO 6100-END.

      *--- ED - Effective Date, ND - New Age Date, OD - Old Age Date
      *--- ED  <  ND  < OD  Reduce
      *--- ND  <  ED  < OD  Reduce
      *--- OD  <  ED  < ND  Increase
      *--- ED  <  OD  < ND  Increase

           INITIALIZE HRDEP-SV-STOP-DATE.

           IF (HRDEP-NEW-AGE-DATE          < HRDEP-OLD-AGE-DATE)
               IF (HRDEP-EFFECT-DATE       > HRDEP-NEW-AGE-DATE)
                   MOVE HRDEP-EFFECT-DATE  TO HRDEP-PROC-DATE
               ELSE
                   MOVE HRDEP-NEW-AGE-DATE TO HRDEP-PROC-DATE
               END-IF

               IF (HDB-STOP-DATE           <= HRDEP-PROC-DATE)
                   GO TO 6100-NEXT-HRDEPBEN-PLAN
               END-IF

               MOVE HRWS-DATE-HIGH-VAL     TO HRDEP-SV-STOP-DATE

               PERFORM 6300-REDUCE-HDB-PERIOD
               THRU    6300-END
                   UNTIL (HRDEPBEN-NOTFOUND)
                   OR    (HDB-COMPANY      NOT = DB-COMPANY)
                   OR    (HDB-EMPLOYEE     NOT = DB-EMPLOYEE)
                   OR    (HDB-DEPENDENT    NOT = DB-DEPENDENT)
                   OR    (HDB-PLAN-TYPE    NOT = DB-PLAN-TYPE)
                   OR    (HDB-PLAN-CODE    NOT = HRDEP-SV-PLAN-CODE)
P48008*            OR    (HRDEP-SV-STOP-DATE  <= HRDEP-PROC-DATE)
           ELSE
               SET WS-FIRST-HDB            TO TRUE

               IF (HRDEP-COVER-TYPE        = "E")
                   PERFORM 6400-INCREASE-HDB-PERIOD
                   THRU    6400-END
                       UNTIL (BENEFIT-NOTFOUND)
                       OR    (BEN-COMPANY     NOT = DB-COMPANY)
                       OR    (BEN-EMPLOYEE    NOT = DB-EMPLOYEE)
                       OR    (BEN-PLAN-TYPE   NOT = DB-PLAN-TYPE)
                       OR    (BEN-PLAN-CODE   NOT = HRDEP-SV-PLAN-CODE)
                       OR    (HRDEP-SV-STOP-DATE  = HRDEP-NEW-AGE-DATE)
               ELSE
               IF (HRDEP-COVER-TYPE        = "R")
                   PERFORM 6500-INCREASE-HDB-PERIOD
                   THRU    6500-END
                       UNTIL (PARTBEN-NOTFOUND)
                       OR    (PTB-COMPANY     NOT = DB-COMPANY)
                       OR    (PTB-EMPLOYEE    NOT = DB-EMPLOYEE)
                       OR    (PTB-PLAN-TYPE   NOT = DB-PLAN-TYPE)
                       OR    (PTB-PLAN-CODE   NOT = HRDEP-SV-PLAN-CODE)
                       OR    (HRDEP-SV-STOP-DATE  = HRDEP-NEW-AGE-DATE)
               ELSE
               IF (HRDEP-COVER-TYPE        = "C")
                   PERFORM 6500-INCREASE-HDB-PERIOD
                   THRU    6500-END
                       UNTIL (PARTBEN-NOTFOUND)
                       OR    (PTB-COMPANY     NOT = DB-COMPANY)
                       OR    (PTB-PARTICIPNT  NOT = DB-PARTICIPNT)
                       OR    (PTB-PLAN-TYPE   NOT = DB-PLAN-TYPE)
                       OR    (PTB-PLAN-CODE   NOT = HRDEP-SV-PLAN-CODE)
                       OR    (HRDEP-SV-STOP-DATE  = HRDEP-NEW-AGE-DATE).

       6100-NEXT-HRDEPBEN-PLAN.
      *--- At this point we have all DB-* fields filled up with correct
      *--- values so not need to reset them
           INITIALIZE DB-EMP-START
                      DB-START-DATE.
           PERFORM 850-FIND-NLT-HDBSET1.
           PERFORM 860-FIND-NEXT-HDBSET1
               UNTIL (HRDEPBEN-NOTFOUND)
               OR    (HDB-COMPANY          NOT = DB-COMPANY)
               OR    (HDB-EMPLOYEE         NOT = DB-EMPLOYEE)
               OR    (HDB-DEPENDENT        NOT = DB-DEPENDENT)
               OR    (HDB-PLAN-TYPE        NOT = DB-PLAN-TYPE)
               OR    (HDB-PLAN-CODE        NOT = HRDEP-SV-PLAN-CODE).

       6100-END.

053800******************************************************************
       6200-FIND-BEN-ON-EFF-DT.
053800******************************************************************

      *--- This means that we don't have HDB effective on EffectDate
      *--- so try finding benefit/partben effective EffectDate
           INITIALIZE HRDEP-COVER-TYPE.

           MOVE HRDEP-EFFECT-DATE          TO DB-START-DATE.
           PERFORM 850-FIND-NLT-BENSET2.
           IF  (BENEFIT-NOTFOUND)
           OR  (BEN-COMPANY                NOT = DB-COMPANY)
           OR  (BEN-PLAN-TYPE              NOT = DB-PLAN-TYPE)
           OR  (BEN-PLAN-CODE              NOT = DB-PLAN-CODE)
           OR  (BEN-EMPLOYEE               NOT = DB-EMPLOYEE)
           OR  ((BEN-STOP-DATE             < HRDEP-EFFECT-DATE)
           AND  (BEN-STOP-DATE             NOT = ZEROES))
               INITIALIZE DB-PARTICIPNT
               PERFORM 850-FIND-NLT-PTBSET2
               IF  (PARTBEN-NOTFOUND)
               OR  (PTB-COMPANY            NOT = DB-COMPANY)
               OR  (PTB-PLAN-TYPE          NOT = DB-PLAN-TYPE)
               OR  (PTB-PLAN-CODE          NOT = DB-PLAN-CODE)
               OR  (PTB-EMPLOYEE           NOT = DB-EMPLOYEE)
               OR  ((PTB-STOP-DATE         < HRDEP-EFFECT-DATE)
               AND  (PTB-STOP-DATE         NOT = ZEROES))
                   SET PARTBEN-NOTFOUND    TO TRUE

                   MOVE HRDEP-EMD-EMPLOYEE TO DB-EMPLOYEE
                   MOVE PARSET3-EMPLOYEE   TO WS-DB-BEG-RNG
                   PERFORM 850-KFIND-BEGRNG-PARSET3
                   PERFORM
                       UNTIL (PARTICIPNT-KNOTFOUND)
                       OR    ((PARTBEN-FOUND)
                       AND    (PTB-COMPANY    = DB-COMPANY)
                       AND    (PTB-PLAN-TYPE  = DB-PLAN-TYPE)
                       AND    (PTB-PLAN-CODE  = DB-PLAN-CODE)
                       AND    (PTB-PARTICIPNT = DB-PARTICIPNT)
                       AND    (PTB-STOP-DATE  >= HRDEP-EFFECT-DATE))

                       IF (PAR-DEPENDENT      = HRDEP-EMD-SEQ-NBR)
                           INITIALIZE DB-EMPLOYEE
                           MOVE PAR-PARTICIPNT  TO DB-PARTICIPNT
                           PERFORM 850-FIND-NLT-PTBSET2
                       END-IF

                       PERFORM 860-KFIND-NXTRNG-PARSET3
                   END-PERFORM
                   MOVE HRDEP-EMD-EMPLOYEE    TO DB-EMPLOYEE
                   IF (PARTICIPNT-KNOTFOUND)
      *--------------- Benefit not found on effective date; Cannot Process
                       MOVE 157                    TO CRT-ERROR-NBR
                       MOVE HRDEP-EMD-STUDENT-FN   TO CRT-FIELD-NBR
                       GO TO 6200-END
                   ELSE
                   IF  (PARTBEN-FOUND)
                   AND (PTB-COMPANY        = DB-COMPANY)
                   AND (PTB-PLAN-TYPE      = DB-PLAN-TYPE)
                   AND (PTB-PLAN-CODE      = DB-PLAN-CODE)
                   AND (PTB-PARTICIPNT     = DB-PARTICIPNT)
                   AND (PTB-STOP-DATE      >= HRDEP-EFFECT-DATE)
                       MOVE "C"            TO HRDEP-COVER-TYPE
                       GO TO 6200-CONTINUE
                   END-IF
                   END-IF
               ELSE
                   MOVE "R"                TO HRDEP-COVER-TYPE
                   GO TO 6200-CONTINUE
               END-IF
           ELSE
               MOVE "E"                    TO HRDEP-COVER-TYPE.

           IF (HRDEP-COVER-TYPE            = SPACES)
               GO TO 6200-END.

       6200-CONTINUE.
           IF (HRDEP-COVER-TYPE            = "C" OR "R")
               MOVE PTB-START-DATE         TO DB-START-DATE
               PERFORM 850-FIND-NLT-PTBSET5
               IF (HRDEP-COVER-TYPE        = "C")
                   MOVE PAR-EMPLOYEE       TO DB-EMPLOYEE
               END-IF
           ELSE
               MOVE BEN-START-DATE         TO DB-START-DATE
               PERFORM 850-FIND-NLT-BENSET4.

           INITIALIZE HRDEP-CALC-AGE-DATE.
           PERFORM 5110-FIND-DEP-N-STUD-AGE.
           IF (ERROR-FOUND)
               GO TO 6200-END.

       6200-END.

053800******************************************************************
       6300-REDUCE-HDB-PERIOD.
053800******************************************************************

           IF (WS-UPDATE)
               MOVE HDB-COMPANY            TO DB-COMPANY
               MOVE HDB-PLAN-TYPE          TO DB-PLAN-TYPE
               MOVE HDB-PLAN-CODE          TO DB-PLAN-CODE
               MOVE HDB-START-DATE         TO DB-START-DATE
               MOVE HDB-EMP-START          TO DB-EMP-START
               MOVE HDB-EMPLOYEE           TO DB-EMPLOYEE
               MOVE HDB-DEPENDENT          TO DB-DEPENDENT
               PERFORM 840-MODIFY-HDBSET2

               IF  (HDB-START-DATE         <= HRDEP-PROC-DATE)
               AND (HDB-STOP-DATE          > HRDEP-PROC-DATE)
                   MOVE HRDEP-PROC-DATE    TO HDB-STOP-DATE
                                              HRDEP-SV-STOP-DATE
               ELSE
               IF (HDB-START-DATE          > HRDEP-PROC-DATE)
                   MOVE HDB-START-DATE     TO HDB-STOP-DATE
                                              HRDEP-SV-STOP-DATE
               END-IF
               END-IF

               MOVE WS-SYSTEM-DATE-YMD     TO HDB-UPD-DATE
               MOVE CRT-USER-NAME          TO HDB-USER-ID
               MOVE HHMMSS                 TO HDB-TIME-STAMP

               PERFORM 820-STORE-HRDEPBEN

               IF (HRHDB-CREATE-TRANS      = "Y")
                   MOVE "C"                TO HRHDB-FC

                   PERFORM 6600-CREATE-HIPAA-TRAN
                   THRU    6600-END
               END-IF
           ELSE
           IF  (HDB-START-DATE             <= HRDEP-PROC-DATE)
           AND (HDB-STOP-DATE              > HRDEP-PROC-DATE)
               MOVE HRDEP-PROC-DATE        TO HRDEP-SV-STOP-DATE
           ELSE
           IF (HDB-START-DATE              > HRDEP-PROC-DATE)
               MOVE HDB-START-DATE         TO HRDEP-SV-STOP-DATE.

           ADD 1                           TO HRDEP-CHANGE-COUNTER.

       6300-NEXT-HRDEPBEN.
P48008     PERFORM 860-FIND-NEXT-HDBSET1.

       6300-END.

053800******************************************************************
       6400-INCREASE-HDB-PERIOD.
053800******************************************************************

           IF (WS-UPDATE)
               MOVE HDB-COMPANY            TO DB-COMPANY
               MOVE HDB-PLAN-TYPE          TO DB-PLAN-TYPE
               MOVE HDB-PLAN-CODE          TO DB-PLAN-CODE
               IF (WS-FIRST-HDB)
                   MOVE HDB-START-DATE     TO DB-START-DATE
                   MOVE HDB-EMP-START      TO DB-EMP-START
P48008             SET WS-NOT-FIRST-HDB    TO TRUE
               ELSE
                   MOVE BEN-START-DATE     TO DB-START-DATE
                                              DB-EMP-START
               END-IF
               MOVE HDB-EMPLOYEE           TO DB-EMPLOYEE
               MOVE HDB-DEPENDENT          TO DB-DEPENDENT
P48008         PERFORM 840-MODIFY-HDBSET2

               IF (HRDEPBEN-NOTFOUND)
P48008             IF  ((BEN-STOP-DATE > HRDEP-EFFECT-DATE)
P48008             OR  (BEN-STOP-DATE = ZEROES))
P48008                 IF  (PLN-COVERAGE-TYPE = 1)
P48008                     MOVE BEN-COV-OPTION        TO DB-COVERAGE-OPT
P48008                     PERFORM 840-FIND-COPSET1
P48008                     IF  (BNCOVOPT-NOTFOUND)
P48008                     OR  (COP-COV-DEPENDENTS NOT = "D" AND "B")
P48008                         MOVE BEN-COMPANY       TO HDB-COMPANY
P48008                         MOVE BEN-EMPLOYEE      TO HDB-EMPLOYEE
P48008                         MOVE HRDEP-EMD-SEQ-NBR TO HDB-DEPENDENT
P48008                         MOVE BEN-PLAN-TYPE     TO HDB-PLAN-TYPE
P48008                         MOVE BEN-PLAN-CODE     TO HDB-PLAN-CODE
P48008                         GO TO 6400-NEXT-BENEFIT
P48008                     END-IF
P48008                 END-IF
P48008                 IF  (PLN-COVERAGE-TYPE = 2)
P48008                     MOVE HRDEP-COVER-TYPE      TO DB-COVER-TYPE
P48008                     MOVE BEN-COV-UPD-DT        TO DB-START-DATE
P48008                     MOVE BEN-COV-GROUP         TO DB-GROUP-NAME
P48008                     PERFORM 840-FIND-CVRSET1
P48008                     IF (BNCOVERAGE-NOTFOUND)
P48008                         MOVE BEN-COMPANY       TO HDB-COMPANY
P48008                         MOVE BEN-EMPLOYEE      TO HDB-EMPLOYEE
P48008                         MOVE HRDEP-EMD-SEQ-NBR TO HDB-DEPENDENT
P48008                         MOVE BEN-PLAN-TYPE     TO HDB-PLAN-TYPE
P48008                         MOVE BEN-PLAN-CODE     TO HDB-PLAN-CODE
P48008                         GO TO 6400-NEXT-BENEFIT
P48008                     END-IF
P48008                 END-IF
P48008                 MOVE "A"                       TO HRHDB-FC
P48008                 PERFORM 6410-CREATE-HRDEPBEN
P48008                 THRU    6410-END
P48008                 IF  (BEN-STOP-DATE = ZEROES)
P48008                 OR  (BEN-STOP-DATE > HRDEP-NEW-AGE-DATE)
P48008                     MOVE HRDEP-NEW-AGE-DATE TO HDB-STOP-DATE
P48008                                                HRDEP-SV-STOP-DATE
P48008                  ELSE
P48008                     MOVE BEN-STOP-DATE      TO HDB-STOP-DATE
P48008                                                HRDEP-SV-STOP-DATE
P48008                 END-IF
P48008             ELSE
P48008                 MOVE BEN-COMPANY            TO HDB-COMPANY
P48008                 MOVE BEN-EMPLOYEE           TO HDB-EMPLOYEE
P48008                 MOVE HRDEP-EMD-SEQ-NBR      TO HDB-DEPENDENT
P48008                 MOVE BEN-PLAN-TYPE          TO HDB-PLAN-TYPE
P48008                 MOVE BEN-PLAN-CODE          TO HDB-PLAN-CODE
P48008                 GO TO 6400-NEXT-BENEFIT
P48008             END-IF
               ELSE
P48008             IF  ((BEN-STOP-DATE > HRDEP-EFFECT-DATE)
P48008             OR  (BEN-STOP-DATE = ZEROES))
P48008                 IF  (HRDEP-EFFECT-DATE  > HDB-STOP-DATE)
P48008                 AND (HRDEP-EFFECT-DATE  < HRDEP-NEW-AGE-DATE)
P48008                 AND (HRDEP-EMD-STUDENT  = "Y")
P48008                     IF  (PLN-COVERAGE-TYPE = 1)
P48008                         MOVE BEN-COV-OPTION TO DB-COVERAGE-OPT
P48008                         PERFORM 840-FIND-COPSET1
P48008                         IF (BNCOVOPT-NOTFOUND)
P48008                         OR (COP-COV-DEPENDENTS NOT = "D" AND "B")
P48008                            GO TO 6400-NEXT-BENEFIT
P48008                        END-IF
P48008                     END-IF
P48008                     IF  (PLN-COVERAGE-TYPE = 2)
P48008                         MOVE HRDEP-COVER-TYPE TO DB-COVER-TYPE
P48008                         MOVE BEN-COV-UPD-DT   TO DB-START-DATE
P48008                         MOVE BEN-COV-GROUP    TO DB-GROUP-NAME
P48008                         PERFORM 840-FIND-CVRSET1
P48008                         IF  (BNCOVERAGE-NOTFOUND)
P48008                            GO TO 6400-NEXT-BENEFIT
P48008                         END-IF
P48008                     END-IF
P48008                     MOVE "A"                  TO HRHDB-FC
P48008                     PERFORM 6410-CREATE-HRDEPBEN
P48008                     THRU    6410-END
P48008                     IF  (BEN-STOP-DATE = ZEROES)
P48008                     OR  (BEN-STOP-DATE > HRDEP-NEW-AGE-DATE)
P48008                         MOVE HRDEP-NEW-AGE-DATE
P48008                                             TO HDB-STOP-DATE
P48008                                                HRDEP-SV-STOP-DATE
P48008                     ELSE
P48008                         MOVE BEN-STOP-DATE  TO HDB-STOP-DATE
P48008                                                HRDEP-SV-STOP-DATE
P48008                     END-IF
P48008                 ELSE
P48008                     IF  (HRDEP-EFFECT-DATE < HDB-STOP-DATE)
P48008                     AND (HRDEP-EFFECT-DATE < HRDEP-NEW-AGE-DATE)
P48008                         PERFORM 850-FIND-NLT-HDBSET1
P48008                         PERFORM 860-FIND-NEXT-HDBSET1 
P48008                         IF  (HRDEPBEN-FOUND)
P48008                         AND (HDB-COMPANY    = DB-COMPANY)
P48008                         AND (HDB-PLAN-TYPE  = DB-PLAN-TYPE)
P48008                         AND (HDB-PLAN-CODE  = DB-PLAN-CODE)
P48008                         AND (HDB-EMPLOYEE   = DB-EMPLOYEE)
P48008                         AND (HDB-DEPENDENT  = DB-DEPENDENT)
P48008                             GO TO 6400-NEXT-BENEFIT
P48008                         ELSE
P48008                             PERFORM 850-FIND-NLT-BENSET4
P48008                             PERFORM 860-FIND-NEXT-BENSET4
P48008                             IF  (BENEFIT-FOUND)
P48008                             AND (BEN-COMPANY    = DB-COMPANY)
P48008                             AND (BEN-PLAN-TYPE  = DB-PLAN-TYPE)
P48008                             AND (BEN-PLAN-CODE  = DB-PLAN-CODE)
P48008                             AND (BEN-EMPLOYEE   = DB-EMPLOYEE)
P48008                                 PERFORM 870-FIND-PREV-BENSET4 
P48008                                 PERFORM 870-FIND-PREV-HDBSET1
P48008                                GO TO 6400-NEXT-BENEFIT
P48008                             END-IF
P48008                             PERFORM 870-FIND-PREV-HDBSET1
P48008                             MOVE HDB-COMPANY    TO DB-COMPANY
P48008                             MOVE HDB-PLAN-TYPE  TO DB-PLAN-TYPE
P48008                             MOVE HDB-PLAN-CODE  TO DB-PLAN-CODE
P48008                             MOVE HDB-START-DATE TO DB-START-DATE
P48008                             MOVE HDB-EMP-START  TO DB-EMP-START
P48008                             MOVE HDB-EMPLOYEE   TO DB-EMPLOYEE
P48008                             MOVE HDB-DEPENDENT  TO DB-DEPENDENT
P48008                             PERFORM 840-MODIFY-HDBSET2
P48008                             MOVE "C"            TO HRHDB-FC
P48008                             MOVE HRDEP-NEW-AGE-DATE 
P48008                                                 TO HDB-STOP-DATE
P48008                         END-IF
P48008                     END-IF 
P48008                 END-IF
P48008             ELSE
P48008                 GO TO 6400-NEXT-BENEFIT 
P48008             END-IF
               END-IF

               MOVE WS-SYSTEM-DATE-YMD     TO HDB-UPD-DATE
               MOVE CRT-USER-NAME          TO HDB-USER-ID
               MOVE HHMMSS                 TO HDB-TIME-STAMP

               PERFORM 820-STORE-HRDEPBEN

               IF (HRHDB-CREATE-TRANS      = "Y")
                   PERFORM 6600-CREATE-HIPAA-TRAN
                   THRU    6600-END
               END-IF
           ELSE
           IF (BEN-STOP-DATE               = ZEROES)
           OR (BEN-STOP-DATE               > HRDEP-NEW-AGE-DATE)
               MOVE HRDEP-NEW-AGE-DATE     TO HRDEP-SV-STOP-DATE
           ELSE
               MOVE BEN-STOP-DATE          TO HRDEP-SV-STOP-DATE.

               ADD 1                       TO HRDEP-ADD-COUNTER.

       6400-NEXT-BENEFIT.
           PERFORM 860-FIND-NEXT-BENSET4.

       6400-END.


087300******************************************************************
       6410-CREATE-HRDEPBEN.
087300******************************************************************

            PERFORM 800-CREATE-HRDEPBEN.

            MOVE BEN-COMPANY               TO HDB-COMPANY.
            MOVE BEN-EMPLOYEE              TO HDB-EMPLOYEE.
            MOVE HRDEP-EMD-SEQ-NBR         TO HDB-DEPENDENT.
            MOVE BEN-PLAN-TYPE             TO HDB-PLAN-TYPE.
            MOVE BEN-PLAN-CODE             TO HDB-PLAN-CODE.
P48008      IF  (HRDEP-FC = "C")
P48008      AND (HRDEP-EFFECT-DATE > BEN-START-DATE)
P48008           MOVE HRDEP-EFFECT-DATE    TO HDB-START-DATE
P48008           MOVE BEN-START-DATE       TO HDB-EMP-START
P48008      ELSE
            MOVE BEN-START-DATE            TO HDB-EMP-START
                                              HDB-START-DATE.

            MOVE WS-SYSTEM-DATE-YMD        TO HDB-CREATION-DATE.
J67329      MOVE HHMMSS                    TO HDB-CREATE-TIME.
J67329      MOVE CRT-USER-NAME             TO HDB-CREATE-USER-ID.   

       6410-END.

053800******************************************************************
       6500-INCREASE-HDB-PERIOD.
053800******************************************************************

           IF (WS-UPDATE)
               MOVE HDB-COMPANY            TO DB-COMPANY
               MOVE HDB-PLAN-TYPE          TO DB-PLAN-TYPE
               MOVE HDB-PLAN-CODE          TO DB-PLAN-CODE
               IF (WS-FIRST-HDB)
                   MOVE HDB-START-DATE     TO DB-START-DATE
                   MOVE HDB-EMP-START      TO DB-EMP-START
               ELSE
                   MOVE PTB-START-DATE     TO DB-START-DATE
                                              DB-EMP-START
               END-IF
               MOVE HDB-EMPLOYEE           TO DB-EMPLOYEE
               MOVE HDB-DEPENDENT          TO DB-DEPENDENT
               PERFORM 840-MODIFY-HDBSET2

               IF (HRDEPBEN-NOTFOUND)
                   MOVE "A"                TO HRHDB-FC

                   PERFORM 6510-CREATE-HRDEPBEN
                   THRU    6510-END
               ELSE
                   MOVE "C"                TO HRHDB-FC
               END-IF

               IF (PTB-STOP-DATE           = ZEROES)
               OR (PTB-STOP-DATE           > HRDEP-NEW-AGE-DATE)
                   MOVE HRDEP-NEW-AGE-DATE TO HDB-STOP-DATE
               ELSE
                   MOVE PTB-STOP-DATE      TO HDB-STOP-DATE
               END-IF

               MOVE WS-SYSTEM-DATE-YMD     TO HDB-UPD-DATE
               MOVE CRT-USER-NAME          TO HDB-USER-ID
               MOVE HHMMSS                 TO HDB-TIME-STAMP

               PERFORM 820-STORE-HRDEPBEN

               IF (HRHDB-CREATE-TRANS      = "Y")
                   PERFORM 6600-CREATE-HIPAA-TRAN
                   THRU    6600-END.

           IF (PTB-STOP-DATE               = ZEROES)
           OR (PTB-STOP-DATE               > HRDEP-NEW-AGE-DATE)
               MOVE HRDEP-NEW-AGE-DATE     TO HRDEP-SV-STOP-DATE
           ELSE
               MOVE PTB-STOP-DATE          TO HRDEP-SV-STOP-DATE.

           IF (WS-FIRST-HDB)
               ADD 1                       TO HRDEP-CHANGE-COUNTER
               SET WS-NOT-FIRST-HDB        TO TRUE
           ELSE
               ADD 1                       TO HRDEP-ADD-COUNTER.

       6500-NEXT-PARTBEN.
           PERFORM 860-FIND-NEXT-PTBSET5.

       6500-END.

087300******************************************************************
       6510-CREATE-HRDEPBEN.
087300******************************************************************

            PERFORM 800-CREATE-HRDEPBEN.

            MOVE PTB-COMPANY               TO HDB-COMPANY.
            MOVE HRDEP-EMD-EMPLOYEE        TO HDB-EMPLOYEE.
            MOVE HRDEP-EMD-SEQ-NBR         TO HDB-DEPENDENT.
            MOVE PTB-PLAN-TYPE             TO HDB-PLAN-TYPE.
            MOVE PTB-PLAN-CODE             TO HDB-PLAN-CODE.
            MOVE PTB-START-DATE            TO HDB-EMP-START
                                              HDB-START-DATE.

            MOVE WS-SYSTEM-DATE-YMD        TO HDB-CREATION-DATE.
J67329      MOVE HHMMSS                    TO HDB-CREATE-TIME.
J67329      MOVE CRT-USER-NAME             TO HDB-CREATE-USER-ID.    

       6510-END.

087300******************************************************************
       6600-CREATE-HIPAA-TRAN.
087300******************************************************************

           MOVE HDB-COMPANY                TO HRHDB-COMPANY.
           MOVE HDB-PLAN-TYPE              TO HRHDB-PLAN-TYPE.
           MOVE HDB-PLAN-CODE              TO HRHDB-PLAN-CODE.

           MOVE HRDEP-COVER-TYPE           TO HRHDB-COVER-TYPE.

124100     IF (HRDEP-COVER-TYPE            = "C")
               MOVE PTB-PARTICIPNT         TO HRHDB-PARTICIPNT
           ELSE
               MOVE HDB-EMPLOYEE           TO HRHDB-EMPLOYEE.

           MOVE HDB-DEPENDENT              TO HRHDB-DEPENDENT.
           MOVE HDB-START-DATE             TO HRHDB-START-DATE.

           INITIALIZE HRHDB-TRAN-SEQ-NBR.

           MOVE HDB-STOP-DATE              TO HRHDB-STOP-DATE.

           MOVE HDB-EMP-START              TO HRHDB-EMP-START.

           PERFORM 5100-CREATE-HRUTILITY.

       6600-END.

087300******************************************************************
       6000-END.
087300******************************************************************
