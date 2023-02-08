******* BNPE70PD 7 <490590374>
000100******************************************************************
000200*                         (BNPE70PD)                   NOV 15-95 *
000300*                                                                *
000400*       P L A N   E L I G I B I L I T Y   E D I T   R T N        *
000500*                                                                *
000600******************************************************************
000700*                                                                *
000800* RECORDS REQUIRED IN MEMORY : PLAN, EMPLOYEE, FLEXPLAN          *
000900*                                                                *
001000* INPUT PARAMETERS        :                                      *
001100*                                                                *
001200* BNPEWS-COMPANY          : Company                   (REQUIRED) *
001300* BNPEWS-PLAN-TYPE        : Plan Type                 (REQUIRED) *
001400* BNPEWS-PLAN-CODE        : Plan Code                 (REQUIRED) *
001500* BNPEWS-EMPLOYEE         : Employee                  (REQUIRED) *
001600* BNPEWS-START-DATE       : Start Date                (REQUIRED) *
001700* BNPEWS-COVER-TYPE       : Cover Type                (REQUIRED) *
001800* Valid Values            : "E"mployee                           *
001900*                           "C"obra                              *
002000*                           "R"etiree                            *
002100*                                                                *
002200******************************************************************
002300*                                                                *
002400* OUTPUT                  : BNPEWS-ELIGIBLE-SW                   *
002500*                               0 = NOT-ELIGIBLE      (88-LEVEL) *
002600*                               1 = ELIGIBLE          (88-LEVEL) *
002700*                           BNPEWS-ERROR-NBR                     *
002800*                           BNPEWS-ERR-VAR1                      *
002900*                           BNPEWS-ERR-VAR2                      *
003000*                           BNPEWS-ERR-VAR3                      *
003100*                                                                *
003200******************************************************************
      ******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  ------   ------   ------------------------------------------  *
      *  ACS01  | ACS01  | CHANGES DUE TO UPGRADEX FROM CPSPRD9 VER    *
      *         |        | CHANGED 5200-EDIT-POST-CODE-TBL             *
      *  ------   ------   ------------------------------------------  *
003300******************************************************************
003400 5000-EDIT-GROUP-N-ZIP            SECTION.
003500******************************************************************
003600 5000-START.
003700
003800     SET NOT-ELIGIBLE            TO TRUE.
003900
004000     IF  (BNPEWS-COVER-TYPE      = "E")
004100     AND (PLN-EMP-COVERED        NOT = "Y")
004200******** Plan does not cover employees
004300         MOVE 105                TO BNPEWS-ERROR-NBR
004400         MOVE BNPEWS-PLAN-TYPE   TO BNPEWS-ERR-VAR1
004500         MOVE BNPEWS-PLAN-CODE   TO BNPEWS-ERR-VAR2
004600         GO TO 5000-END.
004700
004800     IF  (BNPEWS-COVER-TYPE      = "R")
004900     AND (PLN-RET-COVERED        NOT = "Y")
005000******** Plan does not cover retirees
005100         MOVE 108                TO BNPEWS-ERROR-NBR
005200         MOVE BNPEWS-PLAN-TYPE   TO BNPEWS-ERR-VAR1
005300         MOVE BNPEWS-PLAN-CODE   TO BNPEWS-ERR-VAR2
005400         GO TO 5000-END.
005500
005600     IF  ((BNPEWS-COVER-TYPE     = "E")
005700     AND  (PLN-GROUP-NAME        NOT = SPACES))
005800     OR  ((BNPEWS-COVER-TYPE     = "R")
005900     AND  (PLN-RET-GROUP-NAME    NOT = SPACES))
006000         PERFORM 5100-EDIT-GROUP-NAME
006100         THRU    5100-END
006200
006300         IF (BNPE-ERROR-FOUND)
006400             GO TO 5000-END.
006500
006600     IF  (PLN-POST-CODE-TBL      NOT = SPACES)
           AND (BNPEWS-COVER-TYPE      = "E")
006700         PERFORM 5200-EDIT-POST-CODE-TBL
006800         THRU    5200-END
006900
007000         IF (BNPE-ERROR-FOUND)
007100             GO TO 5000-END.
007200
006700     PERFORM 5300-EDIT-CURR-CNTRY-CODE
006800     THRU    5300-END.
007000     IF (BNPE-ERROR-FOUND)
007100         GO TO 5000-END.
007200
007300     SET ELIGIBLE                TO TRUE.
007400
007500     GO TO 5000-END.
007600
007700******************************************************************
007800 5100-EDIT-GROUP-NAME.
007900******************************************************************
008000
008100     MOVE BNPEWS-COMPANY         TO DB-COMPANY.
008200     IF (BNPEWS-COVER-TYPE       = "R")
008300         MOVE PLN-RET-GROUP-NAME TO DB-GROUP-NAME
008400     ELSE
008500         MOVE PLN-GROUP-NAME     TO DB-GROUP-NAME.
008600     MOVE BNPEWS-EMPLOYEE        TO DB-EMPLOYEE.
008700     PERFORM 840-FIND-PGESET1.
008800     IF (PGEMPLOYEE-NOTFOUND)
008900******** Employee does not exist in group
009000         MOVE 102                TO BNPEWS-ERROR-NBR
009100         MOVE BNPEWS-EMPLOYEE    TO BNPEWS-BWZ-EMP-NBR
009200         MOVE BNPEWS-BWZ-EMP-NBR TO BNPEWS-ERR-VAR1
009300         MOVE PLN-GROUP-NAME     TO BNPEWS-ERR-VAR2
009400         GO TO 5100-END.
009500
009600 5100-END.
009700
009800******************************************************************
009900 5200-EDIT-POST-CODE-TBL.
010000******************************************************************
010100
ACS01 *    Commenting out this paragraph and replacing it with CPS custom
ACS01 *    code.

010200*    MOVE PEM-WORK-ZIP           TO BNPEWS-EMP-POSTAL-CODE.
010300*
010400*    MOVE PLN-POST-CODE-TBL      TO DB-POST-CODE-TBL.
010500*    MOVE BNPEWS-EMP-POST-CODE   TO DB-POSTAL-CODE.
010600*    PERFORM 850-KFIND-NLT-BPCSET1.
010700*    IF  (BNPOSTCODE-KFOUND)
010800*    AND (BPC-POST-CODE-TBL      = PLN-POST-CODE-TBL)
010900*        MOVE BPC-POSTAL-CODE    TO BNPEWS-BPC-POSTAL-CODE.
011000*
011100*    IF (BNPOSTCODE-KNOTFOUND)
011200*    OR (BPC-POST-CODE-TBL       NOT = PLN-POST-CODE-TBL)
011300*    OR (BNPEWS-BPC-POST-CODE    NOT = BNPEWS-EMP-POST-CODE)
011400******** Emp post code does not exist in table
011500*        MOVE 104                TO BNPEWS-ERROR-NBR
011600*        MOVE PEM-WORK-ZIP       TO BNPEWS-ERR-VAR1
011700*        MOVE PLN-POST-CODE-TBL  TO BNPEWS-ERR-VAR2
011800*        GO TO 5200-END.

ACS01 *    Begin CPS custom code
010200     MOVE PEM-WORK-ZIP           TO BNPEWS-EMP-POSTAL-CODE.

010300*** GW 1/2007 REWRITE THIS BECAUSE HAVING TROUBLE WITH >1 OPEN
      ****     SPAN ON BNPOSTCODE DUE TO HMO'S

010400**** MOVE PLN-POST-CODE-TBL      TO DB-POST-CODE-TBL.
RAY   *--- 2003/11/05 --------------------------------------------------
RAY   *--- AIC ALSO CHGED BNP INDEXES - ADDED INS-CARRIER & START-DATE
RAY   *--- TO BNPSET1 AND A NEW BNPSET2 USING SAME FLDS IN DIFFERENT
RAY   *--- ORDER. THEY FORGOT TO POPULATE INS-CARIER ON LOOKUP 
RAY   ***  MOVE PLN-INS-CARRIER        TO DB-INS-CARRIER.
RAY   *------------------------------------------------------ 2003/11/05
010500**** MOVE BNPEWS-EMP-POST-CODE   TO DB-POSTAL-CODE.
AI0010**** MOVE BNPEWS-START-DATE      TO DB-START-DATE.
AI0010***  PERFORM 850-KFIND-NLT-BPCSET1.
RAY   *--- 2004/02/03 --------------------------------------------------
RAY   *--- THE "KFIND" IS SUPPOSED TO JUST RETURN THE FOUND OR 
RAY   *--- OR NOTFOUND FLAG. TO CHECK RECORD DATA, USE "FIND".
GORDON***  CHANGED ERROR-NBR 104 BELOW TO GIVE MORE INFO TO DEBUG
GORDON***  ALSO ADDED ERROR MSG 501 TO GIVE MORE INFO TO DEBUG
GORDON***  ALSO ADDED ERROR MSG 500 WHICH AIC FORGOT
GORDON***  ALSO CHANGED SOME "BPC-KFOUND"S TO "FOUND"S
RAY   *    PERFORM 850-KFIND-NLT-BPCSET2.
RAY   **** PERFORM 850-FIND-NLT-BPCSET2.
RAY   *------------------------------------------------------ 2004/02/03


           MOVE "N"                    TO WS-BPC-FOUND.

010500     MOVE BNPEWS-EMP-POST-CODE   TO DB-POSTAL-CODE.
AI0010*****MOVE BNPEWS-START-DATE      TO DB-START-DATE.
0615       MOVE WS-HIGH-VALUES         TO DB-START-DATE.
           INITIALIZE                     DB-INS-CARRIER.
010400     INITIALIZE                     DB-POST-CODE-TBL.
            
RAY        PERFORM 850-FIND-NLT-BPCSET2.

0615       IF (BPC-POSTAL-CODE > BNPEWS-EMP-POST-CODE)
0615         MOVE BNPEWS-EMP-POST-CODE   TO DB-POSTAL-CODE 
0615         MOVE WS-SYSTEM-DATE-YMD     TO DB-START-DATE 
             INITIALIZE                     DB-INS-CARRIER 
0615         INITIALIZE                     DB-POST-CODE-TBL 
RAY          PERFORM 850-FIND-NLT-BPCSET2 
0615********  PERFORM 870-FIND-PREV-BPCSET2
0615       END-IF.
           PERFORM
           UNTIL (BNPOSTCODE-NOTFOUND)
              OR (BPC-POSTAL-CODE NOT = BNPEWS-EMP-POST-CODE)
              OR (WS-BPC-FOUND = "Y")
                DISPLAY "BNPEWS-START-DATE " BNPEWS-START-DATE
HCR16            IF  (BPC-START-DATE <= BNPEWS-START-DATE)
HCR16            AND ((BPC-STOP-DATE >  BNPEWS-START-DATE)
0710***********  IF  (BPC-START-DATE <= BNB-START-DATE)
0710***********  AND ((BPC-STOP-DATE >  BNB-START-DATE)
                  OR  (BPC-STOP-DATE = ZERO))
                 AND (BPC-INS-CARRIER = PLN-INS-CARRIER)
                 AND (BPC-POST-CODE-TBL = PLN-POST-CODE-TBL)
                    MOVE   "Y"          TO WS-BPC-FOUND
                 ELSE
                    PERFORM 860-FIND-NEXT-BPCSET2
                 END-IF
           END-PERFORM.
011000
GW         IF (WS-BPC-FOUND = "N")
GW             MOVE 104                TO BNPEWS-ERROR-NBR
GW             MOVE PEM-WORK-ZIP       TO BNPEWS-ERR-VAR1
GW             MOVE BNPEWS-START-DATE  TO BNPEWS-ERR-VAR2
GW             MOVE PLN-INS-CARRIER    TO BNPEWS-ERR-VAR3
GW             MOVE PLN-POST-CODE-TBL  TO BNPEWS-ERR-VAR4
GW             GO TO 5200-END
GW         ELSE
010900         MOVE BPC-POSTAL-CODE    TO BNPEWS-BPC-POSTAL-CODE.
ACS01 *    End CPS custom code
011900
012000 5200-END.
012100
      ******************************************************************
       5300-EDIT-CURR-CNTRY-CODE.
      ******************************************************************

           IF (PLN-CURRENCY-CODE       NOT = EMP-CURRENCY-CODE)
      ******** Employee not eligible; plan currency not = employee currency
               MOVE 109                TO BNPEWS-ERROR-NBR
               GO TO 5300-END.

           IF (PLN-COUNTRY-CODE        NOT = EMP-WORK-COUNTRY)
      ******** Employee not eligible; plan country not = employee country
               MOVE 110                TO BNPEWS-ERROR-NBR
               GO TO 5300-END.

       5300-END.

012200******************************************************************
012300 5000-END.
012400******************************************************************
012500
012600******************************************************************
012700 5000-EDIT-PLAN-START-DATE        SECTION.
012800******************************************************************
012900 5000-START.
013000
013100     SET NOT-ELIGIBLE            TO TRUE.
013200
013300     PERFORM 5100-EDIT-START-N-STOP-DATE
013400     THRU    5100-END.
013500
013600     IF (BNPE-ERROR-FOUND)
013700         GO TO 5000-END.
013800
013900     IF  (PLN-FLEX-PLAN           NOT = SPACES)
014000     AND (FLP-SPEND-ONLY          = "N")
           AND (CRT-PROGRAM-CODE        NOT = "BS12")
014100         PERFORM 5200-EDIT-FLEX-PLAN
014200         THRU    5200-END
014300
014400         IF (BNPE-ERROR-FOUND)
014500             GO TO 5000-END
014600         END-IF
014700
014800         IF (PLN-WAIVE-FLAG      = "Y")
014900             PERFORM 5300-EDIT-CORE-BENEFIT
015000             THRU    5300-END
015100         END-IF
015200
015300         IF (BNPE-ERROR-FOUND)
015400             GO TO 5000-END.
015500
015600     SET ELIGIBLE                TO TRUE.
015700
015800     GO TO 5000-END.
015900
016000******************************************************************
016100 5100-EDIT-START-N-STOP-DATE.
016200******************************************************************
016300
016400     IF (BNPEWS-START-DATE       < PLN-START-DATE)
016500******** Eligibility date < than plan start date
016600         MOVE 100                TO BNPEWS-ERROR-NBR
016700         MOVE BNPEWS-START-DATE  TO HRWS-DATE-FIELD
               INITIALIZE                 HRWS-DATE-8-FIELD
016800         PERFORM 781-HR-FORMAT-DATE-FIELD
016900         MOVE HRWS-VALUE         TO BNPEWS-ERR-VAR1
017100         MOVE PLN-START-DATE     TO HRWS-DATE-FIELD
017200         PERFORM 781-HR-FORMAT-DATE-FIELD
017300         MOVE HRWS-VALUE         TO BNPEWS-ERR-VAR2
017500         GO TO 5100-END.
017600
017700     IF  (PLN-STOP-DATE          NOT = ZEROES)
017800     AND (BNPEWS-START-DATE      > PLN-STOP-DATE)
017900******** Eligibility date > than plan stop date
018000         MOVE BNPEWS-START-DATE  TO HRWS-DATE-FIELD
               INITIALIZE                 HRWS-DATE-8-FIELD
018100         PERFORM 781-HR-FORMAT-DATE-FIELD
018200         MOVE HRWS-VALUE         TO BNPEWS-ERR-VAR1
018400         MOVE PLN-STOP-DATE      TO HRWS-DATE-FIELD
018500         PERFORM 781-HR-FORMAT-DATE-FIELD
018600         MOVE HRWS-VALUE         TO BNPEWS-ERR-VAR2
018800         MOVE 101                TO BNPEWS-ERROR-NBR
018900         GO TO 5100-END.
019000
019100 5100-END.
019200
019300******************************************************************
019400 5200-EDIT-FLEX-PLAN.
019500******************************************************************
019600
019700     MOVE BNPEWS-COMPANY         TO DB-COMPANY.
019800     MOVE BNPEWS-EMPLOYEE        TO DB-EMPLOYEE.
019900     MOVE BNPEWS-START-DATE      TO DB-START-DATE.
020000     PERFORM 850-FIND-NLT-EFDSET2.
020100     IF  (EMPFLEXDOL-FOUND)
PERF       AND (EFD-COMPANY             = BNPEWS-COMPANY)
PERF       AND (EFD-EMPLOYEE            = BNPEWS-EMPLOYEE)
020200     AND ((BNPEWS-START-DATE     >= EFD-START-DATE)
020300     AND  (BNPEWS-START-DATE     <= EFD-STOP-DATE))
020400         CONTINUE
020500     ELSE
020600******** Employee does not have a flex dollar record for date
020700         MOVE 103                TO BNPEWS-ERROR-NBR
020800         MOVE BNPEWS-START-DATE  TO HRWS-DATE-FIELD
               INITIALIZE                 HRWS-DATE-8-FIELD
020900         PERFORM 781-HR-FORMAT-DATE-FIELD
021000         MOVE HRWS-VALUE         TO BNPEWS-ERR-VAR1
021100         GO TO 5200-END.
021200
021300 5200-END.
021400
021500******************************************************************
021600 5300-EDIT-CORE-BENEFIT.
021700******************************************************************
021800
021900     IF (BNPEWS-PLAN-TYPE        = "HL")
022000         IF (FLP-HEALTH-FLAG     = "Y")
022100************ Core benefit plan exists; cannot waive coverage
022200             MOVE 106            TO BNPEWS-ERROR-NBR
022300         ELSE
022400         IF  (FLP-HEALTH-FLAG    = "S")
022500         AND (PEM-HL-COV-PROOF   NOT = "Y")
022600************ Core benefit plan exist; cannot waive without spouse proof
022700             MOVE 107            TO BNPEWS-ERROR-NBR
022800         END-IF
022900         END-IF
023000     ELSE
023100     IF (BNPEWS-PLAN-TYPE        = "DN")
023200         IF (FLP-DENTAL-FLAG     = "Y")
023300************ Core benefit plan exists; cannot waive coverage
023400             MOVE 106            TO BNPEWS-ERROR-NBR
023500         ELSE
023600         IF  (FLP-HEALTH-FLAG    = "S")
023700         AND (PEM-DN-COV-PROOF   NOT = "Y")
023800************ Core benefit plan exist; cannot waive without spouse proof
023900             MOVE 107            TO BNPEWS-ERROR-NBR
024000         END-IF
024100         END-IF
024200     ELSE
024300     IF (BNPEWS-PLAN-TYPE        = "DI")
024400         IF (FLP-DISAB-FLAG      = "Y")
024500************ Core benefit plan exists; cannot waive coverage
024600             MOVE 106            TO BNPEWS-ERROR-NBR
024700         END-IF
024800     ELSE
024900     IF (BNPEWS-PLAN-TYPE        = "EL")
025000         IF (FLP-LIFE-FLAG       = "Y")
025100************ Core benefit plan exists; cannot waive coverage
025200             MOVE 106            TO BNPEWS-ERROR-NBR
025300         END-IF
025400     END-IF
025500     IF (BNPEWS-PLAN-TYPE        = "DL")
025600         IF (FLP-DEP-LIFE-FLAG   = "Y")
025700************ Core benefit plan exists; cannot waive coverage
025800             MOVE 106            TO BNPEWS-ERROR-NBR.
025900
026000 5300-END.
026100
026200******************************************************************
026300 5000-END.
026400******************************************************************
026500
026600*--- End BNPE70PD (FEB 22-96)
