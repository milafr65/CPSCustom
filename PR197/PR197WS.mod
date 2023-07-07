******* PR197WS 4.2.2.1.19.1.14 <1937927587>
MG0825*  Modified by DIXIE PATTON  DP0718                              *
MH1113*  MODIFIED BY MARTIN HUNTER MH1113                              *
MG0825******************************************************************
MG0825*  07/18/13  - INCREASED OCCURS DUE TO NEW PLANS ADDED           *
MH1113*  11/21/13  - INCREASED OCCURS FROM 900 TO 1500 DUE TO NEW PLANS*
SB0830*   08/30/14 - INCREASED OCCURS FROM 1500 TO 9000 DUE TO NEW PLANS
      ******************************************************************
      *                            PR197WS                             *
      ******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------ | ------ | ------------------------------------------  *
      *  557679 | J57679 | PCD-SEQ-NBR INCREASE 4 TO 8 DIGITS CHANGES  *
      *  ------   ------   ------------------------------------------  *
      *  615880 | J15880 | CREATE NEW WS-DST-PCD-SEQ-NBR THAT WILL     *
      *         |        | HOLD VALUE COMING FROM WR3-PCD-SEQ-NBR      *
      *  ------   ------   ------------------------------------------  *
      *  627195 | J27195 | CREATE NEW WS-DST-HOURS-AMOUNT AND WS-      *
      *         |        | DST-UNITS-AMOUNT TO HOLD VALUE FROM WR3     *
      *  ------   ------   ------------------------------------------  *
      *  611414 | J11414 | CHANGED FOR INDIA PAYROLL                   *
      *  ------   ------   ------------------------------------------  *
      *  668929 | J68929 | MODIFICATION FOR YTD-EXEMPT POPULATION      *
      *  ------   ------   ------- ------------------------------------*
      *  878009 | J78009 | FIXED FOR YTD CALCULATION IN C6EXEMPT       *
      *  ------   ------   ------- ------------------------------------*
      *  954027 | J54027 | ADDED ADDITIONAL VARIABLES FOR USE IN GM    *
      *         |        | REPORT WRITING AND GMTRANEFRT POSTING       *
      *  ------ | ------ | ------------------------------------------- *
      * 1307422 | 307422 | Changed level 02 variables under a lvl 02   *
      *         |        | mother variable                             *
      *  ------ | ------ | ------------------------------------------- *
      * 1482465 | 482465 | ADDED VARIABLE FOR PROPER CLOSING OF TMP    *
      *         |        | FILES.                                      *
      *  ------   ------   ------------------------------------------- *
      * 1371703 | J71703 | Increase PRDED-DED-TABLE to 999             *
      * -------   ------   ------------------------------------------- *
      * 1659520 | 659520 | NEW VARIABLE TO SAVE GHR PLAN DESCRIPTION   *
      * -------   ------   ------------------------------------------- *
      ******************************************************************
000200 01  PR197WS.
000300******************************************************************
000500     02  WS-GLI-OPTION         PIC X(1)  VALUE SPACES.
000600
000700     02  WS-SUB-TOT-AMT        PIC S9(13)V99          VALUE ZEROS.
000800     02  WS-SUB-TOT-HOURS      PIC S9(7)V99          VALUE ZEROS.
UOM        02  WS-SUB-TOT-PAY-UNITS  PIC S9(07)V99         VALUE ZEROES.
000900     02  WS-CHECK-FOUND-SW     PIC 9                 VALUE ZEROS.
001000         88 WS-CHECK-FOUND                           VALUE 1.
001100     02  WS-DST-ACCT-UNIT      PIC X(15)             VALUE SPACES.
001200     02  WS-DST-ACCOUNT        PIC 9(06)             VALUE ZEROS.
001300     02  WS-DST-SUB-ACCT       PIC 9(04)             VALUE ZEROS.
J15880     02  WS-DST-PCD-SEQ-NBR    PIC 9(08)             VALUE ZEROS.
J27195     02  WS-DST-HOURS-AMOUNT   PIC S9(05)V99         VALUE ZEROS.
J27195     02  WS-DST-UNITS-AMOUNT   PIC S9(05)V99         VALUE ZEROES.
001400     02  WS-ACCT-UNIT          PIC X(15)             VALUE SPACES.
001500     02  WS-ACCOUNT            PIC 9(06)             VALUE ZEROS.
001600     02  WS-SUB-ACCOUNT        PIC 9(04)             VALUE ZEROS.
001700     02  WS-POSTING-DATE       PIC 9(8)              VALUE ZEROS.
           02  WS-GL-DATE            PIC 9(8)              VALUE ZEROS.
001800     02  WS-TO-COMPANY         PIC 9(4)              VALUE ZEROS.
001900     02  WS-COMPANY            PIC 9(4)              VALUE ZEROS.
J61964*    02  WS-RECORD-TYPE        PIC X(1)              VALUE SPACES.
           02  WS-DEPARTMENT         PIC X(5)              VALUE SPACES.
           02  WS-HM-PROC-LEVEL      PIC X(05)      VALUE SPACES.
002100
002200     02  WS-CO-NAME              PIC X(30)      VALUE SPACES.
002300     02  WS-GL-UNITS             PIC X(01)      VALUE SPACES.
002400     02  WS-PYM-PROCESS-LEVEL    PIC X(05)      VALUE SPACES.
002500     02  WS-PYM-EMPLOYEE         PIC 9(09)      VALUE ZEROS.
002600
002700     02  WS-SYSTEM-CHECKS-FLAG   PIC 9 VALUE 0.
002800         88  WS-SYSTEM-CHECKS-NOTFOUND VALUE 0.
002900         88  WS-SYSTEM-CHECKS-FOUND    VALUE 1.
003000     02  WS-CHECKS-FLAG          PIC 9 VALUE 0.
003100         88  WS-CHECKS-NOTFOUND        VALUE 0.
003200         88  WS-CHECKS-FOUND           VALUE 1.
003000     02  UPDATE-OPTION-SW        PIC 9 VALUE 1.
003100         88  REPORT-ONLY               VALUE 0.
003200         88  UPDATING                  VALUE 1.
003300
P62055     02  WS-CYCLE-SW             PIC 9 VALUE 0.
P62055         88  FULL-CYCLE                VALUE 0.
P62055         88  MID-CYCLE                 VALUE 1.
         
P79800     02  WS-ACCOUNT-ERROR-SW     PIC 9 VALUE 0.
P79800         88  ACCOUNT-ERROR-NOTFOUND    VALUE 0.
P79800         88  ACCOUNT-ERROR-FOUND       VALUE 1.
         
J85817     02  WS-PR-ADVICE-SERVICE-SW PIC 9 VALUE 0.
J85817         88  PR-ADVICE-NOT-ENABLED     VALUE 0.
J85817         88  PR-ADVICE-ENABLED         VALUE 1.

           02  WS-DEPT-HOURS           PIC S9(07)V99        VALUE ZEROS.
           02  WS-PROC-LEV-HOURS       PIC S9(07)V99        VALUE ZEROS.
           02  WS-DEPT-AMOUNT          PIC S9(13)V99        VALUE ZEROS.
           02  WS-PROC-LEV-AMOUNT      PIC S9(13)V99        VALUE ZEROS.
003500     02  WS-TOTAL-FIELDS.
003600         03  WS-TOT-DEBIT-AMT     PIC S9(13)V99       VALUE ZEROS.
003700         03  WS-TOT-CREDIT-AMT    PIC S9(13)V99       VALUE ZEROS.
003800         03  WS-TOT-HOURS         PIC S9(7)V99        VALUE ZEROS.
UOM            03  WS-TOT-UNITS         PIC S9(7)V99        VALUE ZEROS.
004000     02  WS-GLT-WORK-FIELDS.
004100         03  WS-AMOUNT            PIC S9(13)V99       VALUE ZEROS.
004200         03  WS-UNITS             PIC S9(9)V99        VALUE ZEROS.
004300         03  WS-DIST-AMT-TOT      PIC S9(13)V99       VALUE ZEROS.
004400         03  WS-DIST-HOURS-TOT    PIC S9(7)V99        VALUE ZEROS.
UOM        02  WS-UNIT-OF-MEASURE-FIELDS.
               03  WS-PAY-HOURS         PIC S9(9)V99   VALUE ZEROES. 
               03  WS-PAY-UNITS         PIC S9(9)V99   VALUE ZEROES. 
               03  WS-DIST-UNITS-TOT     PIC S9(9)V99 VALUE ZEROES.
               03  WS-UNIT-MEASURE      PIC X(12)      VALUE SPACES.
004500
004600     02  WS-RESTARTING-PHASE-2-SW     PIC 9(01) VALUE 0.
004700         88 RESTARTING-PHASE-2                  VALUE 1.
005000
           02  WS-RESTARTING-PHASE-3-SW     PIC 9(01) VALUE 0.
               88 RESTARTING-PHASE-3                  VALUE 1.
      
           02  WS-RESTARTING-PHASE-4-SW     PIC 9(01) VALUE 0.
               88 RESTARTING-PHASE-4                  VALUE 1.

           02  WS-RESTARTING-PHASE-5-SW     PIC 9(01) VALUE 0.
               88 RESTARTING-PHASE-5                  VALUE 1.
      
*******-- CKP-Restart-Info is 691 bytes
           02  WS-RESTART-INFO.
               03  WS-RS-PHASE-COUNTER  PIC 9(01)  VALUE ZEROES.
               03  WS-RS-IFGT-DONE-SW   PIC 9(01)  VALUE ZEROES.
                   88  WS-RS-IFGT-DONE             VALUE 1.
               03  WS-RS-RUN-DATE       PIC 9(08)  VALUE ZEROES.
               03  WS-RS-RUN-TIME       PIC 9(06)  VALUE ZEROES.
               03  WS-RS-CKP-ID         PIC 9(12)  VALUE ZEROES.
               03  WS-RS-REC-CNT        PIC 9(08)  VALUE ZEROES.
GMDIST         03  WS-RS-GM-REC-CNT     PIC 9(08)  VALUE ZEROES.
               03  WS-RS-AX-REC-COUNT   PIC 9(12)  VALUE ZEROES.
               03  WS-RS-BENWORK-NAME   PIC X(100) VALUE SPACES.
               03  WS-RS-WORK3-NAME     PIC X(100) VALUE SPACES.
GMDIST         03  WS-RS-GMWORK-NAME    PIC X(100) VALUE SPACES.
P79800         03  WS-RS-GLERR-WORK-NAME PIC X(100) VALUE SPACES.
GM             03  WS-RS-LAST-GEF-OBJ-ID  LIKE OBJ-ID   VALUE ZEROS.
GM             03  WS-RS-TOT-GEF-COUNT  PIC 9(12)     VALUE ZEROS.
006600     02  WS-CL-TABLE.
006700         03  WS-CL-CNT           PIC 9(03)     VALUE ZEROS.
006800         03  WS-CL-PROC-LEV      PIC X(05)     OCCURS 300 TIMES.
006900         03  WS-CL-TYPE          PIC X(01)     OCCURS 300 TIMES.
007000         03  WS-CL-DATE          PIC 9(08)     OCCURS 300 TIMES.
007100         03  WS-CL-AMT           PIC S9(13)V99 OCCURS 300 TIMES.
007200
007300 01  BENEFITS-WS.
007400     02  WS-PLAN-TYPE            PIC X(04) VALUE SPACES.
007500     02  WS-HIRE-DATE            PIC 9(08) VALUE ZEROS.
007600     02  WS-HIRE-DATE-RD         REDEFINES WS-HIRE-DATE.
007700         03  WS-HIRE-YEAR        PIC 9(04).
007800         03  WS-HIRE-MONTH       PIC 9(02).
007900         03  WS-HIRE-DAY         PIC 9(02).
008000     02  WS-VEST-BEG-DATE        PIC 9(08) VALUE ZEROS.
008100     02  WS-VEST-BEG-DATE-RD     REDEFINES WS-VEST-BEG-DATE.
008200         03  WS-VEST-BEG-YEAR    PIC 9(04).
008300         03  WS-VEST-BEG-MONTH   PIC 9(02).
008400         03  WS-VEST-BEG-DAY     PIC 9(02).
P12742     02  WS-VES-END-DATE         PIC 9(08) VALUE ZEROS.
P12742     02  WS-VES-END-DATE-RD      REDEFINES WS-VES-END-DATE.
P12742         03  WS-VES-END-YEAR     PIC 9(04).
P12742         03  WS-VES-END-MONTH    PIC 9(02).
P12742         03  WS-VES-END-DAY      PIC 9(02).
008500     02  WS-PER-END-DATE         PIC 9(08) VALUE ZEROS.
008600     02  WS-PER-END-DATE-RD      REDEFINES WS-PER-END-DATE.
008700         03  WS-PER-END-YEAR     PIC 9(04).
008800         03  WS-PER-END-MONTH    PIC 9(02).
008900         03  WS-PER-END-DAY      PIC 9(02).
009000     02  WS-PLAN-BEG-DATE        PIC 9(08) VALUE ZEROS.
009100     02  WS-PLAN-BEG-DATE-RD     REDEFINES WS-PLAN-BEG-DATE.
009200         03  WS-PLAN-BEG-YEAR    PIC 9(04).
009300         03  WS-PLAN-BEG-MONTH   PIC 9(02).
009400         03  WS-PLAN-BEG-DAY     PIC 9(02).
009500     02  WS-HOURS-SERV           PIC S9(07)V99        VALUE ZEROS.
009600     02  WS-VEST-HOURS           PIC S9(07)V99        VALUE ZEROS.
009700     02  WS-COMP-AMOUNT          PIC S9(13)V99        VALUE ZEROS.
009800     02  WS-WAGE-AMOUNT          LIKE WAGE-AMOUNT     VALUE ZEROS.
009900     02  WS-PRE-DED-AMT          LIKE DED-AMT         VALUE ZEROS.
010000     02  WS-AFT-DED-AMT          LIKE DED-AMT         VALUE ZEROS.
010100     02  WS-CMP-DED-AMT          LIKE DED-AMT         VALUE ZEROS.
010200     02  WS-DED-AMT              LIKE DED-AMT         VALUE ZEROS.
010300     02  WS-TOT-VEST-HOURS       PIC S9(07)V99        VALUE ZEROS.
010400     02  WS-TOT-HOURS-SERV       PIC S9(07)V99        VALUE ZEROS.
010500     02  WS-TOT-COMP-AMOUNT      PIC S9(13)V99        VALUE ZEROS.
010600     02  WS-TOT-PRE-DED-AMT      LIKE DED-AMT         VALUE ZEROS.
010700     02  WS-TOT-AFT-DED-AMT      LIKE DED-AMT         VALUE ZEROS.
010800     02  WS-TOT-CMP-DED-AMT      LIKE DED-AMT         VALUE ZEROS.
010900     02  WS-COD-PRE-TAX-YTD      PIC S9(13)V99        VALUE ZEROS.
011000     02  WS-COD-AFT-TAX-YTD      PIC S9(13)V99        VALUE ZEROS.
011100     02  WS-COD-CMP-CONT-YTD     PIC S9(13)V99        VALUE ZEROS.
P86464     02  WS-SV-WAGE-AMOUNT       LIKE WAGE-AMOUNT     VALUE ZEROS.
P86464     02  WS-SV-EFFORT-AMT        PIC S9(16)V99        VALUE ZEROS.
P86464     02  WS-SV-EFFORT-PCT        PIC S9(7)V9(6)       VALUE ZEROS.
011200
011300     02  WS-BENEFIT-EXISTS-SW    PIC 9(01) VALUE 0.
011400         88  BENEFIT-EXISTS      VALUE 1.
011500         88  NO-BENEFIT-EXISTS   VALUE 0.
011600     02  WS-PLAN-CODE            PIC X(04) VALUE SPACES.
J72349     02  WS-401K-PLAN-FLAG       PIC X(01) VALUE SPACES.
J72349     02  WS-EE-401K-PLAN-FOUND   PIC X(01) VALUE SPACES.
J72349     02  WS-EE-409A-AMOUNT       LIKE DED-AMT         VALUE ZEROS.
011700 01  PR197WS-MORE.
011800     02  WS-PLAN-FOUND-SW        PIC 9(01) VALUE 0.
011900         88  WS-PLAN-FOUND                 VALUE 1.
012000     02  WS-TBL-LIMIT-SW         PIC 9(01) VALUE 0.
012100         88  WS-TBL-OVER-LIMIT   VALUE 1.
012200     02  WS-PLN-TBL-SIZE         PIC 9(03) VALUE ZEROS.
012300     02  WS-PLN-DB-DC-SIZE       PIC 9(03) VALUE ZEROS.

SDB********02  WS-TBL-MAX              PIC 9(03) VALUE 200.
SB0830     02  WS-TBL-MAX              PIC 9(04) VALUE 9000. 

           02  WS-PLN-TBL-1.
SDB*********** 03  WS-PLN-TBL-1-GRP        OCCURS 200 TIMES.
SB0830         03  WS-PLN-TBL-1-GRP        OCCURS 9000 TIMES.               
                   04  WS-PLN-PLAN-TYPE    PIC X(02).
                   04  WS-PLN-PLAN-CODE    PIC X(04).
                   04  WS-PLN-START-DATE   PIC 9(08).
                   04  WS-PLN-STOP-DATE    PIC 9(08).
                   04  WS-PLN-FROM-DATE    PIC X(02).
                   04  WS-PLN-VEST-DATE    PIC X(02).
                   04  WS-PLN-VEST-BEF-FLG PIC X(01).
                   04  WS-PLN-COMP-CLASS   PIC X(03).
                   04  WS-PLN-VEST-CLASS   PIC X(03).
                   04  WS-PLN-DED-CODEG       OCCURS 15 TIMES.
                       05  WS-PLN-DED-CODE    LIKE DED-CODE.
P81387             04  WS-PLN-CODA-CONTRIB PIC X(01).
659520             04  WS-GHR-PLAN-DESC    PIC X(60).

           02  WS-PLN-TBL-2.
               03  WS-BEN-EDM-SEQ-NBR      LIKE SEQ-NBR.
SDB**********  03  WS-PLN-TBL-2-GRP        OCCURS 200 TIMES.
SB0830         03  WS-PLN-TBL-2-GRP        OCCURS 9000 TIMES.
               
                   04  WS-BASE-HOURS       PIC S9(07)V99.
                   04  WS-BASE-AMOUNT      PIC S9(13)V99.
                   04  WS-BASE-VEST-HOURS  PIC S9(07)V99.
                   04  WS-BENEFIT-DONE     PIC X(01).

014200     02  WS-BENWORK-TABLE.
SDB*********   03  WS-BENWORK-OCC          OCCURS 200 TIMES.
SB0830         03  WS-BENWORK-OCC          OCCURS 9000 TIMES.
014400             04  WR-HOURS-SERV       PIC S9(07)V99.
014500             04  WR-VEST-HOURS       PIC S9(07)V99.
014600             04  WR-COMP-AMOUNT      PIC S9(13)V99.
014700             04  WR-CMP-DED-AMT      PIC S9(07)V99.
J72349             04  WR-PLAN-YEAR        PIC  9(04).
014800     02  WS-BENWORK-TABLE-2.
SDB**********  03  WS-BENWORK-OCC-2        OCCURS 200 TIMES.
SB0830         03  WS-BENWORK-OCC-2        OCCURS 9000 TIMES.
015000             04  WR-PRE-DED-AMT      PIC S9(07)V99.
015100             04  WR-AFT-DED-AMT      PIC S9(07)V99.
015200             04  WR-DED-AMT          PIC S9(07)V99.
J72349             04  WR-401K-IN-PLAN     PIC X(01).     
J72349             04  WR-409A-DED-AMT     PIC S9(09)V99.
015300
J72349     02  WS-409A-AMT-TABLE. 
J72349         03  WS-409AWORK-OCC         OCCURS 50  TIMES. 
J72349             04  WS-409A-PROC-LEVEL  PIC X(05).    
J72349             04  WS-409A-PLAN-TYPE   PIC X(02).     
J72349             04  WS-409A-AMOUNT      PIC S9(09)V99.
J72349
J72349     02  WS-409A-TABLE-SIZE          PIC 9(02) VALUE 50.
J72349
016100     02  BENWORK-NAME            PIC X(74) VALUE SPACES.
016200     02  BENWORK-SW              PIC 9(1)  VALUE 1.
016300         88  BENWORK-NOTFOUND              VALUE 1.
016400         88  BENWORK-FOUND                 VALUE 0.
016500     02  BENWORK-CREATED-SW      PIC 9(1)  VALUE 0.
016600         88  BENWORK-CREATED               VALUE 1.
016700     02  WS-GT70SEQ-FILE         PIC X(74) VALUE SPACES.
017200     02  WS-EMPLOYEE-N           PIC 9(09) VALUE ZEROS.
017300     02  WS-EMPLOYEE-A           REDEFINES WS-EMPLOYEE-N
017400                                 PIC X(09).
017500     02  WS-RECORD-COUNT         PIC 9(08) VALUE ZEROS.
gm         02  WS-GEF-COUNT            PIC 9(08) VALUE ZEROS.
gm         02  WS-TOT-GEF-COUNT        PIC 9(08) VALUE ZEROS.

J57679     02  WS-WR3-SEQ-NBR            PIC 9(08) VALUE ZEROES.
           02  WS-WORK3-FILE-NAME        PIC X(100) VALUE SPACES.
           02  WS-ERRORS-FILE-NAME       PIC X(100) VALUE SPACES.
GMDIST     02  WS-GMWORK-FILE-NAME       PIC X(100) VALUE SPACES.
P79800     02  WS-GLERR-WORK-FILE-NAME   PIC X(100) VALUE SPACES.

017700     02  WS-SUMMARY-CONTROL.
017800         03  WS-WR3-PRD-OPTION   PIC X(01) VALUE SPACES.
017900         03  WS-WR3-CLEARING     PIC X(01) VALUE SPACES.
018000         03  WS-WR3-PROCESS-LEVEL PIC X(05) VALUE SPACES.
018100         03  WS-WR3-RECORD-TYPE  PIC X(01) VALUE SPACES.
018200
018300     02  WS-PROGRAM-INFO.
018400         03  WS-WR3-EMPLOYEE     PIC 9(09) VALUE ZEROS.
018500         03  WS-WR3-CHECK-ID     PIC 9(12) VALUE ZEROS.
018600         03  WS-WR3-DEPARTMENT   PIC X(05) VALUE SPACES.
018700         03  WS-WR3-CHECK-TYPE   PIC X(01) VALUE SPACES.
J57679         03  WS-WR3-PCD-SEQ-NBR  PIC 9(08) VALUE ZEROS.
018900         03  WS-WR3-DED-CODE     PIC X(04) VALUE SPACES.
019000         03  WS-WR3-JOB-CODE     PIC X(09) VALUE SPACES.
019100         03  WS-WR3-OBJ-ID       PIC 9(12) VALUE ZEROS.
019200         03  WS-WR3-POSITION     PIC X(12) VALUE SPACES.
019300         03  WS-WR3-ATTEND-CODE  PIC X(02) VALUE SPACES.
P79800         03  WS-WR3-PAY-CODE     PIC X(04) VALUE SPACES.
P79800         03  WS-WR3-FULL-NAME    PIC X(30) VALUE SPACES.
P79800         03  WS-WR3-TRAN-AMOUNT  PIC S9(13)V99.
019400
019500     02  WS-PA-FIELD-NAME          PIC X(20) OCCURS 3 TIMES.
019600     02  WS-PC-FIELD-NAME          PIC X(20) OCCURS 3 TIMES.
019700     02  WS-PD-FIELD-NAME          PIC X(20) OCCURS 3 TIMES.
019800     02  WS-PW-FIELD-NAME          PIC X(20) OCCURS 3 TIMES.
019900     02  WS-C-MESSAGE              PIC X(60) VALUE SPACES.
020000     02  WS-A-MESSAGE              PIC X(60) VALUE SPACES.
020100     02  WS-E-MESSAGE              PIC X(60) VALUE SPACES.
           02  WS-MESSAGE-150            PIC X(60) VALUE SPACES.
020200
020300     02  WS-EMPLOYEE-LIT           PIC X(11) VALUE SPACES.
020400     02  WS-DED-CODE-LIT           PIC X(11) VALUE SPACES.
020500     02  WS-JOB-CODE-LIT           PIC X(11) VALUE SPACES.
020600     02  WS-JOB-CLASS-LIT          PIC X(11) VALUE SPACES.
020700     02  WS-ATTEND-CODE-LIT        PIC X(11) VALUE SPACES.
020800     02  WS-PAY-CODE-LIT           PIC X(11) VALUE SPACES.
           02  WS-POSITION-LIT           PIC X(11) VALUE SPACES.
020900
022500     02  WS-ERR-VARS.
022600         03  WS-ERR-VAR1           PIC X(20) VALUE SPACES.
022700         03  WS-ERR-VAR2           PIC X(20) VALUE SPACES.
022800         03  WS-ERR-VAR3           PIC X(20) VALUE SPACES.
022900         03  WS-ERR-VAR4           PIC X(20) VALUE SPACES.
023000         03  WS-ERR-VAR5           PIC X(20) VALUE SPACES.
023100     02  WS-ERROR-CAT              PIC X(05) VALUE SPACES.
023300     02  WS-VENDOR                 PIC X(10) VALUE SPACES.
023400     02  WS-EMPLOYEE               PIC 9(09) VALUE ZEROES.
J61964*    02  WS-GARN-OBJ-ID            PIC 9(12) VALUE ZEROES.
023600     02  WS-INVOICE                PIC X(22) VALUE SPACES.
025200     02  WS-INVOICE-AMT            PIC S9(13)V99 VALUE ZEROES.
025300
026000     02  WS-SEQ-NBR                PIC 9(04) VALUE ZEROES.
026100     02  WS-AX-REC-COUNT           PIC 9(09) VALUE ZEROES.
J61964*    02  WS-AP-COMMENT-2.
J61964*        03  WS-SSN                PIC X(11) VALUE SPACES.
J61964*        03  FILLER                PIC X(02) VALUE SPACES.
J61964*        03  WS-CASE-NUMBER        PIC X(20) VALUE SPACES.
           02  WS-ONE                    PIC 9(01) VALUE 1.
016800*    02  WS-CHECK-TYPE             PIC X(01) VALUE SPACES.
001900     02  WS-DIST-COMPANY           PIC 9(04) VALUE ZEROES.

P96085     02  WS-CO-EXCH-RATE           PIC S9(6)V9(6) VALUE ZEROES.
P96085     02  WS-DST-CO-EXCH-RATE       PIC S9(6)V9(6) VALUE ZEROES.
P96085     02  WS-CO-CURRENCY-CD         PIC X(05)      VALUE SPACES.

           02  WS-QUALIFIER              PIC 9(04) VALUE ZEROES.

           02  PRGLCLRNG-NAME            PIC X(100) VALUE SPACES.
           02  PRGLCLRNG-SW              PIC 9(1)  VALUE 1.
               88  PRGLCLRNG-NOTFOUND              VALUE 1.
               88  PRGLCLRNG-FOUND                 VALUE 0.
           02  PRGLCLRNG-CREATED-SW      PIC 9(1)  VALUE 0.
               88  PRGLCLRNG-CREATED               VALUE 1.

J61964*    02  WS-SV-COMPANY          PIC 9(4)       VALUE ZEROS.
J61964*    02  WS-SV-PROCESS-LEVEL    PIC X(05)      VALUE SPACES.
GMDIST     02  WS-SV-DEPARTMENT       PIC X(5)       VALUE SPACES.
GMDIST     02  WS-SV-EMPLOYEE         PIC 9(09)      VALUE ZEROS.
GMDIST     02  WS-SV-TR-DATE          PIC 9(08)      VALUE ZEROS.
GMDIST     02  WS-SV-JOB-CODE         PIC X(09)      VALUE SPACES.
GMDIST     02  WS-SV-PAY-CODE         PIC X(04)      VALUE SPACES.
GMDIST     02  WS-SV-POSITION         PIC X(12)      VALUE SPACES.
GMDIST     02  WS-SV-DED-CODE         PIC X(04)      VALUE SPACES.
GMDIST     02  WS-SV-LAB-DIST-FLAG    PIC X(01)      VALUE SPACES.
GMDIST     02  WS-GEF-OBJ-REMAIN      PIC 9(12)      VALUE ZEROS.
GMDIST     02  WS-LAST-GEF-OBJ-ID     PIC 9(12)      VALUE ZEROS.
GMDIST     02  WS-TOT-DED-DIST-AMT    PIC S9(13)V99  VALUE ZEROS.
GMDIST     02  WS-TOT-PAY-DIST-AMT    PIC S9(13)V99  VALUE ZEROS.
J54027     02  WS-TOT-ACT-DIST-AMT    PIC S9(13)V99  VALUE ZEROS.
J54027     02  WS-TOT-TRDED-DIST-AMT  PIC S9(13)V99  VALUE ZEROS.
J54027
J54027     02  WS-GM3-VARIABLES.
J54027         03  WS-GM3-DEDUCTION-TYPE  PIC 9(01)      VALUE ZEROS. 
J54027         03  WS-GM3-EFFORT-FLAG     PIC X(01)      VALUE SPACES.
J54027         03  WS-GM3-LAB-DIST-FLAG   PIC X(01)      VALUE SPACES.
J54027         03  WS-GM3-COMPANY         PIC 9(04)      VALUE ZEROS.
J54027         03  WS-GM3-EMPLOYEE        PIC 9(09)      VALUE ZEROS.
J54027         03  WS-GM3-FULL-NAME       PIC X(30)      VALUE ZEROS.
J54027         03  WS-GM3-PROCESS-LEVEL   PIC X(05)      VALUE ZEROS.
J54027         03  WS-GM3-DEPARTMENT      PIC X(05)      VALUE ZEROS.
J54027         03  WS-GM3-JOB-CODE        PIC X(09)      VALUE ZEROS.
J54027         03  WS-GM3-POSITION        PIC X(12)      VALUE ZEROS.
J54027         03  WS-GM3-PAY-CODE        PIC X(04)      VALUE ZEROS.
J54027         03  WS-GM3-RECORD-TYPE     PIC X(01)      VALUE ZEROS.
J54027         03  WS-GM3-EFFECT-DATE     PIC 9(08)      VALUE ZEROS.
J54027         03  WS-GM3-TO-COMPANY      PIC 9(04)      VALUE ZEROS.
J54027         03  WS-GM3-ACCT-UNIT       PIC X(15)      VALUE SPACES. 
J54027         03  WS-GM3-ACCOUNT         PIC 9(06)      VALUE ZEROS.  
J54027         03  WS-GM3-SUB-ACCOUNT     PIC 9(04)      VALUE ZEROS.
J54027         03  WS-GM3-ACTIVITY        PIC X(15)      VALUE SPACES.
J54027         03  WS-GM3-ACCT-CATEGORY   PIC X(05)      VALUE SPACES.
J54027         03  WS-GM3-PER-END-DATE    PIC 9(08)      VALUE ZEROS. 
J54027         03  WS-GM3-TR-DATE         PIC 9(08)      VALUE ZEROS. 
J54027         03  WS-GM3-SALARY-PCT      PIC S9(7)V9(6) COMP-3       
J54027                                                   VALUE ZEROS.
J54027         03  WS-GM3-EFFORT-PCT      PIC S9(7)V9(6) COMP-3       
J54027                                                   VALUE ZEROS.
J54027         03  WS-GM3-HOURS-AMOUNT    PIC S9(7)V9(6) COMP-3       
J54027                                                   VALUE ZEROS.
J54027         03  WS-GM3-DIST-AMT        PIC S9(11)V9(2) COMP-3       
J54027                                                   VALUE ZEROS.
J54027         03  WS-GM3-WAGE-AMOUNT     PIC S9(11)V9(2) COMP-3        
J54027                                                   VALUE ZEROS.
307422         03  WS-GM3-GM-JOB-CODE     PIC X(09)      VALUE ZEROS.
307422         03  WS-GM3-GM-POSITION     PIC X(12)      VALUE SPACES.
307422         03  WS-GM3-GM-PAY-CODE     PIC X(04)      VALUE SPACES.
307422         03  WS-GM3-TR-JOB-CODE     PIC X(9)       VALUE SPACES.
307422         03  WS-GM3-TR-POSITION     PIC X(12)      VALUE SPACES.
307422         03  WS-GM3-TR-PAY-CODE     PIC X(4)       VALUE SPACES.
307422         03  WS-GM3-TR-PCD-SEQ-NBR  PIC 9(4)       VALUE ZEROS.
307422         03  WS-GM3-CHECK-ID        PIC 9(12)      VALUE ZEROS.
307422         03  WS-GM3-CURRENCY-CODE   PIC X(5)       VALUE SPACES.
307422         03  WS-GM3-DED-CODE        PIC X(4)       VALUE SPACES.
307422         03  WS-GM3-ERROR-NBR       PIC 9(3)       VALUE ZEROS.
307422         03  WS-GM3-ERR-MESSAGE     PIC X(60)      VALUE SPACES.
307422         03  WS-GM3-GML-OBJ-ID      PIC 9(12)      VALUE ZEROS.
307422         03  WS-GM3-OT-PLAN-CODE    PIC X(4)       VALUE SPACES.
307422         03  WS-GM3-GM-LINE-NBR     PIC 9(12)      VALUE ZEROS.
307422         03  WS-GM3-UPDATE-EFFORT   PIC X(1)       VALUE SPACES.
307422         03  WS-GM3-EFFORT-AMT      PIC S9(14)V9(2) COMP-3
J54027                                                   VALUE ZEROS.

J71703     02  WS-EMP-OTD-TABLE-SIZE           PIC 9(04) VALUE 999.

           02  WS-EMP-OTD-TABLE.
J71703         03  WS-EMP-OTD-TABLE-GRP        OCCURS 999 TIMES.
                   04  WS-OTD-DED-CODE         LIKE DED-CODE.
                   04  WS-OTD-EDM-SEQ-NBR      LIKE EDM-SEQ-NBR.
                   04  WS-OTD-SEQ-NBR          LIKE SEQ-NBR.
                   04  WS-OTD-PROCESS-LEVEL    LIKE PROCESS-LEVEL.
                   04  WS-OTD-DED-AMT          LIKE DED-AMT.
                   04  WS-OTD-RECORD-TYPE      LIKE RECORD-TYPE.
                   04  WS-OTD-EFFECT-DATE      LIKE DATE.
                   04  WS-OTD-BEN-PLAN-TYPE    LIKE PLAN-TYPE.
                   04  WS-OTD-BEN-PLAN-CODE    LIKE PLAN-CODE.
                   04  WS-OTD-BEN-START-DATE   LIKE DATE.

           02  WS-MAX-OBJ-PER-PYM-SIZE         PIC 9(04) VALUE 200.
           02  WS-NBR-OF-OBJ-REQUESTED         PIC 9(04) VALUE 1000.
P62055     02  WS-PR197-FILTER-LENGTH          PIC 9(04) VALUE 1.
           02  PR197WS-SV-PAY-CODE             PIC X(04) VALUE SPACES.
P84284     02  WS-GEF-EFFORT-AMT               PIC S9(16)V9(2) COMP-3.
J11414*** CHANGES FOR INDIA PAYROLL START HERE                    ***
J11414     02  WS-IPR-PAYROLL-YEAR             PIC 9(04) VALUE ZEROES.
J11414     02  WS-C6-FILES-DATES               PIC 9(1)  VALUE ZEROES.
J11414         88  WS-C6-FILES-DATES-INVALID             VALUE 0.
J11414         88  WS-C6-FILES-DATES-VALID               VALUE 1.
J11414     02  WS-C6D-VALID-RECORD             PIC 9(1)  VALUE ZEROES.
J11414         88  WS-C6D-VALID-RECORD-NOT-FOUND         VALUE 0.
J11414         88  WS-C6D-VALID-RECORD-FOUND             VALUE 1.
J11414     02  WS-CEI-PROC-STATUS            PIC X(1)  VALUE SPACES.
J11414         88  WS-CEI-PROC-STATUS-PENDING          VALUE "P".
J11414         88  WS-CEI-PROC-STATUS-CLOSED           VALUE "C".
J11414     02  WS-C6H-PROC-STATUS              PIC X(1)  VALUE SPACES.
J11414         88  WS-C6H-PROC-STATUS-PENDING            VALUE "P".
J11414         88  WS-C6H-PROC-STATUS-CLOSED             VALUE "C".
J11414     02  WS-C6E-PROC-STATUS              PIC X(1)  VALUE SPACES.
J11414         88  WS-C6E-PROC-STATUS-PENDING            VALUE "P".
J11414         88  WS-C6E-PROC-STATUS-CLOSED             VALUE "C".
J11414     02  WS-C6X-STATUS                   PIC 9(1)  VALUE ZEROES.
J11414         88  WS-C6X-STATUS-PENDING                 VALUE 0.
J11414         88  WS-C6X-STATUS-CLOSED                  VALUE 9.
J78009*J68929 02  WS-C6X-EXEMPTED-AMT             PIC 9(13)V99 
J78009     02  WS-C6X-EXEMPTED-AMT             PIC S9(13)V99 
J68929                                                   VALUE ZEROES.
J11414     02  WS-PLAN-CODE-DEF                PIC X(4)  VALUE SPACES.
J11414     02  WS-C6PRDDTL.
J11414         05  WS-C6D-COMPANY             PIC 9(4).
J11414         05  WS-C6D-PAYROLL-YEAR        PIC 9(4).
J11414         05  WS-C6D-PAY-PRD-DEF         PIC X(4).
J11414         05  WS-C6D-PAY-FREQUENCY       PIC 9(1).
J11414         05  WS-C6D-START-DATE          PIC 9(8).
J11414         05  WS-C6D-END-DATE            PIC 9(8).
J11414         05  WS-C6D-NBR-OF-DAYS         PIC 9(2).
J11414         05  WS-C6D-NBR-OF-HOURS        PIC 9(5).
J11414         05  WS-C6D-PERIOD-STATUS       PIC X(1).
J11414         05  WS-C6D-REMAIN-CYCLES       PIC 9(2).
J11414     02  WS-INEMP-REASON-CODE-UPDATE-SW PIC 9(1).
J11414         88  WS-INEMP-REASON-CODE-UPDATE-OFF VALUE ZEROS.
J11414         88  WS-INEMP-REASON-CODE-UPDATE-ON  VALUE 1.
J11414     02  WS-IPR-START-DATE.
J11414         05  WS-IPR-START-DATE-YEAR     PIC 9(4).
J11414         05  WS-IPR-START-DATE-MONTH    PIC 9(2).
J11414         05  WS-IPR-START-DATE-DAY      PIC 9(2).
J11414*** CHANGES FOR INDIA PAYROLL END  HERE                     ***
402689     02  PR197WS-GLERR-FILE-SW   PIC 9(01) VALUE 0.
402689         88  PR197WS-GLERR-OPEN            VALUE 1.
402689         88  PR197WS-GLERR-CLOSE            VALUE 0.
482465     02  PR197WS-ERRORS-FILE-SW  PIC 9(01) VALUE 0.
482465         88  PR197WS-ERRORS-OPEN           VALUE 1.
482465         88  PR197WS-ERRORS-CLOSE          VALUE 0.
482465     02  PR197WS-GMWORK-FILE-SW  PIC 9(01) VALUE 0.
482465         88  PR197WS-GMWORK-OPEN           VALUE 1.
482465         88  PR197WS-GMWORK-CLOSE          VALUE 0.
482465     02  PR197WS-WORK3-FILE-SW   PIC 9(01) VALUE 0.
482465         88  PR197WS-WORK3-OPEN            VALUE 1.
482465         88  PR197WS-WORK3-CLOSE           VALUE 0.
482465     02  PR197WS-BENWORK-FILE-SW PIC 9(01) VALUE 0.
482465         88  PR197WS-BENWORK-OPEN          VALUE 1.
482465         88  PR197WS-BENWORK-CLOSE         VALUE 0.
