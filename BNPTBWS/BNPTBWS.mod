******* BNPTBWS 2.1.9 <2958320607>
     ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  Modified by Analysts International,MN                         *
AI0095******************************************************************
AI0095*  AI0095  03/24/03  RETROACTIVE BENEFIT CHANGE MODIFICATION     *
      *  SEE ALSO TAGS RAY AND GW                                      *
000100******************************************************************
000200*                             BNNEWWS                            *
000300******************************************************************
000400 01  BNPTBWS-STORAGE.
AI0095     02  BNPTB-PROCESSED-DATE            PIC 9(08)  VALUE ZERO.
000500     02  BNPTB-SCR-FIELDS.
000600         03  BNPTB-FC                    PIC X(01)  VALUE SPACES.
000700         03  BNPTB-FC-FN                 PIC 9(04)  VALUE ZEROES.
000800         03  BNPTB-COMPANY               PIC 9(04)  VALUE ZEROES.
000900         03  BNPTB-COMPANY-FN            PIC 9(04)  VALUE ZEROES.
001000         03  BNPTB-PARTICIPNT            PIC 9(09)  VALUE ZEROES.
001100         03  BNPTB-PARTICIPNT-FN         PIC 9(04)  VALUE ZEROES.
001200         03  BNPTB-EMPLOYEE              PIC 9(09)  VALUE ZEROES.
001300         03  BNPTB-EMPLOYEE-FN           PIC 9(04)  VALUE ZEROES.
001400         03  BNPTB-ENROLLMENT-DATE       PIC 9(08)  VALUE ZEROES.
001500         03  BNPTB-ENROLLMENT-DATE-FN    PIC 9(04)  VALUE ZEROES.
001600         03  BNPTB-NBR-LINES             PIC 9(02)  VALUE ZEROES.
P53491 01  BNPTB-DETAIL-GROUP.
P53491     02  BNPTB-PARTBEN-DETAIL    OCCURS 15 TIMES.
001900******************************************************************
002000*                FIELDS ENTERED FROM DETAIL SCREEN               *
002100******************************************************************
002200                 05  BNPTB-LINE-FC       PIC X(01)  VALUE SPACES.
002300                 05  BNPTB-LINE-FC-FN    PIC 9(04)  VALUE ZEROES.
002400                 05  BNPTB-PLAN-INFO     PIC X(01)  VALUE SPACES.
002500                 05  BNPTB-RECORD-ORIG   PIC X(01)  VALUE SPACES.
002600                 05  BNPTB-PLAN-TYPE     PIC X(02)  VALUE SPACES.
002700                 05  BNPTB-PLAN-TYPE-FN  PIC 9(04)  VALUE ZEROES.
002800                 05  BNPTB-PLAN-CODE     PIC X(04)  VALUE SPACES.
002900                 05  BNPTB-PLAN-CODE-FN  PIC 9(04)  VALUE ZEROES.
003000                 05  BNPTB-COVER-OPT     PIC 9(02)  VALUE ZEROES.
003100                 05  BNPTB-COVER-OPT-FN  PIC 9(04)  VALUE ZEROES.
003200                 05  BNPTB-MULT-SALARY   PIC S9(02)V99
003300                                                    VALUE ZEROES.
003400                 05  BNPTB-MULT-SALARY-FN
003500                                         PIC 9(04)  VALUE ZEROES.
003600                 05  BNPTB-COVER-AMT     PIC S9(13)V99 COMP-3
003700                                                    VALUE ZEROES.
003800                 05  BNPTB-COVER-AMT-FN  PIC 9(04)  VALUE ZEROES.
003900                 05  BNPTB-PAY-PER-AMT   PIC S9(13)V99
004000                                                    VALUE ZEROES.
004100                 05  BNPTB-PAY-PER-AMT-FN
004200                                         PIC 9(04)  VALUE ZEROES.
004300                 05  BNPTB-PCT-AMT-FLAG  PIC X(01)  VALUE SPACES.
004400                 05  BNPTB-PCT-AMT-FLAG-FN
004500                                         PIC 9(04)  VALUE ZEROES.
004600                 05  BNPTB-START-DATE    PIC 9(08)  VALUE ZEROES.
004700                 05  BNPTB-START-DATE-FN PIC 9(04)  VALUE ZEROES.
004800                 05  BNPTB-PRE-AFT-FLAG  PIC X(01)  VALUE SPACES.
004900                 05  BNPTB-PRE-AFT-FLAG-FN
005000                                         PIC 9(04)  VALUE ZEROES.
005100                 05  BNPTB-SMOKER-FLAG   PIC X(01)  VALUE SPACES.
005200                 05  BNPTB-SMOKER-FLAG-FN
005300                                         PIC 9(04)  VALUE ZEROES.
J55910                 05  BNPTB-PROC-LEVEL    PIC X(05)  VALUE SPACES.
J55910                 05  BNPTB-PROC-LEVEL-FN PIC 9(04)  VALUE ZEROES.
005400                 05  BNPTB-ANNUAL-AMT    PIC S9(13)V99
005500                                                    VALUE ZEROES.
005600                 05  BNPTB-ANNUAL-AMT-FN PIC 9(04)  VALUE ZEROES.
005700                 05  BNPTB-PAY-RATE      PIC S9(13)V99
005800                                                    VALUE ZEROES.
005900                 05  BNPTB-PAY-RATE-FN   PIC 9(04)  VALUE ZEROES.
006000                 05  BNPTB-EMP-PRE-CONT  PIC S9(13)V99
006100                                                    VALUE ZEROES.
006200*                 05  BNPTB-EMP-PRE-CONT-FN
006300*                                         PIC 9(04)  VALUE ZEROES.
006400                 05  BNPTB-EMP-AFT-CONT  PIC S9(13)V99
006500                                                    VALUE ZEROES.
006600                 05  BNPTB-EMP-AFT-CONT-FN
006700                                         PIC 9(04)  VALUE ZEROES.
006800                 05  BNPTB-COMP-CONT     PIC S9(13)V99
006900                                                    VALUE ZEROES.
007000                 05  BNPTB-COMP-CONT-FN  PIC 9(04)  VALUE ZEROES.
007100                 05  BNPTB-STOP-DATE     PIC 9(08)  VALUE ZEROES.
007200                 05  BNPTB-STOP-DATE-FN  PIC 9(04)  VALUE ZEROES.
                       05  BNPTB-CREATE-TRANS    PIC X(01) VALUE SPACES.
                       05  BNPTB-CREATE-TRANS-FN PIC 9(04) VALUE ZEROES.
                       05  BNPTB-REASON          PIC X(10) VALUE SPACES.
                       05  BNPTB-REASON-FN       PIC 9(04) VALUE ZEROES.
                       05  BNPTB-MEMBER-ID       PIC 9(01) VALUE ZEROES.
                       05  BNPTB-MEMBER-ID-FN    PIC 9(04) VALUE ZEROES.
007300******************************************************************
007400*                SCREEN FIELDS END                               *
007500******************************************************************
007600******************************************************************
007700*                OUTPUT FIELDS RECEIVED FROM APIs NOT ON SCREEN  *
007800******************************************************************
007900******************************************************************
008000*                    RECEIVED FROM ELIGIBILITY API               *
008100******************************************************************
008200                 05  BNPTB-ELIG-UPD-DT   PIC 9(08)  VALUE ZEROES.
008300                 05  BNPTB-ELIG-GROUP    PIC X(10)  VALUE SPACES.
008400******************************************************************
008500*                    RECEIVED FROM COVERAGE API                  *
008600******************************************************************
008700                 05  BNPTB-DEP-COVER-AMT PIC S9(13)V99 COMP-3
008800                                                    VALUE ZEROES.
008900                 05  BNPTB-COV-UPD-DT    PIC 9(08)  VALUE ZEROES.
009000                 05  BNPTB-COV-GROUP     PIC X(10)  VALUE SPACES.
009100                 05  BNPTB-COV-OVER-FLG  PIC X(01)  VALUE SPACES.
009200                 05  BNPTB-COV-SAL-DATE  PIC 9(08)  VALUE ZEROES.
009300******************************************************************
009400*                    RECEIVED FROM CONTRIBUTION API              *
009500******************************************************************
009600                 05  BNPTB-PRE-MATCH-OPTION
009700                                         PIC X(01)  VALUE SPACES.
009800                 05  BNPTB-PREM-UPD-DT   PIC 9(08)  VALUE ZEROES.
009900                 05  BNPTB-PREM-GROUP    PIC X(10)  VALUE SPACES.
010000                 05  BNPTB-ZERO-PREMIUM-SW
010100                                         PIC 9(01)  VALUE ZEROES.
010200                 05  BNPTB-NEG-PREMIUM-SW
010300                                         PIC 9(01)  VALUE ZEROES.
010400                 05  BNPTB-POS-PREMIUM-SW
010500                                         PIC 9(01)  VALUE ZEROES.
010600******************************************************************
010700*                    FOLLOWING VARS SET IN ENTRY API             *
010800******************************************************************
010900                 05  BNPTB-PT-START-DATE PIC 9(08)  VALUE ZEROES.
011000*                 05  BNPTB-CMP-FLX-CONT  PIC S9(13)V99
011100*                                                    VALUE ZEROES.
011200                 05  BNPTB-FLEX-FLAG     PIC X(01)  VALUE SPACES.
011300                 05  BNPTB-SPEND-ONLY    PIC X(01)  VALUE SPACES.
011400*                05  BNPTB-STM-SEQ-NBR   PIC 9(04)  VALUE ZEROES.
011500*                05  BNPTB-TAKEN-FLAG    PIC X(01)  VALUE SPACES.
011600                 05  BNPTB-CHANGE-PREMIUM-SW
011700                                         PIC 9(01)  VALUE ZEROES.
011800                 05  BNPTB-COMP-CHANGED-SW
011900                                         PIC 9(01)  VALUE ZEROES.
012000*                05  BNPTB-TA-UPD-FLAG   PIC X(01)  VALUE SPACES.
012100
012200                 05  BNPTB-PRE-EMP-CONT  PIC S9(13)V99 COMP-3
012300                                                    VALUE ZEROES.
012400                 05  BNPTB-YTD-CONT      PIC S9(13)V99
012500                                                    VALUE ZEROES.
012600*                05  BNPTB-YTD-FLEX-CONT PIC S9(13)V99
012700*                                                   VALUE ZEROES.
012800                 05  BNPTB-SPOUSE-CVG    PIC S9(13)V99 COMP-3
012900                                                    VALUE ZEROES.
013000                 05  BNPTB-ANNUAL-COST   PIC S9(06)V99 COMP-3
013100                                                    VALUE ZEROES.
011200                 05  BNPTB-BATCH-FC      PIC X(02)  VALUE SPACES.
                       05  BNPTB-SV-SP-TBL.
                           06  BNPTB-SV-SP-OCC OCCURS 25 TIMES.
                               07  BNPTB-SV-SP-SPOUSE
                                               PIC 9(04)  VALUE ZEROES.
                               07  BNPTB-SV-SP-END-DT
                                               PIC 9(08)  VALUE ZEROES.
                               07  BNPTB-SV-SP-PLN-TP
                                               PIC X(02)  VALUE SPACES.
                               07  BNPTB-SV-SP-PLN-CD
                                               PIC X(04)  VALUE SPACES.
013200         03  BNPTB-C5-MAX-TABLE.
013300             04  BNPTB-C5-TABLE          OCCURS 24 TIMES.
013400                 05  BNPTB-C5-PLAN-CODE  PIC X(04)  VALUE SPACES.
013500                 05  BNPTB-C5-PLAN-MAX   PIC S9(13)V99 COMP-3
013600                                                    VALUE ZEROES.
013700                 05  BNPTB-C5-START-DATE PIC 9(08)  VALUE ZEROES.
013800                 05  BNPTB-C5-START      PIC 9(08)  VALUE ZEROES.
013900                 05  BNPTB-C5-STOP       PIC 9(08)  VALUE ZEROES.
014000
P53491     01  BNPTB-LOCAL-WS.
014200         03  BNPTB-RS-MAX-OC             PIC 9(02)  VALUE 13.
014300         03  BNPTB-START-DATE-CALC       PIC 9(08)  VALUE ZEROES.
014400         03  BNPTB-START-DATE-CALC-RED   REDEFINES
014500                                         BNPTB-START-DATE-CALC.
014600             04  BNPTB-START-YYYY        PIC 9(04).
014700             04  BNPTB-START-DATE-MMDD   PIC 9(04).
014800         03  BNPTB-EFD-START-DATE        PIC 9(08)  VALUE ZEROES.
014900         03  BNPTB-EFD-STOP-DATE         PIC 9(08)  VALUE ZEROES.
015000         03  BNPTB-EFD-GROUP-NAME        PIC X(10)  VALUE SPACES.
015100         03  BNPTB-DEP-STOP-DATE         PIC 9(08)  VALUE ZEROES.
015200         03  BNPTB-MAX-SALARY            PIC S9(09)V9999 COMP-3
015300                                                    VALUE ZEROES.
015400         03  BNPTB-SALARY                PIC S9(09)V9999 COMP-3
015500                                                    VALUE ZEROES.
015600         03  BNPTB-C5-ANNUAL-AMT         PIC S9(13)V99 
015700                                                    VALUE ZEROES.
016200         03  BNPTB-COV-TEMP              PIC S9(09)V99 COMP-3
016300                                                    VALUE ZEROES.
016400         03  BNPTB-HOURLY-RATE           PIC 9(05)V9(03) COMP-3 
016500                                                    VALUE ZEROES.
016600         03  BNPTB-PLAN-HOURLY-RATE      PIC 9(05)V9(03) COMP-3 
016700                                                    VALUE ZEROES.
016800         03  BNPTB-BEN-HOURLY-RATE       PIC 9(05)V9(03) COMP-3
016900                                                    VALUE ZEROES.
017000         03  BNPTB-QUOTIENT              PIC 9(09)V9(09) 
017100                                                    VALUE ZEROES.
017200         03  BNPTB-QUOTIENT-R            REDEFINES BNPTB-QUOTIENT.
017300             04  BNPTB-WHOLE             PIC 9(09).
017400             04  BNPTB-REMAINDER         PIC 9(09).
017500         03  BNPTB-NODECIMAL-AMT         PIC S9(13) COMP-3  
017600                                                    VALUE ZEROES.
017700         03  BNPTB-FLD-TO-ROUND          PIC S9(08)V999 COMP-3
017800                                                    VALUE ZEROES.
017900         03  BNPTB-DIFF                  PIC S9(08)V999 COMP-3
018000                                                    VALUE ZEROES.
018100         03  BNPTB-MID                   PIC S9(08)V999 COMP-3
018200                                                    VALUE ZEROES.
018300         03  BNPTB-BALANCED-DED          PIC X(01)  VALUE SPACES.
018400         03  BNPTB-FIRST-XMIT            PIC X(01)  VALUE SPACES.
018500         03  BNPTB-CREDITS-AVAILABLE     OCCURS 24 TIMES.
018600             05 BNPTB-CREDITS-AVAIL      PIC 9(13)V99 COMP-3
018700                                                    VALUE ZEROES.
018800             05 BNPTB-PRE-CREDITS-AVAIL  PIC 9(13)V99 COMP-3
018900                                                    VALUE ZEROES.
019000             05 BNPTB-CREDITS-SPENT      PIC 9(13)V99 COMP-3
019100                                                    VALUE ZEROES.
019200             05 BNPTB-PRE-CREDITS-SPENT  PIC 9(13)V99 COMP-3
019300                                                    VALUE ZEROES.
019400         03  BNPTB-USER-XFER             PIC X(01)  VALUE SPACES.
019500         03  BNPTB-DEPEND-SW             PIC 9(01)  VALUE ZEROES.
019600             88 BNPTB-DEPENDENTS-EXIST              VALUE 1.
019700             88 BNPTB-NO-DEPENDENTS                 VALUE 0.
019800         03  BNPTB-HRH-A-VALUE           PIC X(10)  VALUE SPACES.
J46844         03  BNPTB-ANNUAL-HOURS          LIKE ANNUAL-HOURS
                                                          VALUE ZEROES.
020000         03  BNPTB-PREM-EMP-CONT         PIC 9(13)V99 COMP-3
020100                                                    VALUE ZEROES.
020200         03  BNPTB-EMP-PREMIUM           PIC 9(13)V99 COMP-3
020300                                                    VALUE ZEROES.
020400         03  BNPTB-EXCESS-PREM           PIC 9(13)V99 COMP-3
020500                                                    VALUE ZEROES.
020600         03  BNPTB-COST-PER-PCT          PIC 9(13)V9(04) COMP-3
020700                                                    VALUE ZEROES.
020800         03  BNPTB-SAVE-COMPANY          PIC 9(04)  VALUE ZEROES.
020900         03  BNPTB-SAVE-PARTICIPNT       PIC 9(09)  VALUE ZEROES.
021000         03  BNPTB-SAVE-EMPLOYEE         PIC 9(09)  VALUE ZEROES.
021100         03  BNPTB-SAVE-START-DATE       PIC 9(08)  VALUE ZEROES.
021200         03  BNPTB-SAVE-PLAN-TYPE        PIC X(02)  VALUE SPACES.
021300         03  BNPTB-SAVE-PLAN-CODE        PIC X(04)  VALUE SPACES.
021400         03  BNPTB-C5-MAX-OC             PIC 9(02)  VALUE 13.
021500         03  BNPTB-BENEFS-SW             PIC 9(01)  VALUE 0.
021600             88  BNPTB-BENEFS-NOT-OK                VALUE 0.
021700             88  BNPTB-BENEFS-OK                    VALUE 1.
021800         03  BNPTB-DEL-PTB-SW            PIC 9(01)  VALUE 0.
021900             88  BNPTB-NOT-DELETING-PTB             VALUE 0.
022000             88  BNPTB-DELETING-PTB                 VALUE 1.
022100         03  BNPTB-CHG-PTB-SW            PIC 9(01)  VALUE 0.
022200             88  BNPTB-NOT-CHANGING-PTB             VALUE 0.
022300             88  BNPTB-CHANGING-PTB                 VALUE 1.
022400         03  BNPTB-STOP-DFLT-SW          PIC 9(01)  VALUE 0.
022500             88  BNPTB-STOP-DEFAULTED               VALUE 1.
022600         03  BNPTB-PART-ENT-SW           PIC 9(01)  VALUE 0.
022700             88  BNPTB-PART-ENT                     VALUE 1.
022800         03  BNPTB-RETIREE-ENT-SW        PIC 9(01)  VALUE 0.
022900             88  BNPTB-RETIREE-ENT                  VALUE 1.
023000         03  BNPTB-BNAWS                 OCCURS 15 TIMES.
023100             04  BNPTB-BNA-START-DATE    PIC 9(08)  VALUE ZEROES.
023200             04  BNPTB-BNA-GROUP-NAME    PIC X(10)  VALUE ZEROES.
023300             04  BNPTB-CNT-SAL-DATE      PIC 9(08)  VALUE ZEROES.
023400             04  BNPTB-CNT-AGE-DATE      PIC 9(08)  VALUE ZEROES.
023500             04  BNPTB-CNT-SERV-DATE     PIC 9(08)  VALUE ZEROES.
023600             04  BNPTB-COMP-CONT-SW      PIC X(01)  VALUE SPACES.
023700
023800         03  BNPTB-I1                    PIC 9(04)  COMP-3
023900                                                    VALUE ZEROES.

183700         03  BNPTB-TRAN-SEQ-NBR         LIKE SEQ-NBR VALUE ZEROES.
RAY            03  WS-BNPTB-START-DATE         PIC 9(8)   VALUE ZERO.
GW             03  WS-BNPTB-STOP-DATE          PIC 9(8)   VALUE ZERO.
RAY            03  WS-BNPTB-NOTFND-SW-SAVE     PIC 9(01) VALUE ZERO.


               03  BNPTB-HIPAA-DEFAULT-SW      PIC X(05)   VALUE SPACES.
                   88  BNPTB-HIPAA-DEFAULT                 VALUE "BN100"
                                                                 "BN101"
                                                                 "BN102"
                                                                 "BN103"
                                                                "BN105".
               03  BNPTB-HIPAA-REASON          LIKE REASON VALUE SPACES.
               03  BNPTB-BT                    LIKE TYPE  VALUE "BT".
183900         03  BNPTB-FUNCTION-CODE     PIC X(04)       VALUE SPACES.
183900         03  BNPTB-COVER-TYPE        LIKE COVER-TYPE VALUE SPACES.
J93630         03  BNPTB-SAVE-BEG-YR           PIC 9(04)   VALUE ZEROES.
J93630         03  BNPTB-SAVE-END-YR           PIC 9(04)   VALUE ZEROES.
