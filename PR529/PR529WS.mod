******* PR529WS 5.1.3 <1784599001>
000100******************************************************************
000200*                                PR529WS                         *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   --------------------------------------------*
      * 717636 | J17636 | Increase Bank Account number from 17 to 40   *
000300******************************************************************
      *  JT#       TAG     DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      * 1248725 | J48725 | Increase Tax Category from 2 to 4           *
      ******************************************************************
000400 01  PR529WS.
000500*
002400     02  WS-TE-TOT-EMPLOYEES         PIC 9(09)       VALUE ZEROS.
           02  WS-NBR-PAYMENTS             PIC 9(09)       VALUE ZEROS.
           02  WS-NBR-OF-UPDS              PIC 9(09)       VALUE ZEROS.
002500*
003600     02  WS-PENSION-ID-MATCH-SW      PIC 9(01)       VALUE ZEROES.
003700         88 NO-PENSION-IDR-MATCH                     VALUE 0.
003800         88 PENSION-ID-MATCH                         VALUE 1.
004100     02  WS-END-OF-FILE-SW           PIC 9(01)       VALUE ZEROS.
004200         88 END-OF-FILE                              VALUE 1.
004300     02  WS-PAYMENT-PROCESSED-SW     PIC 9(01)       VALUE ZEROS.
004400         88  NO-PAYMENT-PROCESSED                    VALUE 0.
004400         88  PAYMENT-PROCESSED                       VALUE 1.
004500     02  WS-RECORDS-FOUND-SW         PIC 9(01)       VALUE ZEROS.
004700         88  NO-RECORDS-FOUND                        VALUE ZEROS.
004600         88  RECORDS-FOUND                           VALUE 1.
004800     02  WS-ERROR-RECORDS-SW         PIC 9(01)       VALUE ZEROES.
004900         88  NO-ERR-RECS-WRITTEN                     VALUE 0.
004900         88  ERR-RECS-WRITTEN                        VALUE 1.
005000*
J65502     02  WS-ERRORFILE-NAME           LIKE FILENAME   VALUE SPACES.
006900     02  WS-PREC-PROCESSED           PIC 9(08)       VALUE ZEROES.
007000     02  WS-PREC-IN-TRAN             PIC 9(04)       VALUE ZEROES.
      *
011100     02  WS-RESTART-SW               PIC 9(01)       VALUE ZEROES.
011200         88  WS-NOT-RESTARTING                       VALUE 0.
011200         88  WS-RESTARTING                           VALUE 1.
011300
011800     02  WS-NBR-RECS                 PIC 9(12)       VALUE ZEROES.

011400     02  WS-RESTART-INFO.
011500         03  WS-RS-COMPANY           PIC 9(04)       VALUE ZEROES.
011800         03  WS-RS-EMPLOYEE          PIC 9(09)       VALUE ZEROES.
011800         03  WS-RS-NBR-RECS          PIC 9(12)       VALUE ZEROES.
               03  WS-RS-TOT-EMP           PIC 9(08)       VALUE ZEROES.
               03  WS-RS-TOT-ERRORS        PIC 9(08)       VALUE ZEROES.

           02  WS-TOT-ERRORS               PIC 9(08)       VALUE ZEROES.
           02  WS-QUIT-PROG-SW             PIC 9(01)       VALUE ZEROES.
               88  NO-QUIT-PROG                            VALUE 0.
               88  QUIT-PROG                               VALUE 1.
           02  WS-COMPANY                  PIC 9(04)       VALUE ZEROES.
           02  WS-PENSION-ID               PIC 9(08)       VALUE ZEROES.
           02  WS-EMPLOYEE                 PIC 9(09)       VALUE ZEROES.
           02  WS-EFFECT-DATE              PIC 9(08)       VALUE ZEROES.
           02  WS-END-DATE                 PIC 9(08)       VALUE ZEROES.
P68632     02  WS-ROTH-IC-YEAR             PIC 9(04)       VALUE ZEROES.
           02  WS-I1                       PIC 9(04)       VALUE ZEROES.
           02  WS-I2                       PIC 9(04)       VALUE ZEROES.
           02  WS-I3                       PIC 9(04)       VALUE ZEROES.
           02  WS-I4                       PIC 9(04)       VALUE ZEROES.

007300     02  WS-IFC.
               03  WS-IFC-RECORD-TYPE      PIC X(01)       VALUE SPACES.
007700         03  WS-IFC-PENSION-ID       PIC 9(04)       VALUE ZEROES.
007800         03  WS-IFC-PENSION-ID-A     REDEFINES
                                           WS-IFC-PENSION-ID
007900                                     PIC X(04).
007400         03  WS-IFC-COMPANY          PIC 9(04)       VALUE ZEROES.
007500         03  WS-IFC-COMPANY-A        REDEFINES
                                           WS-IFC-COMPANY
007600                                     PIC X(04).
008000         03  WS-IFC-EMPLOYEE         PIC 9(09)       VALUE ZEROES.
008100         03  WS-IFC-EMPLOYEE-A       REDEFINES
                                           WS-IFC-EMPLOYEE
008200                                     PIC X(09).
008300         03  WS-IFC-EFFECT-DATE      PIC 9(08)       VALUE ZEROES.
008400         03  WS-IFC-EFFECT-DATE-A    REDEFINES
                                           WS-IFC-EFFECT-DATE
008500                                     PIC X(08).
008300         03  WS-IFC-END-DATE         PIC 9(08)       VALUE ZEROES.
008400         03  WS-IFC-END-DATE-A       REDEFINES
                                           WS-IFC-END-DATE
008500                                     PIC X(08).
008300         03  WS-IFC-PENS-DIST-TYPE   PIC 9(08)       VALUE ZEROES.
008400         03  WS-IFC-PENS-DIST-TYPE-A REDEFINES
                                           WS-IFC-PENS-DIST-TYPE
008500                                     PIC X(08).
               03  WS-IFC-PENS-DIST-CODE   PIC X(01)       VALUE SPACES.
               03  WS-IFC-PENS-DST-CODE2   PIC X(01)       VALUE SPACES.
J48725         03  WS-IFC-TAX-CATEGORY     PIC 9(04)       VALUE ZEROES.
008400         03  WS-IFC-TAX-CATEGORY-A   REDEFINES
                                           WS-IFC-TAX-CATEGORY
J48725                                     PIC X(04).
               03  WS-IFC-PROCESS-LEVEL    PIC X(05)       VALUE SPACES.
               03  WS-IFC-DEPARTMENT       PIC X(05)       VALUE SPACES.
008300         03  WS-IFC-ADDL-FED-TAX     PIC 9(07)V99    VALUE ZEROES.
008400         03  WS-IFC-ADDL-FED-TAX-A   REDEFINES
                                           WS-IFC-ADDL-FED-TAX
008500                                     PIC X(09).
008300         03  WS-IFC-TAX-FREQ-OVERRIDE
                                           PIC 9(01)       VALUE ZEROES.
008400         03  WS-IFC-TAX-FREQ-OVERRIDE-A
                                           REDEFINES
                                           WS-IFC-TAX-FREQ-OVERRIDE
008500                                     PIC X(01).
               03  WS-IFC-PROCESS-GRP      PIC X(01)       VALUE SPACES.
008300         03  WS-IFC-PERC-TOT-DIST    PIC 9(03)       VALUE ZEROES.
008400         03  WS-IFC-PERC-TOT-DIST-A  REDEFINES
                                           WS-IFC-PERC-TOT-DIST
008500                                     PIC X(03).
               03  WS-IFC-PAYABLE-TO       PIC X(30)       VALUE SPACES.
               03  WS-IFC-FBO-NAME         PIC X(30)       VALUE SPACES.
               03  WS-IFC-ADDR1            PIC X(30)       VALUE SPACES.
               03  WS-IFC-ADDR2            PIC X(30)       VALUE SPACES.
               03  WS-IFC-ADDR3            PIC X(30)       VALUE SPACES.
               03  WS-IFC-ADDR4            PIC X(30)       VALUE SPACES.
P80538         03  WS-IFC-CITY             PIC X(30)       VALUE SPACES.
               03  WS-IFC-STATE            PIC X(02)       VALUE SPACES.
               03  WS-IFC-POSTAL-CODE      PIC X(10)       VALUE SPACES.
               03  WS-IFC-COUNTRY-CODE     PIC X(02)       VALUE SPACES.
               03  WS-IFC-ROLLOVER-ACCT    PIC X(17)       VALUE SPACES.
               03  WS-IFC-MISCELLANEOUS    PIC X(06)       VALUE SPACES.
               03  WS-IFC-MISCELLANEOUS2   PIC X(06)       VALUE SPACES.
008300         03  WS-IFC-WIRE-IND         PIC 9(01)       VALUE ZEROES.
008400         03  WS-IFC-WIRE-IND-A       REDEFINES
                                           WS-IFC-WIRE-IND
008500                                     PIC X(01).
               03  WS-IFC-CA-EFT           PIC X(01)       VALUE SPACES.
               03  WS-IFC-FUNDING-SOURCE   PIC X(06)       VALUE SPACES.
008300         03  WS-IFC-FUND-AMOUNT      PIC 9(09)V99    VALUE ZEROES.
008400         03  WS-IFC-FUND-AMOUNT-A    REDEFINES
                                           WS-IFC-FUND-AMOUNT
008500                                     PIC X(11).
               03  WS-IFC-PAY-CODE         PIC X(04)       VALUE SPACES.
008300         03  WS-IFC-AMOUNT-RATE      PIC 9(09)V9999  VALUE ZEROES.
008400         03  WS-IFC-AMOUNT-RATE-A    REDEFINES
                                           WS-IFC-AMOUNT-RATE
008500                                     PIC X(13).
008300         03  WS-IFC-EBANK-ID         PIC 9(09)       VALUE ZEROES.
008400         03  WS-IFC-EBANK-ID-A       REDEFINES
                                           WS-IFC-EBANK-ID
008500                                     PIC X(09).
               03  WS-IFC-BANK-NAME        PIC X(30)       VALUE SPACES.
      *        03  WS-IFC-EBNK-ACCT-NBR    PIC X(17)       VALUE SPACES.
J17636         03  WS-IFC-EBNK-ACCT-NBR    PIC X(40)       VALUE SPACES.
               03  WS-IFC-ACCOUNT-TYPE     PIC X(01)       VALUE SPACES.
               03  WS-IFC-PAYMENT-DESCRIPTION
                                           PIC X(30)       VALUE SPACES.
P68632         03  WS-IFC-ROTH-YEAR
P68632                                     PIC 9(04)       VALUE ZEROES.
P68632         03  WS-IFC-ROTH-YEAR-A
P68632                                     REDEFINES
P68632                                     WS-IFC-ROTH-YEAR
P68632                                     PIC X(04).
GW1104         03  WS-IFC-BEN-START-DATE   PIC 9(08)       VALUE ZEROES.
GW1104         03  WS-IFC-BEN-START-DATE-A REDEFINES
GW1104                                     WS-IFC-BEN-START-DATE
GW1104                                     PIC X(08).
GW1104         03  WS-IFC-BEN-STOP-RSN     PIC X(10)       VALUE SPACES.

J93849         03  WS-IFC-IAT-ACH          PIC X(01)       VALUE SPACES.


003600     02  WS-DATA-FOUND-SW            PIC 9(01)       VALUE ZEROES.
003700         88 NO-DATA-FOUND                            VALUE 0.
003800         88 DATA-FOUND                               VALUE 1.

GW1104     02 WS-TAG-READ-CTR              PIC 9(07)       VALUE ZERO.
GW1104     02 WS-TAG-ADD-CTR               PIC 9(07)       VALUE ZERO.
GW1104     02 WS-TAG-CHG-CTR               PIC 9(07)       VALUE ZERO.
GW1104     02 WS-TAG-SKIP-CTR              PIC 9(07)       VALUE ZERO.
GW1104
GW1104     02 WS-PENS-SEQ-NBR              PIC 9(04) VALUE ZERO.
