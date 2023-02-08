******* BN100WS 29 <1767964531>
000100******************************************************************
000200*                             BN100WS                            *
000300******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  363718 | J63718 | INCLUDE ADDITIONAL VARIABLES TO HOLD        *
      *         |        | PREVIOUSLY PRINTED RECORD ON REPORT.        *
      *  ------   ------   ------------------------------------------  *
      *  666766 | J66766 | ADDED WS-PREV-PLN-DED-CODE & WS-DED-CODE-SW *
      *  ------   ------   ------------------------------------------  *
      * 1170393 | 170393 | ADDED START-DATE HOLD VARIABLE              *
      ******************************************************************
000400 01  BN100WS.
000100******************************************************************
000200*                     RESTART RELATED VARIABLES                  *
000300******************************************************************
002100     02  WS-RESTART-DATA.
002200         05  WS-RESTART-PHASE        PIC 9(01) VALUE ZEROES.
002200             88  CREATE-WORK                   VALUE 1 0.
002200             88  PROCESS-BN100EFDC             VALUE 2.
002200             88  PROCESS-BN100EFDA             VALUE 3.
002200             88  PROCESS-BN100BENC             VALUE 4.
002200             88  PROCESS-BN100BENA             VALUE 5.
002200             88  PRINT-REPORT                  VALUE 6.
002200             88  PURGE-BNG-BNH                 VALUE 7.
J65504         05  WS-REST-WORK-FILE       LIKE FILENAME VALUE SPACES.
J65504         05  WS-REST-EFD-CHANGE-FILE LIKE FILENAME VALUE SPACES.
J65504         05  WS-REST-BEN-CHANGE-FILE LIKE FILENAME VALUE SPACES.
J65504         05  WS-REST-EFD-ADD-FILE    LIKE FILENAME VALUE SPACES.
J65504         05  WS-REST-BEN-ADD-FILE    LIKE FILENAME VALUE SPACES.
012600         05  WS-RESTART-COUNT        PIC 9(12) VALUE ZEROES.
012600         05  WS-REST-EMP-COUNT       PIC 9(12) VALUE ZEROES.
002200         05  WS-REST-FILE-NAME       PIC X(10) VALUE SPACES.
002200         05  WS-REST-NAV-POINTER     PIC 9(02) VALUE ZEROES.
001900         05  WS-REST-EFDA-DATA-SW    PIC 9(01) VALUE ZEROES.
001900         05  WS-REST-EFDC-DATA-SW    PIC 9(01) VALUE ZEROES.
001900         05  WS-REST-BENA-DATA-SW    PIC 9(01) VALUE ZEROES.
001900         05  WS-REST-BENC-DATA-SW    PIC 9(01) VALUE ZEROES.
J65504         05  WS-REST-WORKS-FILE      LIKE FILENAME VALUE SPACES.
J65504         05  WS-REST-WORKI-FILE      LIKE FILENAME VALUE SPACES.
J65504     02  WS-WORK-FILE                LIKE FILENAME VALUE SPACES.
J65504     02  WS-WORKS-FILE               LIKE FILENAME VALUE SPACES.
J65504     02  WS-WORKI-FILE               LIKE FILENAME VALUE SPACES.
J65504     02  WS-EFD-CHANGE-FILE          LIKE FILENAME VALUE SPACES.
J65504     02  WS-BEN-CHANGE-FILE          LIKE FILENAME VALUE SPACES.
J65504     02  WS-EFD-ADD-FILE             LIKE FILENAME VALUE SPACES.
J65504     02  WS-BEN-ADD-FILE             LIKE FILENAME VALUE SPACES.
012600     02  WS-RECORD-COUNT             PIC 9(12) VALUE ZEROES.
012600     02  WS-EMP-REC-COUNT            PIC 9(12) VALUE ZEROES.
002200     02  WS-FILE-NAME                PIC X(10) VALUE SPACES.
002200     02  WS-BENA-FILE-NAME           PIC X(10) VALUE
                                                     "BN100BENA".
012600     02  WS-UPDATE-COUNT             PIC 9(04) VALUE ZEROES.
002200     02  WS-NAV-POINTER              PIC 9(02) VALUE ZEROES.
PERF       02  WS-BN100-MSG-114            PIC X(60) VALUE SPACES.
PERF       02  WS-BN100-MSG-115            PIC X(60) VALUE SPACES.
PERF       02  WS-BN100-MSG-130            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-190            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-191            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-192            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-193            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-194            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-195            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-196            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-198            PIC X(60) VALUE SPACES.
PERF       02  WS-BNMSG-MSG-203            PIC X(60) VALUE SPACES.
PERF       02  WS-BNCOM-MSG-107            PIC X(60) VALUE SPACES.
PERF       02  WS-BNCOM-MSG-108            PIC X(60) VALUE SPACES.
000100******************************************************************
000200*                         SWITCHES/FLAGS                         *
000300******************************************************************
001900     02  WS-BN100WORK-SW             PIC 9(01) VALUE ZERO.
002000         88  BN100WORK-FOUND                   VALUE ZERO.
002000         88  BN100WORK-NOTFOUND                VALUE 1.
001900     02  WS-BN100BENC-SW             PIC 9(01) VALUE ZERO.
002000         88  BN100BENC-FOUND                   VALUE ZERO.
002000         88  BN100BENC-NOTFOUND                VALUE 1.
001900     02  WS-BN100EFDC-SW             PIC 9(01) VALUE ZERO.
002000         88  BN100EFDC-FOUND                   VALUE ZERO.
002000         88  BN100EFDC-NOTFOUND                VALUE 1.
001900     02  WS-BN100BENA-SW             PIC 9(01) VALUE ZERO.
002000         88  BN100BENA-FOUND                   VALUE ZERO.
002000         88  BN100BENA-NOTFOUND                VALUE 1.
001900     02  WS-BN100EFDA-SW             PIC 9(01) VALUE ZERO.
002000         88  BN100EFDA-FOUND                   VALUE ZERO.
002000         88  BN100EFDA-NOTFOUND                VALUE 1.
001900     02  WS-END-OF-FILE-SW           PIC 9(01) VALUE ZERO.
002000         88  NO-END-OF-FILE                    VALUE 0.
002000         88  END-OF-FILE                       VALUE 1.
001900     02  WS-PROC-BNG-BNH-SW          PIC 9(01) VALUE ZERO.
002000         88  PROC-BNG                          VALUE ZERO.
002000         88  PROC-BNH                          VALUE 1.
001900     02  WS-PROC-EFD-BEN-SW          PIC 9(01) VALUE ZERO.
002000         88  PROC-OFF                          VALUE ZERO.
002000         88  PROC-EFD                          VALUE 1.
002000         88  PROC-BEN                          VALUE 2.
002000         88  PROC-PLN                          VALUE 3.
001900     02  WS-PAY-FREQ-CHG-SW          PIC 9(01) VALUE ZERO.
002000         88  NO-PAY-FREQ-CHG                   VALUE ZERO.
002000         88  PAY-FREQ-CHG                      VALUE 1.
001900     02  WS-PROC-FLEX-SW             PIC 9(01) VALUE ZERO.
002000         88  PROC-NON-FLEX                     VALUE ZERO.
002000         88  PROC-FLEX                         VALUE 1.
001900     02  WS-LOOP-SW                  PIC 9(01) VALUE ZERO.
002000         88  NO-QUIT-LOOP                      VALUE ZERO.
002000         88  QUIT-LOOP                         VALUE 1.
001900     02  WS-DUP-EFD-SW               PIC 9(01) VALUE ZERO.
002000         88  NO-DUP-EFD                        VALUE ZERO.
002000         88  DUP-EFD                           VALUE 1.
001900     02  WS-DUP-WF3-SW               PIC 9(01) VALUE ZERO.
002000         88  NO-DUP-WF3                        VALUE ZERO.
002000         88  DUP-WF3                           VALUE 1.
001900     02  WS-DUP-BEN-SW               PIC 9(01) VALUE ZERO.
002000         88  NO-DUP-BEN                        VALUE ZERO.
002000         88  DUP-BEN                           VALUE 1.
001900     02  WS-RULE-TYPE-SW             PIC X(01) VALUE SPACES.
002000         88  TERM-RULES                        VALUE "T".
002000         88  CHANGE-RULES                      VALUE "C".
002000         88  ADD-RULES                         VALUE "A".
001900     02  WS-CHANGE-SW                PIC X(01) VALUE SPACES.
002000         88  DEL-GROUP-CHANGES                 VALUE "D".
002000         88  ZIP-CODE-CHANGES                  VALUE "Z".
002000         88  SALARY-CHANGES                    VALUE "S".
002000         88  PAY-FREQ-CHANGES                  VALUE "P".
002000         88  ADD-GROUP-CHANGES                 VALUE "A".
002000         88  NEW-HIRE-CHANGES                  VALUE "H".
001900     02  WS-ERROR-FOR-CA-SW          PIC 9(01) VALUE ZEROES.
002000         88  NO-ERROR-FOR-CA                   VALUE ZERO.
002000         88  ERROR-FOR-CA                      VALUE 1.
001900     02  WS-NAVIGATE-SW              PIC 9(01) VALUE ZERO.
002000         88  NO-NAV-SEQ                        VALUE ZERO.
002000         88  USE-NAV-SEQ                       VALUE 1.
001900     02  WS-PRINT-PF-MSG-SW          PIC 9(01) VALUE ZERO.
002000         88  NO-PRINT-PF-MSG                   VALUE ZERO.
002000         88  PRINT-PF-MSG                      VALUE 1.
001900     02  WS-PRINT-BD-MSG-SW          PIC 9(01) VALUE ZERO.
002000         88  NO-PRINT-BD-MSG                   VALUE ZERO.
002000         88  PRINT-BD-MSG                      VALUE 1.
001900     02  WS-PRINT-SD-MSG-SW          PIC 9(01) VALUE ZERO.
002000         88  NO-PRINT-SD-MSG                   VALUE ZERO.
002000         88  PRINT-SD-MSG                      VALUE 1.
001900     02  WS-PRINT-SF-MSG-SW          PIC 9(01) VALUE ZERO.
002000         88  NO-PRINT-SF-MSG                   VALUE ZERO.
002000         88  PRINT-SF-MSG                      VALUE 1.
001900     02  WS-EFDA-DATA-SW             PIC 9(01) VALUE ZERO.
002000         88  NO-EFDA-DATA                      VALUE ZERO.
002000         88  EFDA-DATA                         VALUE 1.
001900     02  WS-EFDC-DATA-SW             PIC 9(01) VALUE ZERO.
002000         88  NO-EFDC-DATA                      VALUE ZERO.
002000         88  EFDC-DATA                         VALUE 1.
001900     02  WS-BENA-DATA-SW             PIC 9(01) VALUE ZERO.
002000         88  NO-BENA-DATA                      VALUE ZERO.
002000         88  BENA-DATA                         VALUE 1.
001900     02  WS-BENC-DATA-SW             PIC 9(01) VALUE ZERO.
002000         88  NO-BENC-DATA                      VALUE ZERO.
002000         88  BENC-DATA                         VALUE 1.
001900     02  WS-CREATE-PAR-SW            PIC 9(01) VALUE ZERO.
002000         88  NO-CREATE-PAR                     VALUE ZERO.
002000         88  CREATE-PAR                        VALUE 1.
013300     02  WS-NUMBER-SW                PIC 9(01) VALUE ZERO.
013400         88  WS-NUMBER-FOUND                   VALUE 1.
001900     02  WS-FLEX-FLAG                PIC X(01) VALUE SPACES.
013300     02  WS-DETAIL-PRINTED-SW        PIC 9(01) VALUE ZERO.
013400         88  DETAIL-NOT-PRINTED                VALUE 0.
013400         88  DETAIL-PRINTED                    VALUE 1.
013300     02  WS-PROC-163-ERR-SW          PIC 9(01) VALUE ZERO.
013400         88  NO-PROC-163-ERR                   VALUE 0.
013400         88  PROC-163-ERR                      VALUE 1.
001900     02  WS-GRP-W-GRP-SW             PIC 9(01) VALUE ZERO.
002000         88  GWG-RULES-FOUND                   VALUE ZERO.
002000         88  GWG-RULES-NOTFOUND                VALUE 1.
001900     02  WS-MOVE-BNBEN-LINE-SW       PIC 9(01) VALUE ZERO.
002000         88  MOVE-1ST-BNBEN-LINE               VALUE ZERO.
002000         88  MOVE-2ND-BNBEN-LINE               VALUE 1.
001900     02  WS-CREATE-ONLY-BENA-SW      PIC 9(01) VALUE ZERO.
002000         88  NO-CREATE-ONLY-BENA               VALUE ZERO.
002000         88  CREATE-ONLY-BENA                  VALUE 1.
001900     02  WS-RECOMPUTE-BEN-SW         PIC 9(01) VALUE ZERO.
002000         88  NO-RECOMPUTE-BEN                  VALUE ZERO.
002000         88  RECOMPUTE-BEN                     VALUE 1.
001900     02  WS-ERROR-IN-EFD-SW          PIC 9(01) VALUE ZERO.
002000         88  NO-ERROR-IN-EFD                   VALUE ZERO.
002000         88  ERROR-IN-EFD                      VALUE 1.
           02  WS-CONT-7-SAL-CHG-SW        PIC 9(01) VALUE ZERO.
               88  CONT-7-SAL-CHG                    VALUE 1.
           02  WS-COMP-EMP-SW              PIC 9(01) VALUE ZERO.
               88  SELECT-EMPLOYEE                   VALUE 0.
               88  SELECT-COMPANY                    VALUE 1.
           02  WS-SKIP-CHANEG-STOP-SW      PIC 9(01) VALUE ZERO.
               88  NOT-SKIP-CHANGE-STOP              VALUE 0.
               88  SKIP-CHANGE-STOP                  VALUE 1.
P09149     02  WS-CO-ERR-SW                PIC 9(01) VALUE ZERO.
P09149         88  COMPANY-HAS-ERROR                 VALUE 1.
P09149     02  WS-PL-ERR-SW                PIC 9(01) VALUE ZERO.
P09149         88  PL-HAS-ERROR                      VALUE 1.
P09149     02  WS-EMP-ERR-SW               PIC 9(01) VALUE ZERO.
P09149         88  EMP-HAS-ERROR                     VALUE 1.
P09149     02  WS-EFF-ERR-SW               PIC 9(01) VALUE ZERO.
P09149         88  EFF-HAS-ERROR                     VALUE 1.
           02  WS-PL-WARN-SW               PIC 9(01) VALUE ZERO.
               88  PL-HAS-WARN                       VALUE 1.
           02  WS-EMP-WARN-SW              PIC 9(01) VALUE ZERO.
               88  EMP-HAS-WARN                      VALUE 1.
           02  WS-EFF-WARN-SW              PIC 9(01) VALUE ZERO.
               88  EFF-HAS-WARN                      VALUE 1.
           02  WS-ELIGIBILITY-SW           PIC 9(01) VALUE ZERO.
               88  NO-ELIG-ERROR                     VALUE 0.
               88  ELIG-ERROR                        VALUE 1.
P43669     02  WS-G3-ARRAY-FULL-SW         PIC 9(01) VALUE ZERO.
P43669         88  WS-G3-ARRAY-FULL                  VALUE 1.
J66766     02  WS-DED-CODE-SW              PIC 9(01) VALUE ZERO.
J66766         88  WS-DIFF-DED-CODE                  VALUE 0.
J66766         88  WS-SAME-DED-CODE                  VALUE 1.
000100******************************************************************
000200*                           COUNTERS                             *
000300******************************************************************
002000     02  WS-GROUP-CNT                PIC 9(04) VALUE ZEROES.
002000     02  WS-I1                       PIC 9(04) VALUE ZEROES.
002000     02  WS-I2                       PIC 9(04) VALUE ZEROES.
002000     02  WS-I3                       PIC 9(04) VALUE ZEROES.
002000     02  WS-I4                       PIC 9(04) VALUE ZEROES.
           02  WS-I6                       PIC 9(04) VALUE ZEROES.
001900     02  WS-SEQ-NBR                  PIC 9(04) COMP-3
001900                                               VALUE ZEROES.
002000     02  WS-PARTICIPNT               PIC 9(09) VALUE ZEROES.
000100******************************************************************
000200*                     DUPLICATE RECORD VARIABLES                 *
000300******************************************************************
           02  WS-BN100WORK-KEY.
               03  WS3-COMPANY             PIC 9(04) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS3-EMPLOYEE            PIC 9(09) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS3-EFFECT-DATE         PIC 9(08) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS3-REC-TYPE            PIC X(01) VALUE SPACES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS3-SEQ-NBR             PIC 9(04) VALUE ZEROES.
           02  WS-BN100BEN-KEY.
               03  WS1-COMPANY             PIC 9(04) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS1-EMPLOYEE            PIC 9(09) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS1-PLAN-TYPE           PIC X(02) VALUE SPACES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS1-PLAN-CODE           PIC X(04) VALUE SPACES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS1-START-DATE          PIC 9(08) VALUE ZEROES.
           02  WS-BN100EFD-KEY.
               03  WS2-COMPANY             PIC 9(04) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS2-EMPLOYEE            PIC 9(09) VALUE ZEROES.
               03  FILLER                  PIC X(01) VALUE SPACES.
               03  WS2-START-DATE          PIC 9(08) VALUE ZEROES.
000100******************************************************************
000200*                           TABLES                               *
000300******************************************************************
002000     02  WS-OLD-CYCLE                OCCURS 9 TIMES
                                           PIC X(01).
002000     02  WS-NEW-CYCLE                OCCURS 9 TIMES
                                           PIC X(01).
           02  WS-EFR-TABLE.
               03  WS-EFD-EMPLOYEE-TBL     OCCURS 24 TIMES.
                   04  WS-EFD-EMPLOYEE     PIC 9(09) VALUE ZEROES.
                   04  WS-EFD-START-DT     PIC 9(08) VALUE ZEROES.
                   04  WS-EFR-PERIOD-1     OCCURS 24 TIMES.
                       05 WS-START-DT      PIC 9(08) VALUE ZEROES.
                       05 WS-STOP-DT       PIC 9(08) VALUE ZEROES.
           02  WS-EFR-TABLE-1.
               03  WS-EFR-AMTS             OCCURS 24 TIMES.
                   04  WS-EFR-PERIOD-2     OCCURS 24 TIMES.
                       05 WS-CREDITS-AVAIL PIC S9(13)V99  VALUE ZEROES.
                       05 WS-PRE-TAX-AVAIL PIC S9(13)V99  VALUE ZEROES.
002000     02  WS-MNTHS-TBL                PIC X(24) VALUE SPACES.
002000     02  WS-MNTHS-TBL-RED            REDEFINES WS-MNTHS-TBL
                                           OCCURS 12 TIMES
                                           PIC 9(02).
           02  WS-DATE-TABLE.
               03  FILLER                  PIC X(02) VALUE "31".
               03  FILLER                  PIC X(02) VALUE "28".
               03  FILLER                  PIC X(02) VALUE "31".
               03  FILLER                  PIC X(02) VALUE "30".
               03  FILLER                  PIC X(02) VALUE "31".
               03  FILLER                  PIC X(02) VALUE "30".
               03  FILLER                  PIC X(02) VALUE "31".
               03  FILLER                  PIC X(02) VALUE "31".
               03  FILLER                  PIC X(02) VALUE "30".
               03  FILLER                  PIC X(02) VALUE "31".
               03  FILLER                  PIC X(02) VALUE "30".
               03  FILLER                  PIC X(02) VALUE "31".
           02  WS-DATE-TABLE-RED           REDEFINES WS-DATE-TABLE.
               03  WSDT-OCCURS             OCCURS 12 TIMES.
                   04  WSDT-MONTH-DAYS     PIC 9(02).
           02  WS-FLEX-STOP-TABLE.
               03  WS-FLEX-STOP-VARS       OCCURS 100 TIMES.
002000             04  WS-FST-COMPANY      PIC 9(04) VALUE ZEROES.
002000             04  WS-FST-EMPLOYEE     PIC 9(09) VALUE ZEROES.
002000             04  WS-FST-PLAN-TYPE    PIC X(02) VALUE SPACES.
002000             04  WS-FST-PLAN-CODE    PIC X(04) VALUE SPACES.
002000             04  WS-FST-START-DATE   PIC 9(08) VALUE ZEROES.
                   04  WS-FST-STOP-DATE    PIC 9(08) VALUE ZEROES.
           02  BN100WS-SV-SP-TBL.
               03  BN100WS-SV-SP-OCC    OCCURS 25 TIMES.
                   04  BN100WS-SV-SPOUSE    PIC 9(04) VALUE ZEROES.
                   04  BN100WS-SV-SP-END-DT PIC 9(08) VALUE ZEROES.
                   04  BN100WS-SV-SP-PLN-TP PIC X(02) VALUE SPACES.
                   04  BN100WS-SV-SP-PLN-CD PIC X(04) VALUE SPACES.
           02  WS-DEP-TBL.
               03  WS-DEP-STOP-TBL          OCCURS 100 TIMES.
                   04  WS-DEP-NBR           LIKE DEPENDENT.
                   04  WS-DEP-ST-DT         LIKE DATE.
000100******************************************************************
000200*                         DATE FIELDS                            *
000300******************************************************************
001900     02  WS-EFFECT-DATE              PIC 9(08) VALUE ZEROES.
001900     02  WS-TERM-DATE                PIC 9(08) VALUE ZEROES.
001900     02  WS-CHANGE-DATE              PIC 9(08) VALUE ZEROES.
001900     02  WS-ADD-DATE                 PIC 9(08) VALUE ZEROES.
002000     02  WS-DATE                     PIC 9(08) VALUE ZEROES.
           02  WS-DATE-RED                 REDEFINES WS-DATE.
002000         03  WS-YEAR                 PIC 9(04).
002000         03  WS-MONTH                PIC 9(02).
002000         03  WS-DAY                  PIC 9(02).
           02  WS-EFD-START-DATE           PIC 9(08) VALUE ZEROES.
002000     02  WS-START-DATE               PIC 9(08) VALUE ZEROES.
002000     02  WS-STOP-DATE                PIC 9(08) VALUE ZEROES.
026200     02  WS-PAR-OCC-DATE             PIC 9(08) VALUE ZEROES.
026200     02  WS-PAR-TERM-DATE            PIC 9(08) VALUE ZEROES.
026300     02  WS-PAR-TERM-DATE-RED        REDEFINES WS-PAR-TERM-DATE.
026400        03  WS-PAR-TERM-CC           PIC 9(02).
026500        03  WS-DATE-YYMMDD           PIC 9(06).
026600        03  WS-DATE-YYMMDD-R         REDEFINES WS-DATE-YYMMDD.
026700            04  WS-PAR-TERM-YEAR     PIC 9(02).
026800            04  WS-PAR-TERM-MONTH    PIC 9(02).
026900            04  WS-PAR-TERM-DAY      PIC 9(02).
           02  WS-FLD-START-DATE           PIC 9(08) VALUE ZEROES.
           02  WS-PREV-CHANGE-DATE         PIC 9(08) VALUE ZEROES.
000500     02  WS-FLD-DATE                 PIC 9(08) VALUE ZEROS.
000600     02  WS-FLD-DATE-R               REDEFINES WS-FLD-DATE.
               03  WS-FLD-YEAR             PIC 9(04).
               03  WS-FLD-MMDD.
000900             04  WS-FLD-MONTHS       PIC 9(02).
001000             04  WS-FLD-DAYS         PIC 9(02).
000500     02  WS-SYS-DATE                 PIC 9(08) VALUE ZEROS.
000600     02  WS-SYS-DATE-R               REDEFINES WS-SYS-DATE.
               03  WS-SYS-YEAR             PIC 9(04).
               03  WS-SYS-MMDD.
000900             04  WS-SYS-MONTHS       PIC 9(02).
001000             04  WS-SYS-DAYS         PIC 9(02).
000500     02  WS-HDB-STOP-DATE            PIC 9(08) VALUE ZEROS.
J57497     02  WS-NEW-DATE                 PIC 9(08) VALUE ZEROES.
000100******************************************************************
000200*                        SAVE VARIABLES                          *
000300******************************************************************
170393     02  WS-PREV-START-DATE          PIC 9(08) VALUE ZEROES.
           02  WS-PGE-COMPANY              PIC 9(04) VALUE ZEROES.
           02  WS-PGE-EMPLOYEE             PIC 9(09) VALUE ZEROES.
           02  WS-PGE-GROUP-NAME           PIC X(10) VALUE SPACES.
           02  WS-SAVE-PAY-PLAN            PIC X(04) VALUE SPACES.
002000     02  WS-SAVE-COMPANY             PIC 9(04) VALUE ZEROES.
002000     02  WS-SAVE-EMPLOYEE            PIC 9(09) VALUE ZEROES.
002000     02  WS-SAVE-FLEX-PLAN           PIC X(04) VALUE SPACES.
002000     02  WS-SAVE-START-DATE          PIC 9(08) VALUE ZEROES.
002000     02  WS-SAVE-SEQ-NBR             PIC 9(04) VALUE ZEROES.
002000     02  WS-SV-START-DATE            PIC 9(08) VALUE ZEROES.
002000     02  WS-SAVE-EFFECT-DATE         PIC 9(08) VALUE ZEROES.
002000     02  WS-SAVE-CHANGE-TYPE         PIC X(02) VALUE SPACES.
002000     02  WS-SAVE-PLAN-TYPE           PIC X(02) VALUE SPACES.
002000     02  WS-SAVE-PLAN-CODE           PIC X(04) VALUE SPACES.
002000     02  WS-SAVE-PLAN-OPTION         PIC 9(02) VALUE ZEROES.
002000     02  WS-SAVE-FUNCTION-CODE       PIC X(02) VALUE SPACES.
002000     02  WS-SAVE-DIST-ST-DATE        PIC 9(08) VALUE ZEROES.
002000     02  WS-SAVE-DEPENDENT           PIC 9(04) VALUE ZEROES.
002000     02  WS-FLEX-TERM-DATE           PIC 9(08) VALUE ZEROES.
002000     02  WS-PROC-LEVEL               PIC X(05) VALUE SPACES.
002000     02  WS-DEPARTMENT               PIC X(05) VALUE SPACES.
001900     02  WS-NEW-EMP-CONT-TYPE        PIC X(01) VALUE SPACES.
001900     02  WS-NEW-CONT-TAX-STS         PIC X(01) VALUE SPACES.
002000     02  WS-NEW-MAX-MULTIPLE         PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-NEW-MIN-MULTIPLE         PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-NEW-MAX-COVER            PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-NEW-AMT-MAXIMUM          PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-NEW-PCT-MAXIMUM          PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-SWAP-COMPANY             PIC 9(04) VALUE ZEROES.
002000     02  WS-SWAP-EMPLOYEE            PIC 9(09) VALUE ZEROES.
002000     02  WS-SWAP-PLAN-TYPE           PIC X(02) VALUE SPACES.
002000     02  WS-SWAP-PLAN-CODE           PIC X(04) VALUE SPACES.
002000     02  WS-SWAP-START-DATE          PIC 9(08) VALUE ZEROES.
002000     02  WS-SWAP-STOP-DATE           PIC 9(08) VALUE ZEROES.
002000     02  WS-SAV-COMPANY              PIC 9(04) VALUE ZEROES.
002000     02  WS-SV-EMPLOYEE              PIC 9(09) VALUE ZEROES.
002000     02  WS-SV-EFFECT-DATE           PIC 9(08) VALUE ZEROES.
002000     02  WS-SV-CHANGE-TYPE           PIC X(02) VALUE SPACES.
002000     02  WS-SV-SEQ-NBR               PIC 9(04) VALUE ZEROES.
002000     02  WS-SV-TERM-DATE             PIC 9(08) VALUE ZEROES.
002000     02  WS-SAVE-ADD-DATE            PIC 9(08) VALUE ZEROES.
002000     02  WS-CHANGE-TYPE              PIC X(01) VALUE SPACES.
002000     02  WS-SV-PLAN-TYPE             PIC X(02) VALUE SPACES.
002000     02  WS-SV-PLAN-CODE             PIC X(04) VALUE SPACES.
           02  WS-SV-BENC-FOUND-SW         PIC 9(01) VALUE ZEROES.
J63718     02  WS-PREV-PLAN-CODE           PIC X(04) VALUE SPACES.
J63718     02  WS-PREV-PLAN-TYPE           PIC X(02) VALUE SPACES.
J63718     02  WS-PREV-FUNCTION-CODE       PIC X(02) VALUE SPACES.
J63718     02  WS-PREV-EMPLOYEE            PIC 9(09) VALUE ZEROES.
J66766     02  WS-PREV-PLN-DED-CODE        PIC X(04) VALUE SPACES.
J66766     02  WS-COMP-EDM-DED-CODE        PIC X(04) VALUE SPACES.
000100******************************************************************
000200*                          NUMERICS                              *
000300******************************************************************
001900     02  WS-EMPLOYEE                 PIC 9(09) VALUE ZEROES.
001900     02  WS-PROC-EMPLOYEE            PIC 9(09) VALUE ZEROES.
002000     02  WS-REMAINDER                PIC 9(04) VALUE ZEROES.
002000     02  WS-DUMMY                    PIC 9(04) VALUE ZEROES.
002000     02  WS-OLD-DIVI                 PIC 9(02) VALUE ZEROES.
002000     02  WS-NEW-DIVI                 PIC 9(02) VALUE ZEROES.
002000     02  WS-COMPANY                  PIC 9(04) VALUE ZEROES.
002000     02  WS-SALARY                   PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-NEW-COMP-DOLS            PIC S9(13)V99   COMP-3
                                                     VALUE ZEROES.
002000     02  WS-NBR-FTE                  PIC S9(01)V999999 COMP-3
                                                     VALUE ZEROES.
J46844     02  WS-ANNUAL-HOURS             LIKE ANNUAL-HOURS
                                                     VALUE ZEROES.
           02  WS-MAX-PERIODS              PIC 9(02) VALUE 24.
           02  WS-MAX-EFDS                 PIC 9(02) VALUE 24.
           02  WS-TOTAL-AVAIL              PIC S9(13)V99 COMP-3
                                                     VALUE ZEROES.
           02  WS-TOTAL-AVAIL-A            PIC $$$,$$$,$$$.99 
                                                     VALUE ZEROES.
027800     02  WS-OCC-MONTHS-EXT           PIC 9(02) VALUE ZEROES.
           02  WS-BWZ-EMP-NBR              PIC Z(09) VALUE ZEROES.
002000     02  WS-GWG-FLD-NBR              PIC 9(04) VALUE 1999.
           02  WS-CREDITS-SPENT            PIC S9(13)V99 COMP-3
                                                     VALUE ZEROES.
002000     02  WS-ERROR-NBR                PIC 9(03) VALUE ZEROES.
002000     02  WS-DEP-MAX                  PIC 9(03) VALUE 100.
012700     02  WS-AGE-YR-SAL               PIC S9(13)V99 VALUE ZEROES.
012700     02  WS-RTD-AGE-YR-SAL           PIC S9(13)V99 VALUE ZEROES.
012700     02  WS-CURR-AGE-YR-SAL          PIC S9(13)V99 VALUE ZEROES.
           02  WS-EMP-HEADING              PIC 9(09) VALUE ZEROES.
P44035     02  WS-EMP-DET-HEADING          PIC 9(09) VALUE ZEROES.
000100******************************************************************
000200*                             ALPHAS                             *
000300******************************************************************
002000     02  WS-FUNCTION-CODE            PIC X(01) VALUE SPACES.
001900     02  WS-FLEX-PLAN                PIC X(04) VALUE SPACES.
002000     02  WS-DED-TABLE                PIC X(02) VALUE SPACES.
002000     02  WS-FLP-GROUP-NAME           PIC X(10) VALUE SPACES.
002000     02  WS-OLD-VALUE                PIC X(20) VALUE SPACES.
002000     02  WS-NEW-VALUE                PIC X(30) VALUE SPACES.
002000     02  WS-ADD-DELETE               PIC X(01) VALUE SPACES.
002000     02  WS-ERR-VARS.
002000         03  WS-ERR-VAR1             PIC X(20) VALUE SPACES.
002000         03  WS-ERR-VAR2             PIC X(20) VALUE SPACES.
002000         03  WS-ERR-VAR3             PIC X(20) VALUE SPACES.
002000         03  WS-ERR-VAR4             PIC X(20) VALUE SPACES.
002000         03  WS-ERR-VAR5             PIC X(20) VALUE SPACES.
002000     02  WS-ERROR-CAT                PIC X(05) VALUE SPACES.
           02  WS-FLD-GROUP-NAME           PIC X(10) VALUE SPACES.
           02  WS-G3.
P43669         03  WS-G3-LINE OCCURS 99 TIMES.
                   04  WS-G3-EFFECT-MESSAGE    PIC X(14) VALUE SPACES.
                   04  WS-G3-EFFECT-DATE       PIC 9(08) VALUE ZEROS.
                   04  WS-G3-ACTION            PIC X(06) VALUE SPACES.
                   04  WS-G3-PAH-ACTION-CODE   PIC X(10) VALUE SPACES.
                   04  WS-G3-ADD-DEL-CHANGE    PIC X(01) VALUE SPACES.
                   04  WS-G3-GRP-OR-CHANGE-1   PIC X(11) VALUE SPACES.
                   04  WS-G3-GRP-OR-CHANGE-2   PIC X(11) VALUE SPACES.
                   04  WS-G3-GRP-OR-CHANGE-3   PIC X(11) VALUE SPACES.
                   04  WS-G3-GRP-OR-CHANGE-4   PIC X(11) VALUE SPACES.
                   04  WS-G3-GRP-OR-CHANGE-5   PIC X(11) VALUE SPACES.
                   04  WS-G3-GRP-OR-CHANGE-6   PIC X(11) VALUE SPACES.
ACS003     02  WS-WF1-PLAN-TYPE-SW     PIC 9(01)           VALUE ZEROES.
ACS003         88  A-WF1-PLAN-TYPE                         VALUE ZEROES.
ACS003         88  NOT-WF1-PLAN-TYPE                       VALUE 1.
ACS003     02  WS-WF2-PLAN-TYPE-SW     PIC 9(01)           VALUE ZEROES.
ACS003         88  A-WF2-PLAN-TYPE                         VALUE ZEROES.
ACS003         88  NOT-WF2-PLAN-TYPE                       VALUE 1.
