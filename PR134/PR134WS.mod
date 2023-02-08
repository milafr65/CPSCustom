******* PR134WS 9.1.3 <578098533>
000100******************************************************************
000200*                                PR134WS                         *
000300******************************************************************
      *  JT#      TAG      DESCRIPTION                                 *
      * -------   ------   ------------------------------------------  *
      * 1035273 | 035273 | ADDED VALIDATION WHEN ENTERING EMPL GRP &   *
      *         |        | EMPLOYEE THAT HAVE THE SAME VALUES          *
      * -------   ------   ------------------------------------------  *
000400 01  PR134WS.
000500     02  WS-BT-TOT-HOURS         PIC S9(07)V99   VALUE ZEROS.
000600     02  WS-BT-TOT-RATE          PIC S9(10)V9999 VALUE ZEROS.
000700     02  WS-ET-TOT-HOURS         PIC S9(07)V99   VALUE ZEROS.
000800     02  WS-ET-TOT-RATE          PIC S9(09)V9999 VALUE ZEROS.
000900     02  WS-GT-TOT-HOURS         PIC S9(07)V99   VALUE ZEROS.
001000     02  WS-GT-TOT-RATE          PIC S9(09)V9999 VALUE ZEROS.
001100     02  WS-TE-TOT-EMPLOYEES     PIC 9(09)       VALUE ZEROS.
001200     02  WS-P-TIME-GRP           PIC X(01)       VALUE SPACES.
001300     02  WS-ERROR-FOUND          PIC X(01)       VALUE SPACES.
P08896     02  WS-ERROR-CAT            PIC X(05)       VALUE SPACES.
P08896     02  WS-ERROR-NBR            PIC 9(03)       VALUE ZEROES.
           02  WS-ERR-VAR1             PIC X(20)       VALUE SPACES.
001400
001500     02  WS-RESTART-INFO.
001600         03  WS-RESTART-PHASE    PIC 9(01)       VALUE ZEROS.
001700         03  WS-RESTART-RECORD-COUNT
001800                                 PIC 9(09)       VALUE ZEROS.
J65499         03  WS-RESTART-WF-NAME  LIKE FILENAME   VALUE SPACES.
002000
002100     02  WS-ACTUAL-HOURS         PIC S9(07)V99   VALUE ZEROS.
002200     02  WS-ACTUAL-AMOUNT        PIC S9(09)V9999 VALUE ZEROS.
002300     02  WS-RECORD-COUNT         PIC 9(09)       VALUE ZEROS.
002400     02  WS-EMPLOYEE             PIC 9(09)       VALUE ZEROS.
002500     02  WS-GROUP-QUALIFY-SW     PIC 9(01)       VALUE ZEROS.
002600         88  GROUP-QUALIFIED                     VALUE 1.
002700         88  NOT-GROUP-QUALIFIED                 VALUE 0.
002800     02  WS-STM-QUALIFY-SW       PIC 9(01)       VALUE ZEROS.
002900         88  STM-QUALIFIED                       VALUE 1.
003000         88  NOT-STM-QUALIFIED                   VALUE 0.
003100     02  WS-PAYCODE-DONE-SW      PIC 9(01)       VALUE ZEROS.
003200         88 PAYCODE-DONE                         VALUE 1.
003300         88 PAYCODE-NOT-DONE                     VALUE 0.
003400     02  WS-STD-EXISTS-SW        PIC 9(01)       VALUE ZEROS.
003500         88 STD-EXISTS                           VALUE 1.
003600         88 STD-DOES-NOT-EXIST                   VALUE 0.
003700     02  WS-TG-EXISTS-SW         PIC 9(01)       VALUE ZEROS.
003800         88 TG-EXISTS                            VALUE 1.
003900         88 TG-DOES-NOT-EXIST                    VALUE 0.
004000     02  WS-EMPLOYEE-FLAG        PIC 9(01)       VALUE ZEROS.
004100         88  WS-EMPLOYEE-NOTFOUND                VALUE ZEROS.
004200         88  WS-EMPLOYEE-FOUND                   VALUE 1.
           02  WS-EMPLOYEE-SELECT-FLG  PIC 9(01)       VALUE ZEROS.
               88  WS-NO-EMPLOYEE-SELECT               VALUE ZEROS.
               88  WS-EMPLOYEE-SELECT                  VALUE 1.
           02  WS-GROUP-SELECT-FLG     PIC 9(01)       VALUE ZEROS.
               88  WS-NO-GROUP-SELECT                  VALUE ZEROS.
               88  WS-GROUP-SELECT                     VALUE 1.
004300     02  WS-STD-TR-WITHIN-TG     PIC 9(01)       VALUE ZEROS.
004400         88  NO-STD-TR-WITHIN-TG                 VALUE ZEROS.
004500         88  STD-TR-WITHIN-TG                    VALUE 1.
004600     02  WS-PC-ONLY-FOR-TG       PIC 9(01)       VALUE ZEROS.
004700         88  PC-ONLY-FOR-STD                     VALUE ZEROS.
004800         88  PC-ONLY-FOR-TG                      VALUE 1.
004900     02  WS-END-OF-FILE-SW       PIC 9(01)       VALUE ZEROS.
005000         88 END-OF-FILE                          VALUE 1.
005100         88 NOT-END-OF-FILE                      VALUE 0.
005200     02  WS-BST-ELEMENT-FLAG     PIC 9(01)       VALUE ZEROS.
005300         88  WS-BST-ELEMENT-NOTFOUND             VALUE ZEROS.
005400         88  WS-BST-ELEMENT-FOUND                VALUE 1.
005500     02  WS-EXISTING-BATCH-SW    PIC 9(01)       VALUE ZEROS.
005600         88 EXISTING-BATCH                       VALUE 1.
005700     02  WS-TRD-CREATED-SW       PIC 9(01)       VALUE ZEROS.
005800         88 TRD-CREATED                          VALUE 1.
005900         88 TRD-NOT-CREATED                      VALUE 0.
006000     02  WS-PR134-ERROR-SW       PIC 9(01)       VALUE ZEROS.
006100         88 PR134-ERROR                          VALUE 1.
006200
006300     02  WS-BST-TABLE.
006400         03  WS-BST-TABLE-OCC        OCCURS 200 TIMES.
006500             04  WS-BST-PAY-CODE     PIC X(04).
006600             04  WS-BST-TOT-HOURS    PIC S9(07)V99   COMP-3.
006700             04  WS-BST-TOT-RATE     PIC S9(10)V9999 COMP-3.
006800
006900     02  WS-EMPLOYEE-TR-COUNT    PIC 9(04) VALUE ZEROS.
007000
J65499     02  WS-WORKFILE-NAME    LIKE FILENAME VALUE SPACES.
J65499     02  WS-ERRORFILE-NAME   LIKE FILENAME VALUE SPACES.
007300     02  WS-COMMENTS-SW          PIC 9(01)       VALUE ZEROS.
007400         88 COMMENTS-PRINTED                     VALUE 1.
007500         88 NO-COMMENTS-PRINTED                  VALUE 0.
007600     02  WS-EMP-TOTAL-SW         PIC 9(01)       VALUE ZEROS.
007700         88 EMP-TOTAL-PRINTED                    VALUE 1.
007800         88 NO-EMP-TOTAL-PRINTED                 VALUE 0.
007600     02  WS-GROUP-ACCESS-SW      PIC 9(01)       VALUE ZEROS.
007700         88 GROUP-ACCESS                         VALUE 1.
007800         88 NOT-GROUP-ACCESS                     VALUE 0.
007900     02  WS-SAVE-SELECTION       PIC X(01)       VALUE SPACES.
P40205     02  WS-EMP-TOT-ERRORS       PIC 9(09)       VALUE ZEROS.
P48725     02  WS-SALARY-CLASS         PIC X(1)        VALUE SPACES.

P45097     02  WS-LP-EMP-REC-OK        PIC 9(09)       VALUE ZEROS.
P45097     02  WS-LP-EMP-REC-ERR       PIC 9(09)       VALUE ZEROS.
P45097     02  WS-LP-BATCH-REC-OK      PIC 9(09)       VALUE ZEROS.
P45097     02  WS-LP-BATCH-REC-ERR     PIC 9(09)       VALUE ZEROS.

035273     02  PR134WS-TABLES.
035273         05  PR134WS-PRM-EMPLOYEE-TBL.
035273             10  PR134WS-PRM-EMPLOYEE-TABLE  OCCURS 15 TIMES.
035273                 15  PR134WS-PRM-EMPLOYEE    LIKE EMPLOYEE
035273                                             VALUE ZEROES.
035273         05  PR134WS-PRM-EMPL-GRP-TBL.
035273             10  PR134WS-PRM-EMPL-GRP-TABLE  OCCURS 10 TIMES.
035273                 15  PR134WS-PRM-GROUP-NAME  LIKE GROUP-NAME
035273                                             VALUE SPACES.
035273     02  PR134WS-NUMERIC-EDITED.
035273         05  PR134WS-MSG-EMPLOYEE            PIC ZZZZZZZZ9.

MG0325     02  WS-BYPASS-CG-SW         PIC 9(01)       VALUE ZEROS.
MG0325         88 BYPASS-CG                            VALUE 1.
MG0325         88 DO-NOT-BYPASS-CG                     VALUE 0.
