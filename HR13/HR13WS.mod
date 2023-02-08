******* HR13WS 10 <4003200520>
000100******************************************************************
000200*                             HR13WS                             *
000300******************************************************************
000400 01  HR13WS.
ACS004     02  WS-SMOKER-CHANGED-SW    PIC 9(01) VALUE ZEROS.
ACS004         88 SMOKER-CHANGED                 VALUE 0.     
ACS004         88 SMOKER-NOT-CHANGED             VALUE 1.
ACS004     02  WS-SMOKER-FLAG          PIC X(01) VALUE SPACES.
000500     02  HR13WS-J-CURRENT-DATE   PIC S9(13) COMP-3 VALUE ZEROES.
000600     02  HR13WS-J-EMD-BIRTHDATE  PIC S9(13) COMP-3 VALUE ZEROES.
000700     02  HR13WS-RECORD-ORIG      PIC X(01)  VALUE SPACES.
000800     02  HR13WS-COV-DEPENDENTS   PIC X(01)  VALUE SPACES.
000900     02  HR13WS-DEP-AGE-CALC     PIC 9(06)  VALUE ZEROES.
001000     02  HR13WS-DEP-AGE-R        REDEFINES HR13WS-DEP-AGE-CALC.
001100         03  HR13WS-DEP-AGE      PIC 9(02).
001200         03  FILLER              PIC 9(04). 
001300     02  HR13WS-STUDENT-AGE-CALC PIC 9(06) VALUE ZEROES.
001400     02  HR13WS-STUDENT-AGE-R   REDEFINES HR13WS-STUDENT-AGE-CALC.
001500         03  HR13WS-STUDENT-AGE  PIC 9(02).
001600         03  FILLER              PIC 9(04). 
001700     02  HR13WS-DEP-AGE-DATE     PIC 9(08) VALUE ZEROES.
001800     02  HR13WS-STUDENT-AGE-DATE PIC 9(08) VALUE ZEROES.
001900     02  HR13WS-PLAN-TYPE-TBL        PIC X(08) VALUE
002000         "HLDNELDL".      
002100     02  HR13WS-PLAN-TYPE           REDEFINES HR13WS-PLAN-TYPE-TBL
002200                                    PIC X(02) OCCURS 4 TIMES.
002300     02  HR13WS-STOP-DFLT-SW        PIC 9(01)    VALUE 0.
002400         88  STOP-DEFAULTED                      VALUE 1.
002500     02  HR13WS-DEL-HDB-SW          PIC 9(01)    VALUE 0.
002600         88  NOT-DELETING-HDB                    VALUE 0.
002700         88  DELETING-HDB                        VALUE 1.
002800     02  HR13WS-CHG-HDB-SW          PIC 9(01)    VALUE 0.
002900         88  NOT-CHANGING-HDB                    VALUE 0.
003000         88  CHANGING-HDB                        VALUE 1.
003100     02  WS-END-OF-FILE-SW       PIC 9(01) VALUE 0.
003200         88  END-OF-FILE         VALUE 1.
003300         88  NOT-END-OF-FILE     VALUE 0.
003400     02  WS-PLAN-TYPE-SW         PIC 9(01) VALUE 0.
003500         88  PLAN-TYPE-FOUND     VALUE 1.
003600         88  PLAN-TYPE-NOTFOUND  VALUE 0.
003700     02  HR13WS-NBR-LINES        PIC 9(02)        VALUE 10.
           02  HR13WS-PT-STUDENTS      PIC X(01)        VALUE SPACES.
           02  HR13WS-TERM-OPT         LIKE TERM-OPT    VALUE SPACES.
           02  HR13WS-DEP-END-DATE     LIKE STOP-DATE   VALUE ZEROES.
003800******************************************************************
003900*           HR13WS SAVE FIELDS                                   *
004000******************************************************************
004100 01  SAVE-FIELD-WS.
004200     02  HR13WS-EMD-CUR-AGE      PIC S9(02) COMP-3 VALUE ZEROES.
           02  HR13WS-SAVED-DATA.
               03  HR13WS-SAVED-DATA-GRP       OCCURS 10 TIMES.
                   04  HR13WS-SAVED-HILITE     PIC X(01)   VALUE SPACES.
                   04  HR13WS-SAVED-PLAN-TYPE  PIC X(02)   VALUE SPACES.
                   04  HR13WS-SAVED-PLAN-CODE  PIC X(04)   VALUE SPACES.
                   04  HR13WS-SAVED-DEPENDENT  PIC 9(04)   VALUE ZEROES.
                   04  HR13WS-SV-EMP-START-DT  PIC 9(08)   VALUE ZEROES.
                   04  HR13WS-SAVED-START-DATE PIC 9(08)   VALUE ZEROES.
004300
004400
011000     02  HR13WS-DUMMY            PIC 9(13)        COMP-3
011100                                                  VALUE ZEROES.
011200     02  HR13WS-REMAINDER        PIC 9(13)V9(02)  COMP-3
011300                                                  VALUE ZEROES.
