******* BNPE70WS 1 <4190539282>
      ******************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      ******************************************************************
      *  ADDED BNPEWS-ERR-VAR4 AND WS-BPC-FOUND                        *
000100******************************************************************
000200*                           BNPE70WS                             *
000300******************************************************************
000400 01  BNPE70WS.
000500     02  BNPEWS-COMPANY          PIC 9(04)        VALUE ZEROES.
000600     02  BNPEWS-PLAN-TYPE        PIC X(02)        VALUE SPACES.
000700     02  BNPEWS-PLAN-CODE        PIC X(04)        VALUE SPACES.
000800     02  BNPEWS-EMPLOYEE         PIC 9(09)        VALUE ZEROES.
000900     02  BNPEWS-START-DATE       PIC 9(08)        VALUE ZEROES.
001000     02  BNPEWS-COVER-TYPE       PIC X(01)        VALUE SPACES.
001100     02  BNPEWS-ELIGIBLE-SW      PIC 9(01)        VALUE ZEROES.
001200         88  NOT-ELIGIBLE                         VALUE ZERO.
001300         88  ELIGIBLE                             VALUE 1.
001400     02  BNPEWS-EMP-POSTAL-CODE.
001500         03  BNPEWS-EMP-POST-CODE     PIC X(05)   VALUE SPACES.
001600         03  BNPEWS-EMP-EXT-POST-CODE PIC X(05)   VALUE SPACES.
001700     02  BNPEWS-BPC-POSTAL-CODE.
001800         03  BNPEWS-BPC-POST-CODE     PIC X(05)   VALUE SPACES.
001900         03  BNPEWS-BPC-EXT-POST-CODE PIC X(05)   VALUE SPACES.
002000     02  BNPEWS-ERROR-NBR        PIC 9(03)        VALUE ZEROES.
002100         88  NO-BNPE-ERROR-FOUND                 VALUE ZERO.
002200         88  BNPE-ERROR-FOUND                    VALUE 1 THRU 999.
002300     02  BNPEWS-BWZ-EMP-NBR      PIC Z(09)        VALUE ZEROES.
002400     02  BNPEWS-ERR-VAR1         PIC X(30)        VALUE SPACES.
002500     02  BNPEWS-ERR-VAR2         PIC X(30)        VALUE SPACES.
002600     02  BNPEWS-ERR-VAR3         PIC X(30)        VALUE SPACES.
002600     02  BNPEWS-ERR-VAR4         PIC X(30)        VALUE SPACES.
           02  WS-BPC-FOUND            PIC X            VALUE SPACES.
