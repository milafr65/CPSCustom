******* BN72WS 3 <1338413387>
000100******************************************************************
      * 11/2011 SDB ADDED CUSTOM BACK IN ACS001     
      *================================================================
000200*                            BN72WS                              *
000300******************************************************************
000400 01  BN72WS.
000500     02  BN72WS-DEPEND-SW             PIC 9(01) VALUE ZEROS.
000600         88 BN72WS-DEPENDENTS-EXIST             VALUE 1.
000700         88 BN72WS-NO-DEPENDENTS                VALUE 0.
P33813     02  BN72WS-HIGH-VAL-SW           PIC 9(01) VALUE ZEROS.
P33813         88 BN72WS-MOVE-NO-HIGH-VAL             VALUE 0.
P33813         88 BN72WS-MOVE-HIGH-VAL                VALUE 1.
           02  WS-GO-TO-SW                  PIC X VALUE "N".
ACS002*    02  WS-ESMS-ESMX-DEL-SW     PIC 9(01)           VALUE ZEROES.
ACS002*        88  ESMS-ESMX-NOT-DELETED                   VALUE ZEROES.
ACS002*        88  ESMS-ESMX-DELETED                       VALUE 1.
ACS002     02  WS-ESMS-DEL-SW     PIC 9(01)                VALUE ZEROES.
ACS002         88  ESMS-NOT-DELETED                        VALUE ZEROES.
ACS002         88  ESMS-DELETED                            VALUE 1.

