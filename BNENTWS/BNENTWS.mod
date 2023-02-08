******* BNENTWS 9.1.26 <2659845712>
*****************************************************************
      *               M O D I F I C A T I O N   L O G:                 *
      ******************************************************************
      *  MODIFIED BY ANALYSTS INTERNATIONAL,MN                         *
AI0095******************************************************************
AI0095*  AI0095  03/24/03  RETROACTIVE BENEFIT CHANGE MODIFICATION     *
      *  SEE ALSO TAGS RAY AND GW                                      *
000100******************************************************************
000200*                             BNBENWS                            *
000300******************************************************************
      *                                                                *
      *  JT#      TAG      DESCRIPTION                                 *
      *  ------   ------   ------------------------------------------  *
      *  866130 | J66130 | FIX FOR SHOWING ERROR MESSAGE " ONLY TWO    *
      *         |        | DECIMAL PLACES ALLOWED                      *
      *  ------   ------   ------------------------------------------  *
      *  992654 | J92654 | REVISED VARIABLE DEFINITION FOR             *
      *         |        | BNBEN-TEMP-NBR.                             *
      *  ------   ------   ------------------------------------------  *
      * 1117816 | 117816 | ADDED A NON FLEX VARIABLE TO BE USED IN A   *
      *         |        | CONDITION THAT INITIALIZES BNEDWS-MAGIC-SW  *
      *  ------   ------   ------------------------------------------  *
      ******************************************************************
000700 01  BNBENWS-STORAGE.
           02  BNBEN-PROCESSED-DATE            PIC 9(08) VALUE ZERO. 
000700     02  BNBEN-SCR-FIELDS.
               03  BNBEN-FC                    PIC X(01)  VALUE SPACES.
               03  BNBEN-FC-FN                 PIC 9(04)  VALUE ZEROES.
               03  BNBEN-COMPANY               PIC 9(04)  VALUE ZEROES.
               03  BNBEN-COMPANY-FN            PIC 9(04)  VALUE ZEROES.
               03  BNBEN-ENROLLMENT-DATE       PIC 9(08)  VALUE ZEROES.
               03  BNBEN-ENROLLMENT-DATE-FN    PIC 9(04)  VALUE ZEROES.
               03  BNBEN-NBR-LINES             PIC 9(02)  VALUE ZEROES.
               03  BNBEN-SKIP-GROUP-N-ZIP      PIC X(01)  VALUE SPACES.
               03  BNBEN-LP-WARN-I1            PIC 9(02)  VALUE ZEROES.
               03  BNBEN-DED-OVER-XMIT         PIC 9(01)  VALUE ZEROES.
               03  BNBEN-YTD-XMIT              PIC 9(01)  VALUE ZEROES.
J17193         03  BNBEN-WEB-UPDATE            PIC X(01)  VALUE SPACES.
J17193         03  BNBEN-RULE-TYPE             PIC X(01)  VALUE SPACES.
J17193         03  BNBEN-FAMILY-STATUS         PIC X(10)  VALUE SPACES.
J17193         03  BNBEN-NEW-DATE              PIC 9(08)  VALUE ZEROES.
J17193         03  BNBEN-UPDATE-ACA            PIC X(01)  VALUE SPACES.
J17193         03  BNBEN-ACA-INIT-ENROLL       PIC X(01)  VALUE SPACES.
P66372     02  BNBEN-BENSET1           PIC X(10)       VALUE "BENSET1".
P53493     02  BNBEN-DETAIL-GROUP.
                   04  BNBEN-BENEFIT-DETAIL    OCCURS 30 TIMES.
000300******************************************************************
      *                FIELDS ENTERED FROM DETAIL SCREEN               *
000300******************************************************************
                       05  BNBEN-LINE-FC       PIC X(01)  VALUE SPACES.
                       05  BNBEN-LINE-FC-FN    PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-EMPLOYEE      PIC 9(09)  VALUE ZEROES.
                       05  BNBEN-EMPLOYEE-FN   PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-PLAN-TYPE     PIC X(02)  VALUE SPACES.
                       05  BNBEN-PLAN-TYPE-FN  PIC 9(04)  VALUE ZEROES.
183800                 05  BNBEN-PLAN-CODE     PIC X(04)  VALUE SPACES.
                       05  BNBEN-PLAN-CODE-FN  PIC 9(04)  VALUE ZEROES.
184000                 05  BNBEN-COVER-OPT     PIC 9(02)  VALUE ZEROES.
184000                 05  BNBEN-COVER-OPT-FN  PIC 9(04)  VALUE ZEROES.
184300                 05  BNBEN-MULT-SALARY   PIC S9(03)V999
                                                          VALUE ZEROES.
                       05  BNBEN-MULT-SALARY-FN
                                               PIC 9(04)  VALUE ZEROES.
J67795                 05  BNBEN-COVER-AMT     PIC S9(13)V9(4) COMP-3
                                                          VALUE ZEROES.
                       05  BNBEN-COVER-AMT-FN  PIC 9(04)  VALUE ZEROES.
J67795                 05  BNBEN-PAY-RATE      PIC S9(13)V9(4)
                                                          VALUE ZEROES.
                       05  BNBEN-PAY-RATE-FN   PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-PCT-AMT-FLAG  PIC X(01)  VALUE SPACES.
                       05  BNBEN-PCT-AMT-FLAG-FN
                                               PIC 9(04)  VALUE ZEROES.
183700                 05  BNBEN-START-DATE    PIC 9(08)  VALUE ZEROES.
                       05  BNBEN-START-DATE-FN PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-PRE-AFT-FLAG  PIC X(01)  VALUE SPACES.
                       05  BNBEN-PRE-AFT-FLAG-FN
                                               PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-PEND-EVIDENCE PIC X(01)  VALUE SPACES.
                       05  BNBEN-SMOKER-FLAG   PIC X(01)  VALUE SPACES.
                       05  BNBEN-SMOKER-FLAG-FN
                                               PIC 9(04)  VALUE ZEROES.
J67795                 05  BNBEN-EMP-PRE-CONT  PIC S9(13)V9(4)
                                                          VALUE ZEROES.
                       05  BNBEN-EMP-PRE-CONT-FN
                                               PIC 9(04)  VALUE ZEROES.
J67795                 05  BNBEN-EMP-AFT-CONT  PIC S9(13)V9(4)
                                                          VALUE ZEROES.
                       05  BNBEN-EMP-AFT-CONT-FN
                                               PIC 9(04)  VALUE ZEROES.
183900                 05  BNBEN-STOP-DATE     PIC 9(08)  VALUE ZEROES.
                       05  BNBEN-STOP-DATE-FN  PIC 9(04)  VALUE ZEROES.
183900                 05  BNBEN-DED-START-DATE LIKE START-DATE
                                                          VALUE ZEROES.
                       05  BNBEN-DED-START-DATE-FN PIC 9(04)
                                                          VALUE ZEROES.
183900                 05  BNBEN-DED-STOP-DATE  LIKE STOP-DATE
                                                          VALUE ZEROES.
                       05  BNBEN-DED-STOP-DATE-FN  PIC 9(04)
                                                          VALUE ZEROES.
P85709                 05  BNBEN-DED-OVSTOP       LIKE STOP-DATE
P85709                                                    VALUE ZEROES.
                       05  BNBEN-PAY-FRQ-ADD-ERR PIC X(01) VALUE SPACES.
                       05  BNBEN-CREATE-TRANS    PIC X(01) VALUE SPACES.
                       05  BNBEN-CREATE-TRANS-FN PIC 9(04) VALUE ZEROES.
                       05  BNBEN-REASON          PIC X(10) VALUE SPACES.
                       05  BNBEN-REASON-FN       PIC 9(04) VALUE ZEROES.
                       05  BNBEN-MEMBER-ID       PIC 9(01) VALUE ZEROES.
                       05  BNBEN-MEMBER-ID-FN    PIC 9(04) VALUE ZEROES.
                       05  BNBEN-BTE-EMP-GRP   LIKE GROUP-NAME
                                                          VALUE SPACES.
                       05  BNBEN-BTE-POSITION  LIKE POSITION
                                                          VALUE SPACES.
                       05  BNBEN-BTE-ALLOC-PCT LIKE PERCENT
                                                          VALUE ZEROES.
J73344                 05  BNBEN-PRE-STOP-DATE   PIC 9(08) VALUE ZEROES.
J73344                 05  BNBEN-PRE-DED-START-DATE    
J73344                                           PIC 9(08) VALUE ZEROES.
J73344                 05  BNBEN-PRE-DED-STOP-DATE     
J73344                                           PIC 9(08) VALUE ZEROES.
000300******************************************************************
      *                SCREEN FIELDS END                               *
000300******************************************************************
000300******************************************************************
      *                OUTPUT FIELDS RECEIVED FROM APIs NOT ON SCREEN  *
000300******************************************************************
000300******************************************************************
      *                    RECEIVED FROM ELIGIBILITY API               *
000300******************************************************************
184100                 05  BNBEN-ELIG-UPD-DT   PIC 9(08)  VALUE ZEROES.
184100                 05  BNBEN-ELIG-GROUP    PIC X(10)  VALUE SPACES.
000300******************************************************************
      *                    RECEIVED FROM COVERAGE API                  *
000300******************************************************************
                       05  BNBEN-DEP-COVER-AMT PIC S9(13)V99 COMP-3
                                                          VALUE ZEROES.
184100                 05  BNBEN-COV-UPD-DT    PIC 9(08)  VALUE ZEROES.
184100                 05  BNBEN-COV-GROUP     PIC X(10)  VALUE SPACES.
                       05  BNBEN-COV-OVER-FLG  PIC X(01)  VALUE SPACES.
183700                 05  BNBEN-COV-SAL-DATE  PIC 9(08)  VALUE ZEROES.
183700                 05  BNBEN-COV-AGE-DATE  PIC 9(08)  VALUE ZEROES.
000300******************************************************************
      *                    RECEIVED FROM CONTRIBUTION API              *
000300******************************************************************
J67795                 05  BNBEN-COMP-CONT     PIC S9(13)V9(4)
                                                          VALUE ZEROES.
002000                 05  BNBEN-PCT-MATCHED   PIC S9(03)V9(02) COMP-3
                                                          VALUE ZEROES.
184300                 05  BNBEN-MAX-MATCH     PIC S9(09)V99
                                                          VALUE ZEROES.
                       05  BNBEN-PRE-MATCH-OPTION
                                               PIC X(01)  VALUE SPACES.
                       05  BNBEN-PRE-MATCH-CALC
                                               PIC X(01)  VALUE SPACES.
184100                 05  BNBEN-PREM-UPD-DT   PIC 9(08)  VALUE ZEROES.
184100                 05  BNBEN-PREM-GROUP    PIC X(10)  VALUE SPACES.
                       05  BNBEN-ZERO-PREMIUM-SW
                                               PIC 9(01)  VALUE ZEROES.
                       05  BNBEN-NEG-PREMIUM-SW
                                               PIC 9(01)  VALUE ZEROES.
                       05  BNBEN-POS-PREMIUM-SW
                                               PIC 9(01)  VALUE ZEROES.
010100                 05  BNBEN-BNA-START-DATE
                                               PIC 9(08)  VALUE ZEROES.
010100                 05  BNBEN-BNA-GROUP-NAME
                                               PIC X(10)  VALUE ZEROES.
183700                 05  BNBEN-CNT-SAL-DATE  PIC 9(08)  VALUE ZEROES.
183700                 05  BNBEN-CNT-AGE-DATE  PIC 9(08)  VALUE ZEROES.
183700                 05  BNBEN-CNT-SERV-DATE PIC 9(08)  VALUE ZEROES.
183700                 05  BNBEN-COMP-CONT-SW  PIC X(01)  VALUE SPACES.
183700                 05  BNBEN-CREATION-DATE LIKE DATE.
183700                 05  BNBEN-UPD-DATE      LIKE UPD-DATE.
183700                 05  BNBEN-TIME-STAMP    LIKE TIME-STAMP.
183700                 05  BNBEN-USER-ID       LIKE USER.
000300******************************************************************
      *                    FOLLOWING VARS SET IN ENTRY API             *
000300******************************************************************
183700                 05  BNBEN-EFD-START-DT  PIC 9(08)  VALUE ZEROES.
183700                 05  BNBEN-EFD-STOP-DT   PIC 9(08)  VALUE ZEROES.
183700                 05  BNBEN-EFD-GROUP-NM  PIC X(10)  VALUE SPACES.
J67795                 05  BNBEN-EFD-PT-AMT    PIC 9(13)V9(4)
                                                          VALUE ZEROES.
183700                 05  BNBEN-PT-START-DATE PIC 9(08)  VALUE ZEROES.
J67795                 05  BNBEN-CMP-FLX-CONT  PIC S9(13)V9(4)
                                                          VALUE ZEROES.
                       05  BNBEN-FLEX-FLAG     PIC X(01)  VALUE SPACES.
                       05  BNBEN-SPEND-ONLY    PIC X(01)  VALUE SPACES.
                       05  BNBEN-STM-SEQ-NBR   PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-TAKEN-FLAG    PIC X(01)  VALUE SPACES.
                       05  BNBEN-CHANGE-PREMIUM-SW
                                               PIC 9(01)  VALUE ZEROES.
                       05  BNBEN-COMP-CHANGED-SW
                                               PIC 9(01)  VALUE ZEROES.
                       05  BNBEN-CMP-CONT-CHG-SW
                                               PIC 9(01)  VALUE ZEROES.
                       05  BNBEN-MID-YEAR-SW   PIC 9(01)  VALUE ZEROES.
                       05  BNBEN-SVD-PRE-DATE  PIC 9(08)  VALUE ZEROES.
                       05  BNBEN-TA-UPD-FLAG   PIC X(01)  VALUE SPACES.

J67795                 05  BNBEN-PRE-EMP-CONT  PIC S9(13)V9(4) COMP-3
                                                          VALUE ZEROES.
J67795                 05  BNBEN-YTD-CONT      PIC S9(13)V9(4)
                                                          VALUE ZEROES.
J67795                 05  BNBEN-EMP-YTD-CONT  PIC S9(13)V9(4)
                                                          VALUE ZEROES.
J67795                 05  BNBEN-YTD-FLEX-CONT PIC S9(13)V9(4)
                                                          VALUE ZEROES.
                       05  BNBEN-SPOUSE-CVG    PIC S9(13)V99 COMP-3
                                                          VALUE ZEROES.
J67795                 05  BNBEN-ANNUAL-COST   PIC S9(06)V9(4) COMP-3
                                                          VALUE ZEROES.
                       05  BNBEN-ADDED-BENEFITS-SW
                                               PIC X(01)  VALUE SPACES.
                       05  BNBEN-FUTURE-ERROR-SW
                                               PIC X(01)  VALUE SPACES.
                       05  BNBEN-SKIP-ELIGIBILITY
                                               PIC X(01)  VALUE SPACES.
011200                 05  BNBEN-BATCH-FC      PIC X(02)  VALUE SPACES.
                       05  BNBEN-SV-SP-TBL.
                           06  BNBEN-SV-SP-OCC OCCURS 25 TIMES.
                               07  BNBEN-SV-SP-SPOUSE
                                               PIC 9(04)  VALUE ZEROES.
                               07  BNBEN-SV-SP-END-DT
                                               PIC 9(08)  VALUE ZEROES.
                               07  BNBEN-SV-SP-PLN-TP
                                               PIC X(02)  VALUE SPACES.
                               07  BNBEN-SV-SP-PLN-CD
                                               PIC X(04)  VALUE SPACES.
                       05  BNBEN-TRIGGER-ENABLED-SW
                                               PIC 9(01)  VALUE ZEROES.
J17193                 05  BNBEN-ENROLLMENT-SW PIC 9(01)  VALUE ZEROES.
J17193                 05  BNBEN-FROM-DATE     PIC 9(08)  VALUE ZEROES.
               03  BNBEN-C5-MAX-TABLE.
                   04  BNBEN-C5-TABLE          OCCURS 24 TIMES.
                       05  BNBEN-C5-PLAN-CODE  PIC X(04)  VALUE SPACES.
                       05  BNBEN-C5-FLEX-PLAN  PIC X(04)  VALUE SPACES.
                       05  BNBEN-C5-ENTRY-MMDD PIC 9(04)  VALUE ZEROES.
                       05  BNBEN-C5-SPEND-ONLY PIC X(01)  VALUE SPACES.
                       05  BNBEN-C5-PLAN-MAX   PIC S9(13)V99 COMP-3
                                                          VALUE ZEROES.
                       05  BNBEN-C5-START-DATE PIC 9(08)  VALUE ZEROES.
                       05  BNBEN-C5-START      PIC 9(08)  VALUE ZEROES.
                       05  BNBEN-C5-STOP       PIC 9(08)  VALUE ZEROES.

           02  BNBEN-LOCAL-WS.
               03  BNBEN-C5-ANNUAL-AMT         PIC S9(13)V99 
                                                          VALUE ZEROES.
               03  BNBEN-USE-NAVIGATE-SW       PIC X(01)  VALUE SPACES.
                   88  USE-NAVIGATE                       VALUE "Y".
               03  BNBEN-NAV-TABLE OCCURS 30 TIMES.
                   04  BNBEN-NBR               PIC 9(02)  VALUE ZEROES.
               03  BNBEN-FIELD-NBR             PIC 9(02)  VALUE ZEROES.
               03  BNBEN-RS-MAX-OC             PIC 9(02)  VALUE 13.
027800         03  BNBEN-START-DATE-CALC       PIC 9(08)  VALUE ZEROES.
004500         03  BNBEN-START-DATE-CALC-RED   REDEFINES
                                               BNBEN-START-DATE-CALC.
004600             04  BNBEN-START-YYYY        PIC 9(04).
004700             04  BNBEN-START-DATE-MMDD   PIC 9(04).
               03  BNBEN-MAX-SALARY            PIC S9(09)V9999 COMP-3
                                                          VALUE ZEROES.
               03  BNBEN-SALARY                PIC S9(09)V9999 COMP-3
                                                          VALUE ZEROES.
               03  BNBEN-COVERAGE              PIC S9(13)V99 
                                                          VALUE ZEROES.
               03  BNBEN-COV-TEMP              PIC S9(09)V99 COMP-3
                                                          VALUE ZEROES.
000500         03  BNBEN-HOURLY-RATE           PIC 9(05)V9(03) COMP-3 
000600                                                    VALUE ZEROES.
000700         03  BNBEN-PLAN-HOURLY-RATE      PIC 9(05)V9(03) COMP-3 
000800                                                    VALUE ZEROES.
000900         03  BNBEN-BEN-HOURLY-RATE       PIC 9(05)V9(03) COMP-3
001000                                                    VALUE ZEROES.
               03  BNBEN-QUOTIENT              PIC 9(09)V9(09) 
                                                          VALUE ZEROES.
000600         03  BNBEN-QUOTIENT-R            REDEFINES BNBEN-QUOTIENT.
000700             04  BNBEN-WHOLE             PIC 9(09).
000800             04  BNBEN-REMAINDER         PIC 9(09).
003300         03  BNBEN-NODECIMAL-AMT         PIC S9(13) COMP-3  
                                                          VALUE ZEROES.
003900         03  BNBEN-FLD-TO-ROUND          PIC S9(08)V999 COMP-3
004000                                                    VALUE ZEROES.
               03  BNBEN-DIFF                  PIC S9(08)V999 COMP-3
004000                                                    VALUE ZEROES.
               03  BNBEN-MID                   PIC S9(08)V999 COMP-3
004000                                                    VALUE ZEROES.
               03  BNBEN-BALANCED-DED          PIC X(01)  VALUE SPACES.
               03  BNBEN-FIRST-XMIT            PIC X(01)  VALUE SPACES.
               03  BNBEN-CREDITS-AVAILABLE     OCCURS 24 TIMES.
                   05 BNBEN-CREDITS-AVAIL      PIC 9(13)V99 COMP-3
                                                          VALUE ZEROES.
                   05 BNBEN-PRE-CREDITS-AVAIL  PIC 9(13)V99 COMP-3
                                                          VALUE ZEROES.
                   05 BNBEN-CREDITS-SPENT      PIC 9(13)V99 COMP-3
                                                          VALUE ZEROES.
                   05 BNBEN-PRE-CREDITS-SPENT  PIC 9(13)V99 COMP-3
                                                          VALUE ZEROES.
               03  BNBEN-USER-XFER             PIC X(01)  VALUE SPACES.
               03  BNBEN-DEPEND-SW             PIC 9(01)  VALUE ZEROES.
                   88 BNBEN-DEPENDENTS-EXIST              VALUE 1.
                   88 BNBEN-NO-DEPENDENTS                 VALUE 0.
               03  BNBEN-HRH-A-VALUE           PIC X(10)  VALUE SPACES.
J46844         03  BNBEN-ANNUAL-HOURS          LIKE ANNUAL-HOURS
                                                          VALUE ZEROES.
J67795         03  BNBEN-PREM-EMP-CONT         PIC 9(13)V9(4) COMP-3
                                                          VALUE ZEROES.
J67795         03  BNBEN-EMP-CONT              PIC S9(13)V9(4) COMP-3
                                                          VALUE ZEROES.
J67795         03  BNBEN-CMP-CONT              PIC 9(13)V9(4) COMP-3
                                                          VALUE ZEROES.
J67795         03  BNBEN-EMP-PREMIUM           PIC 9(13)V9(4) COMP-3
                                                          VALUE ZEROES.
               03  BNBEN-SAVE-COMPANY          PIC 9(04)  VALUE ZEROES.
               03  BNBEN-SAVE-EMPLOYEE         PIC 9(09)  VALUE ZEROES.
               03  BNBEN-SAVE-START-DATE       PIC 9(08)  VALUE ZEROES.
               03  BNBEN-SAVE-PLAN-TYPE        PIC X(02)  VALUE SPACES.
               03  BNBEN-SAVE-PLAN-CODE        PIC X(04)  VALUE SPACES.
               03  BNBEN-C5-MAX-OC             PIC 9(02)  VALUE 13.
010900         03  BNBEN-BENEFS-SW             PIC 9(01)  VALUE 0.
011000             88  BNBEN-BENEFS-NOT-OK                VALUE 0.
011100             88  BNBEN-BENEFS-OK                    VALUE 1.
010300         03  BNBEN-DEL-BEN-SW            PIC 9(01)  VALUE 0.
010400             88  BNBEN-NOT-DELETING-BEN             VALUE 0.
010500             88  BNBEN-DELETING-BEN                 VALUE 1.
010600         03  BNBEN-CHG-BEN-SW            PIC 9(01)  VALUE 0.
010700             88  BNBEN-NOT-CHANGING-BEN             VALUE 0.
010800             88  BNBEN-CHANGING-BEN                 VALUE 1.
010100         03  BNBEN-STOP-DFLT-SW          PIC 9(01)  VALUE 0.
010200             88  BNBEN-STOP-DEFAULTED               VALUE 1.
010100         03  BNBEN-I1                    PIC 9(04)  COMP-3
                                                          VALUE ZEROES.
010100         03  BNBEN-I2                    PIC 9(04)  COMP-3
                                                          VALUE ZEROES.
010100         03  BNBEN-I3                    PIC 9(04)  COMP-3
                                                          VALUE ZEROES.
J32750         03  BNBEN-I4                    PIC 9(04)  COMP-3
J32750                                                    VALUE ZEROES.
J32750         03  BNBEN-I5                    PIC 9(04)  COMP-3
J32750                                                    VALUE ZEROES.
010100         03  BNBEN-NEW-ADD-AT            PIC 9(04)  COMP-3
                                                          VALUE ZEROES.
010100         03  BNBEN-LAST-DETAIL           PIC 9(04)  COMP-3
                                                          VALUE ZEROES.
010100         03  BNBEN-ORIG-NBR-LINES        PIC 9(04)  COMP-3
                                                          VALUE ZEROES.
027800         03  BNBEN-ADD-START-DATE        PIC 9(08)  VALUE ZEROES.
               03  BNBEN-DONE-SW               PIC 9(01)  VALUE ZEROES.
                   88 ITS-NOT-DONE                        VALUE 0.
                   88 ITS-DONE                            VALUE 1.
               03  BNBEN-PASS-ERROR-CAT        PIC X(05)  VALUE SPACES.
               03  BNBEN-PASS-MSG-NBR          PIC 9(03)  VALUE ZEROES.
********************MODS BELOW *****************************************
               03  BNBEN-HUT-KEY.
                   04  BNBEN-SYSTEM            PIC X(02)  VALUE "BN".
                   04  BNBEN-RELEASE           PIC 9(01)  VALUE 7.
                   04  BNBEN-REL-LEVEL         PIC 9(02)  VALUE 2.
                   04  BNBEN-PLN-KEY1          PIC X(10)  VALUE "BN15".
                   04  BNBEN-BEN-KEY1          PIC X(10)  VALUE
                                                            "BNTRANS".
               03  BNBEN-HUT-PLN-DATA.
                   04  BNBEN-PLN-CREATE-TRANS
                                               PIC X(01)  VALUE SPACES.
                   04  BNBEN-PLN-PLAN-SPONSOR-ID
                                               PIC X(30)  VALUE SPACES.
                   04  BNBEN-PLN-ID-CLASSIFICATION
                                               PIC X(01)  VALUE SPACES.
                   04  BNBEN-PLN-INSURANCE-LINE
                                               PIC X(03)  VALUE SPACES.
               03  BNBEN-HUT-BEN-DATA.
                   04  BNBEN-BEN-STATUS        PIC X(06)  VALUE SPACES.
                   04  BNBEN-BEN-FC            PIC X(01)  VALUE SPACES.
                   04  BNBEN-BEN-REASON        PIC X(02)  VALUE SPACES.
                   04  BNBEN-BEN-MEMBER-ID     PIC 9(01)  VALUE ZEROES.
                   04  BNBEN-BEN-EFF-DATE      PIC 9(08)  VALUE ZEROES.
                   04  BNBEN-BEN-STOP-DATE     PIC 9(08)  VALUE ZEROES.
                   04  BNBEN-BEN-COVER-TYPE    PIC X(01)  VALUE SPACES.
                   04  BNBEN-BEN-COBRA-EVENT   PIC X(01)  VALUE SPACES.
                   04  BNBEN-BEN-EMP-START     PIC 9(08)  VALUE ZEROES.
***********************MODS END ABOVE ************************

183700         03  BNBEN-TRAN-SEQ-NBR         LIKE SEQ-NBR VALUE ZEROES.
               03  BNBEN-HIPAA-DEFAULT-SW      PIC X(05)   VALUE SPACES.
                   88  BNBEN-HIPAA-DEFAULT                 VALUE "BN100"
                                                                 "BN101"
                                                                 "BN102"
                                                                 "BN103"
                                                                "BN105".
               03  BNBEN-HIPAA-REASON          LIKE REASON VALUE SPACES.
               03  BNBEN-BT                    LIKE TYPE   VALUE "BT".
               03  BNBEN-BTE-PERCENT           LIKE PERCENT
                                                           VALUE ZEROES.
               03  BNBEN-TOTAL-PCT-ON-BTE      LIKE PERCENT
                                                           VALUE ZEROES.
027800         03  BNBEN-MAX-DTL-LINES         PIC 9(02)  VALUE 30.
027800         03  BNBEN-DTL-OVERFLOW-SW       PIC 9(01)  VALUE ZEROES.
                   88  NOT-DTL-OVERFLOW                   VALUE 0.
                   88  DTL-OVERFLOW                       VALUE 1.
RAY            03  WS-BNBEN-START-DATE         PIC 9(08) VALUE ZERO.
GW             03  WS-BNBEN-STOP-DATE          PIC 9(08) VALUE ZERO.
RAY            03  WS-BNBEN-NOTFND-SW-SAVE     PIC 9(01) VALUE ZERO.
               03  BNBEN-DED-TABLE.
                   04 BNBEN-DED-CODE           OCCURS 25 TIMES
                                               PIC X(05)  VALUE SPACES.
                   04 BNBEN-DED-SEQ            OCCURS 25 TIMES
                                               PIC 9(04)  VALUE ZEROES.
               03  BNBEN-EDM-TABLE.
                   04 BNBEN-EDM-CODE           OCCURS 25 TIMES
                                               PIC X(05)  VALUE SPACES.
                   04 BNBEN-EDM-SEQ            OCCURS 25 TIMES
                                               PIC 9(04)  VALUE ZEROES.
                   04 BNBEN-EDM-EFFECT         OCCURS 25 TIMES
                                               PIC 9(08)  VALUE ZEROES.
                   04 BNBEN-EDM-END            OCCURS 25 TIMES
                                               PIC 9(08)  VALUE ZEROES.
183900         03  BNBEN-DED-START         LIKE START-DATE VALUE ZEROES.
183900         03  BNBEN-DED-STOP          LIKE STOP-DATE  VALUE ZEROES.
183900         03  BNBEN-EFFECT-DATE      LIKE EFFECT-DATE VALUE ZEROES.
183900         03  BNBEN-END-DATE          LIKE END-DATE   VALUE ZEROES.
183900         03  BNBEN-SV-EMP            LIKE EMPLOYEE   VALUE ZEROES.
183900         03  BNBEN-SV-PL-TP          LIKE PLAN-TYPE  VALUE SPACES.
183900         03  BNBEN-SV-PL-CD          LIKE PLAN-CODE  VALUE SPACES.
183900         03  BNBEN-SV-I1             PIC 9(04)       VALUE ZEROES.
183900         03  BNBEN-FUNCTION-CODE     PIC X(04)       VALUE SPACES.
           02  BNBEN-PRF-WS.
               03  BNBEN-SV-PLT          LIKE PLAN-TYPE   VALUE SPACES.
               03  BNBEN-SV-PLC          LIKE PLAN-CODE   VALUE SPACES.
               03  BNBEN-SV-EE           LIKE EMPLOYEE    VALUE ZEROES.
P58843     02  BEN-YTD-CONT-SW             PIC 9(1)   VALUE ZEROS.
P58843         88  NOT-ZERO-YTD-CONT                  VALUE 0.
P58843         88  ZERO-YTD-CONT                      VALUE 1.
P85709     02  BEN-COUNT                   PIC 9(02) VALUE ZEROES.
J92654** REDEFINED VARIABLE INTRODUCED BY J67795.
J92654     02  BNBEN-TEMP-NBR              PIC S9(13)V9(4) VALUE ZEROES
J92654                                     SIGN IS TRAILING SEPARATE.
J92654     02  BNBEN-TEMP-NBR-R            REDEFINES BNBEN-TEMP-NBR.
J92654         03 FILLER                   PIC X(15).
J92654         03 BNBEN-DEC-3-4            PIC X(02).
J92654         03 FILLER                   PIC X.
J66130     02  BNBEN-COVER-AMT-SW          PIC X(01) VALUE SPACES.
J66130         88 BNBEN-COVER-AMT-Y                  VALUE "Y".
J66130     02  BNBEN-PAY-RATE-SW           PIC X(01) VALUE SPACES.
J66130         88 BNBEN-PAY-RATE-Y                   VALUE "Y".
J66130     02  BNBEN-EMP-PRE-CONT-SW       PIC X(01) VALUE SPACES.
J66130         88 BNBEN-EMP-PRE-CONT-Y               VALUE "Y".
J66130     02  BNBEN-EMP-AFT-CONT-SW       PIC X(01) VALUE SPACES.
J66130         88 BNBEN-EMP-AFT-CONT-Y               VALUE "Y".
J66130     02  BNBEN-COMP-CONT-SWT         PIC X(01) VALUE SPACES.
J66130         88 BNBEN-COMP-CONT-Y                  VALUE "Y".
117816     02  BNBEN-PROC-FLEX-SW          PIC 9(01) VALUE ZERO.
117816         88 BNBEN-PROC-NON-FLEX                VALUE ZERO.
117816         88 BNBEN-PROC-FLEX                    VALUE 1. 
GW7/07     02 WS-NONPARTIC-CD            PIC X(03) VALUE "0".
