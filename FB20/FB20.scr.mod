******* FB20.scr 24 <1981084269>
$RELEASE   08.0
******************************************************************
*               M O D I F I C A T I O N   L O G:                 *
******************************************************************
*  Modified by MARK GLISSEN - MG0710                             *
******************************************************************
*  07/10/07  - MODIFIED FB20 FOR VISIBILITY REASONS              *
*  11/01/10  - M. Hunter - ACS - Reapplied above mods after 9.0  *
*                                Apps Upgrade. Moved Currency    *
*                                code field                      *
*  08/25/11  - M. Hunter - ACS - Reapplied above mods after 9.0.1*
*                                Apps Upgrade.                   *
*                                                                *
******************************************************************
*******************************************************************************
*                            SCREEN   FB20  (1)
*******************************************************************************
$FORM      FB201
$MENU      FBMN1
$KEYFCS    CD
$IBCM      A 418
$IBCM      C 417
$IBCM      INP 416
$DATAFCS   AC
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
$DRAW      GROUP    8, 1, 1,80
$URLKN     GB,02,04,05
$COMMENTKN GB,02,04,05
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB201
[FB20[ ]                         ?Period Budget, Rate

| Company:&    ] |Year:&    ] |Budget:&   ];  ;;                              ;
;            ;   | Currency: [    ]            ;                 ;
| Account: [               &      &    ]  ;                                   ;
|  Action: [ ]|Spread Code:[   ]|Compute:[            ]
           [ New Budget ]      [ By Account ]      [ By Accounting Unit ]
   
|Prd|        Units |Factor ]|        Rate ]|       Amount ]|    Year to Date ]
|BB]&               [       &               &               ];                 ;
; 1;&               [       &               &               ];                 ;
; 2;&               [       &               &               ];                 ;
; 3;&               [       &               &               ];                 ;
; 4;&               [       &               &               ];                 ;
; 5;&               [       &               &               ];                 ;
; 6;&               [       &               &               ];                 ;
; 7;&               [       &               &               ];                 ;
; 8;&               [       &               &               ];                 ;
; 9;&               [       &               &               ];                 ;
;10;&               [       &               &               ];                 ;
;11;&               [       &               &               ];                 ;
;12;&               [       &               &               ];                 ;
;13;&               [       &               &               ];                 ;
|Tot;               ;                       ;               ;
$END-SCR   FB201
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     FB201
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2    *ORIGINATOR            P     06          RO 
  2     FBD-COMPANY           N     04          SK    01  C
  2     FBD-FISCAL-YEAR       N     04          SN    06 
  2     FBD-BUDGET-NBR        N     03          SN    GB 
  2     FBH-VERSION           N     02          SB
  2     FBH-DESCRIPTION       AL    30          SB    G10 
  2     FBH-DOUBLE-ENTRY-MSG  AL    12          SB
  2     FBH-CURRENCY-CODE     A     05          SB    98 
  2     FBH-COMMIT-BUD-MSG    AL    17          SB
  2     FBD-ACCT-UNIT         A     15          SN    02  P
  2     FBD-ACCOUNT           N     06          SN    04 
  2     FBD-SUB-ACCOUNT       N     04          SL    05 
  2     GDT-ACCOUNT-DESC      AL    60          SB    04D 
  2     ACTION-CODE           A     01          SB 
  2     FBD-SPREAD-CODE       A     03          SB    GD 
  2     FBD-COMPUTE-NAME      A     12          SB    LK 
  2     HEADER-WIN            W     12          SB          FB20.2
  2     ACCOUNT-WIN           W     12          SB          FB20.3
  2     ACCT-UNIT-WIN         W     20          SB          FB20.4
  2     BEG-BAL-UNITS         VS    17        2 SB    GHD
  2     BB-FACTOR             A     15          SB    FB1
  2     BB-FACTOR-RATE        S     16        7 SB    FB3   
  2     BEG-BAL-AMT           VS    17        2 SB 
  2     BEG-BAL-YTD-AMT       VS    17        2 SB          ZZZZZZZZZZZZZ.ZZ-
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 13       No_Grid
    4   PERIOD-NBR            N     02          LB 
    4   FBD-UNITS             VS    17        2 LB    GHD
    4   FBD-FACTOR            A     15          LB    FB1
    4   FBD-FACTOR-RATE       S     16        7 LB    FB3   
    4   FBD-AMOUNT            VS    17        2 LB    GAB
    4   FBD-YTD-AMOUNT        VS    17        2 LB          ZZZZZZZZZZZZZ.ZZ-
  2     FBD-UNITS-TOT         VS    17        2 SB 
  2     FBD-AMOUNT-TOT        VS    17        2 SB 
$END-TRANS FB201
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB201
  FC                    I                     A=Add
                                              C=Change
                                              D=Delete
                                              I=Inquire
                                              N=Next
                                              P=Previous
  ACTION-CODE                                 S=Spread
                                              D=Duplicate
                                              A=Actual LY
                                              B=Budget LY
                                              R=Actual CY   
                                              C=Compute Amounts
                                              U=Compute Units 
$END-EDITS FB201
*******************************************************************************
*                            SCREEN   FB20  (2)
*******************************************************************************
$FORM      FB202
$WINDOW
$ROW       BOTTOM
$NOTKXFER
$KEYFCS    CD
$IBCM      INP 414
$IBCM      C 415
$DATAFCS   AC
$DBLXMTFCS D
$URLKN     GB
$COMMENTKN GB
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB202
[ ]                          ?Define Budget

|              Company:&    ] ;                              ;
|                 Year:&    ]
|               Budget:&   ]  ;  ;  [                              ] 
|        Currency Code:[     ];                              ;

|Allow Amount Decimals:[ ]    ;   ;
|  Allow Unit Decimals:[ ]    ;   ;
|         Double Entry:[ ]    ;                              ;
|    Commitment Budget:[ ]    ;                    ;

                               [  Lock  ] ;          ;
$END-SCR   FB202
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     FB202
  2    *TC                    A     06          SR 
  2     FC                    A     01          SF 
  2    *ORIGINATOR            P     06          RO 
  2     FBH-COMPANY           N     04          SK    01  C
  2     GLS-NAME              AL    30          SB    01D 
  2    *GLS-CURRENCY-CODE     A     05          SB    GC 
  2     FBH-FISCAL-YEAR       N     04          SN    06 
  2     FBH-BUDGET-NBR        N     03          SN    GB 
  2     FBH-VERSION           N     02          SB
  2     FBH-DESCRIPTION       AL    30          SR    G10 
  2     FBH-CURRENCY-CODE     A     05          SB    98 
  2     CUC-DESCRIPTION       A     30          SB    98D
  2     FBH-ALLOW-DEC         A     01          SB 
  2     FBH-ALLOW-DEC         X     03          SB 
  2     FBH-ALLOW-UNIT-DEC    A     01          SB 
  2     FBH-ALLOW-UNIT-DEC    X     03          SB 
  2     FBH-DOUBLE-ENTRY      A     01          SB
  2     FBH-DOUBLE-ENTRY      X     30          SB
  2     FBH-COMMIT-BUD        A     01          SB
  2     FBH-COMMIT-BUD        X     20          SB
  2     BUD-LOCK-WIN          W     08          SB          FB20.5
  2    *LOCK                  A     01          SB 
  2     LOCK                  X     10          SB 
  2    *USER-CLASS            A     12          SB 
  2    *DESCRIPTION           A     30          SB 
  2    *FBH-LOCK              A     01          SB    GlA   BUD-LOCK-WIN
  2    *FBH-USER-CLASS        A     12          SB    FB5   BUD-LOCK-WIN
  2    *RWH-DESCRIPTION       A     30          SB    G0    BUD-LOCK-WIN
$END-TRANS FB202
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB202
  FC                                          A=Add
                                              C=Change
                                              D=Delete
                                              I=Inquire
                                              N=Next
                                              P=Previous
  FBH-ALLOW-DEC         N                     N=No
                                              Y=Yes
  FBH-ALLOW-UNIT-DEC    N                     N=No
                                              Y=Yes
  FBH-DOUBLE-ENTRY      N                     N=No
                                              Y=Yes
                                              B=Budget Edits on Release
  FBH-COMMIT-BUD        N                     N=No
                                              Y=Yes
  LOCK                                        Y=Locked
                                              N=
  FBH-LOCK              N
$END-EDITS FB202
*******************************************************************************
*                            SCREEN   FB20  (3)
*******************************************************************************
$FORM      FB203
$WINDOW
$NOTKXFER
$NOLFCOK
$KEYFCS    CD
$DATAFCS   C
$LDATAFCS  ACD
$FCPAIRS   CA,CC,CD,IX
$LSELECT   X
$LINEKN    GBD
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
$DRAW      GROUP    8, 1, 1,77
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB203
[ ]                            ?By Account

|   Company:;    ;|   Year:;    ; |Budget:;   :     ;
|    Period:&  ]                  ;                         ;
|   Account:&      &    ]  ;                                              ;
|    Action:[ ] ;            ;| Spread Code:[   ]
        [Computed Budget]|                     Position To:[               ]

|FC |Accounting Unit|          Units|           Amounts|          Year to Date
[ ];               ;&               ]  &               ] ;                   ;
   ;                              ;   ;                :      ;
[ ];               ;&               ]  &               ] ;                   ;
   ;                              ;   ;                :      ;
[ ];               ;&               ]  &               ] ;                   ;
   ;                              ;   ;                :      ;
[ ];               ;&               ]  &               ] ;                   ;
   ;                              ;   ;                :      ;
[ ];               ;&               ]  &               ] ;                   ;
   ;                              ;   ;                :      ;
[ ];               ;&               ]  &               ] ;                   ;
   ;                              ;   ;                :      ;
$END-SCR   FB203
*                            ELEMENT NAME                                       
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE       
* ----- --------------------- --  ----        - -- -- --- - --------------------
$TRANS     FB203
  2    *TC                    A     06          SR 
  2     FC                    A     01          SF 
  2    *ORIGINATOR            P     06          RO 
  2     FBD-COMPANY           N     04          SB    01  C
  2    *GLS-NAME              AL    30          SB    01D 
  2    *GLS-CURRENCY-CODE     A     05          SB    GC 
  2     FBD-FISCAL-YEAR       N     04          SB    06 
  2     FBD-BUDGET-NBR        N     03          SB    GB 
  2     FBH-CURRENCY-CODE     A     05          SB    98 
  2     FBD-PERIOD            N     02          SB    GP 
  2     FBH-DESCRIPTION       AL    30          SB    G10 
  2     FBD-ACCOUNT           N     06          SN    04S 
  2     FBD-SUB-ACCOUNT       N     04          SL    05S 
  2     GDT-ACCOUNT-DESC      AL    60          SB 
  2     ACTION-CODE           A     01          SB 
  2     ACTION-CODE           X     25          SB 
  2     FBD-SPREAD-CODE       A     03          SB    GD 
  2     COMP-ACCT-UNIT-WIN    W     15          SB          FB31.1
  2    *PT-FBD-VAR-LEVELS     A     30          SB 
  2     PT-FBD-ACCT-UNIT      A     15          SB 
  2     DETAIL-GROUP          G           
   3    DETAIL-LINE           G                    06        No_Grid
    4   LINE-FC               A     01          LF 
    4   GLN-ACCT-UNIT         A     15          LB    02  P
    4   FBD-UNITS             VS    17        2 LB
    4   FBD-AMOUNT            VS    17        2 LB          
    4   FBD-YTD-AMOUNT        VS    17        2 LB           ZZZZZZZZZZZZZ.ZZ-
    4   GLN-DESCRIPTION       AL    30          LB    02D                 
    4   ACTIVE-MSG            A     16          LB 
    4   RECORD-EXISTS         A     06          LB 
$END-TRANS FB203
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB203
  FC                                          C=Change
                                              I=Inquire
                                              N=Next
                                              P=Previous
                                              +=PageDown
                                              -=PageUp
                                              F=Forward
                                              B=Backward
  FBD-PERIOD                                  00=Beg Bal 
                                              01=Period 1
                                              02=Period 2
                                              03=Period 3
                                              04=Period 4
                                              05=Period 5
                                              06=Period 6
                                              07=Period 7
                                              08=Period 8
                                              09=Period 9
                                              10=Period 10
                                              11=Period 11
                                              12=Period 12
                                              13=Period 13
  ACTION-CODE                                 S=Spread
                                              A=Actual Last Year
                                              B=Budget Last Year
                                              R=Actual Curr Year
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
                                              X=Select for Transfer
$END-EDITS FB203
*******************************************************************************
*                            SCREEN   FB20  (4)
*******************************************************************************
$FORM      FB204
$WINDOW
$NOTKXFER
$NOLFCOK
$KEYFCS    CD
$DATAFCS   C
$LDATAFCS  ACD
$FCPAIRS   CA,CC,CD,IX
$LSELECT   X
$LINEKN    GBE
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
$DRAW      GROUP    8, 1, 1,77
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB204
[ ]                        ?By Accounting Unit

|           Company:;    ; | Year:;    ; |Budget:;     :    ;
|            Period:&  ];                :                              ;
|   Accounting Unit:[               ];                              ;
|            Action:[ ];            ;|  Spread Code:[   ]
                [Computed Budget]|                  Position To:&      &    ]

|FC |Account Number|           Units|           Amounts|          Year to Date
[ ] '000000]'0000]  &               ]  &               ] ;                   ;
    ;                                          ; ;                   :      ;
[ ] '000000]'0000]  &               ]  &               ] ;                   ;
    ;                                          ; ;                   :      ;
[ ] '000000]'0000]  &               ]  &               ] ;                   ;
    ;                                          ; ;                   :      ;
[ ] '000000]'0000]  &               ]  &               ] ;                   ;
    ;                                          ; ;                   :      ;
[ ] '000000]'0000]  &               ]  &               ] ;                   ;
    ;                                          ; ;                   :      ;
[ ] '000000]'0000]  &               ]  &               ] ;                   ;
    ;                                          ; ;                   :      ;
$END-SCR   FB204
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     FB204
  2    *TC                    A     06          SR 
  2     FC                    A     01          SF 
  2    *ORIGINATOR            P     06          RO 
  2     FBD-COMPANY           N     04          SB    01  C
  2    *GLS-NAME              AL    30          SB    01D 
  2    *GLS-CURRENCY-CODE     A     05          SB    GC 
  2     FBD-FISCAL-YEAR       N     04          SB    06 
  2     FBD-BUDGET-NBR        N     03          SB    GB 
  2     FBH-CURRENCY-CODE     A     05          SB    98 
  2     FBD-PERIOD            N     02          SB    GP 
  2     ACTIVE-MSG-H          A     16          SB 
  2     FBH-DESCRIPTION       AL    30          SB    G10 
  2     FBD-ACCT-UNIT         A     15          SN    02  P
  2     GLN-DESCRIPTION       AL    30          SB 
  2     ACTION-CODE           A     01          SB 
  2     ACTION-CODE           X     25          SB 
  2     FBD-SPREAD-CODE       A     03          SB    GD 
  2     COMP-ACCOUNT-WIN      W     15          SB          FB30.1
  2     PT-GLM-ACCOUNT        N     06          SB 
  2     PT-GLM-SUB-ACCOUNT    N     04          SB 
  2     DETAIL-GROUP          G           
   3    DETAIL-LINE           G                    06       No_Grid
    4   LINE-FC               A     01          LF 
    4   FBD-ACCOUNT           N     06          LL    04 
    4   FBD-SUB-ACCOUNT       N     04          LL    05 
    4   FBD-UNITS             VS    17        2 LB 
    4   FBD-AMOUNT            VS    17        2 LB 
    4   FBD-YTD-AMOUNT        VS    17        2 LB           ZZZZZZZZZZZZZ.ZZ-
    4   GDT-ACCOUNT-DESC      AL    60          LB 
    4   ACTIVE-MSG-D          A     19          LB 
    4   RECORD-EXISTS         A     06          LB 
$END-TRANS FB204
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB204
  FC                                          C=Change
                                              I=Inquire
                                              N=Next
                                              P=Previous
                                              +=PageDown
                                              -=PageUp
                                              F=Forward
                                              B=Backward
  FBD-PERIOD                                  00=Beg Bal
                                              01=Period 1
                                              02=Period 2
                                              03=Period 3
                                              04=Period 4
                                              05=Period 5
                                              06=Period 6
                                              07=Period 7
                                              08=Period 8
                                              09=Period 9
                                              10=Period 10
                                              11=Period 11
                                              12=Period 12
                                              13=Period 13
  ACTION-CODE                                 S=Spread
                                              A=Actual Last Year
                                              B=Budget Last Year
                                              R=Actual Curr Year
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
                                              X=Select for Transfer
$END-EDITS FB204
*******************************************************************************
*                            SCREEN   FB20 (5)
*******************************************************************************
$FORM      FB205    S
$WINDOW
$NOTKXFER
$KEYFCS    CD
$DATAFCS   AC
$NEXTFCS   NP
$ADDFCS    A
$DELFCS    D
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB205    S
             ?|Lock

|          Lock Budget:[ ]  ;          ;

|           User Class:[            ]  ;                              ; 
$END-SCR   FB205    S
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     FB205    S
  2    *TC                    A     06          SR 
  2    *FC                    A     01          SF 
  2     FBH-LOCK              A     01          SB    GlA 
  2     FBH-LOCK              X     10          SB 
  2     FBH-USER-CLASS        A     12          SB    FB5
  2     RWH-DESCRIPTION       AL    30          SB    G0
$END-TRANS FB205    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB205    S
  FC                                          I=Inquire
                                              C=Change
  FBH-LOCK                                    N=No
                                              Y=Yes
$END-EDITS FB205    S
*******************************************************************************
*                            SCREEN   FB20  (6)
*******************************************************************************
$FORM      FB206
$MENU      FBMN1
$KEYFCS    CD
$DATAFCS   AC
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
$DRAW      GROUP    8, 1, 1,80
$URLKN     GB
$COMMENTKN GB
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB206
[FB20[ ]                         ?Period Budget, Amounts

|Company:&    ] |   Year:&    ] |   Budget:&   ];     ;     
;                         ;               
|Account:[               &      &    ]  ;                                 ;
| Action:[ ]|Spread Code:[   ]|Compute:[            ]
         [ New Budget ]      [ By Account ]      [ By Accounting Unit ]
   
^Period ]|          Amount|Year to Date   |Last Year Actual|Last Year Budget]
|Beg Bal]&               ];               :               :               ;
     ; 1;&               ];               :               :               ;
     ; 2;&               ];               :               :               ;
     ; 3;&               ];               :               :               ;
     ; 4;&               ];               :               :               ;
     ; 5;&               ];               :               :               ;
     ; 6;&               ];               :               :               ;
     ; 7;&               ];               :               :               ;
     ; 8;&               ];               :               :               ;
     ; 9;&               ];               :               :               ;
     ;10;&               ];               :               :               ;
     ;11;&               ];               :               :               ;
     ;12;&               ];               :               :               ;
     ;13;&               ];               :               :               ;
|  Total  ;               ;               ;               :               ;
$END-SCR   FB206
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     FB206
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2    *ORIGINATOR            P     06          RO 
  2     FBD-COMPANY           N     04          SK    01  C
  2     FBD-FISCAL-YEAR       N     04          SN    06 
  2     FBD-BUDGET-NBR        N     03          SN    GB 
  2     FBH-CURRENCY-CODE     A     05          SB    98 
  2     FBH-DESCRIPTION       AL    25          SB    G10 
  2     FBD-ACCT-UNIT         A     15          SN    02  P
  2     FBD-ACCOUNT           N     06          SN    04 
  2     FBD-SUB-ACCOUNT       N     04          SL    05 
  2     GDT-ACCOUNT-DESC      AL    60          SB    04D 
  2     ACTION-CODE           A     01          SB 
  2     FBD-SPREAD-CODE       A     03          SB    GD 
  2     FBD-COMPUTE-NAME      A     12          SB    LK 
  2     HEADER-WIN            W     12          SB          FB20.2
  2     ACCOUNT-WIN           W     12          SB          FB20.3
  2     ACCT-UNIT-WIN         W     20          SB          FB20.4
  2     BEG-BAL-AMT           VS    17        2 SB
  2     BEG-BAL-YTD-AMT       VS    17        2 SB          ZZZZZZZZZZZ.ZZ-
  2     BEG-BAL-LY-ACTUAL     VS    17        2 SB          ZZZZZZZZZZZ.ZZ-
  2     BEG-BAL-LY-BUDGET     VS    17        2 SB          ZZZZZZZZZZZ.ZZ-
  2     DETAIL-GROUP          G                 SB                        
   3    DETAIL-LINE           G                 SB 13       No_Grid
    4   PERIOD-NBR            N     02          LB 
    4   FBD-AMOUNT            VS    17        2 LB    GAB
    4   FBD-YTD-AMOUNT        VS    17        2 LB          ZZZZZZZZZZZ.ZZ-
    4   FBD-LY-ACTUAL         VS    17        2 LB          ZZZZZZZZZZZ.ZZ-
    4   FBD-LY-BUDGET         VS    17        2 LB          ZZZZZZZZZZZ.ZZ-
  2     FBD-AMOUNT-TOT        VS    17        2 SB 
  2     FBD-LY-ACTUAL-TOT     VS    17        2 SB
  2     FBD-LY-BUDGET-TOT     VS    17        2 SB
$END-TRANS FB206
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB206
  FC                    I                     A=Add
                                              C=Change
                                              D=Delete
                                              I=Inquire
                                              N=Next
                                              P=Previous
  ACTION-CODE                                 S=Spread
                                              D=Duplicate
                                              A=Actual LY
                                              B=Budget LY
                                              R=Actual CY
                                              Y=Year to Date   
                                              C=Compute Amounts
$END-EDITS FB206
*******************************************************************************
*                            SCREEN   FB20  (7)
*******************************************************************************
$FORM      FB207
$MENU      FBMN1
$KEYFCS    CD
$DATAFCS   AC
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
$DRAW      GROUP    7, 1, 1,80
$URLKN     GB
$COMMENTKN GB
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    FB207
[FB20[ ]                         ?Period Budget, Units

| Company: &    ] |   Year:&    ] |   Budget:&   ];                         ;
| Account: [               &      &    ]  ;                                 ;
|  Action: [ ]|Spread Code:[   ]|Compute:[            ]
           [ New Budget ]      [ By Account ]      [ By Accounting Unit ]
   
^Period ]|           Units|Year to Date   |Last Year Actual|Last Year Budget]
|Beg Bal]&               ];               :               :               ;
     ; 1;&               ];               :               :               ;
     ; 2;&               ];               :               :               ;
     ; 3;&               ];               :               :               ;
     ; 4;&               ];               :               :               ;
     ; 5;&               ];               :               :               ;
     ; 6;&               ];               :               :               ;
     ; 7;&               ];               :               :               ;
     ; 8;&               ];               :               :               ;
     ; 9;&               ];               :               :               ;
     ;10;&               ];               :               :               ;
     ;11;&               ];               :               :               ;
     ;12;&               ];               :               :               ;
     ;13;&               ];               :               :               ;
|  Total  ;               ;                ;               :               ;
$END-SCR   FB207
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     FB207
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2    *ORIGINATOR            P     06          RO 
  2     FBD-COMPANY           N     04          SK    01  C
  2     FBD-FISCAL-YEAR       N     04          SN    06 
  2     FBD-BUDGET-NBR        N     03          SN    GB 
  2     FBH-DESCRIPTION       AL    25          SB    G10 
  2     FBD-ACCT-UNIT         A     15          SN    02  P
  2     FBD-ACCOUNT           N     06          SN    04 
  2     FBD-SUB-ACCOUNT       N     04          SL    05 
  2     GDT-ACCOUNT-DESC      AL    60          SB    04D 
  2     ACTION-CODE           A     01          SB 
  2     FBD-SPREAD-CODE       A     03          SB    GD 
  2     FBD-COMPUTE-NAME      A     12          SB    LK 
  2     HEADER-WIN            W     12          SB          FB20.2
  2     ACCOUNT-WIN           W     12          SB          FB20.3
  2     ACCT-UNIT-WIN         W     20          SB          FB20.4
  2     BEG-BAL-UNITS         VS    17        2 SB 
  2     BEG-BAL-YTD-UNITS     VS    17        2 SB          ZZZZZZZZZZZ.ZZ-
  2     BEG-BAL-LY-ACTUAL     VS    17        2 SB          ZZZZZZZZZZZ.ZZ-
  2     BEG-BAL-LY-BUDGET     VS    17        2 SB          ZZZZZZZZZZZ.ZZ-
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 13       No_Grid
    4   PERIOD-NBR            N     02          LB 
    4   FBD-UNITS             VS    17        2 LB 
    4   FBD-YTD-UNITS         VS    17        2 LB          ZZZZZZZZZZZ.ZZ-
    4   FBD-LY-ACTUAL         VS    17        2 LB          ZZZZZZZZZZZ.ZZ-
    4   FBD-LY-BUDGET         VS    17        2 LB          ZZZZZZZZZZZ.ZZ-
  2     FBD-UNITS-TOT         VS    17        2 SB 
  2     FBD-LY-ACTUAL-TOT     VS    17        2 SB
  2     FBD-LY-BUDGET-TOT     VS    17        2 SB
$END-TRANS FB207
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     FB207
  FC                    I                     A=Add
                                              C=Change
                                              D=Delete
                                              I=Inquire
                                              N=Next
                                              P=Previous
  ACTION-CODE                                 S=Spread
                                              D=Duplicate
                                              A=Actual LY
                                              B=Budget LY
                                              R=Actual CY
                                              Y=Year to Date   
                                              U=Compute Units
$END-EDITS FB207
