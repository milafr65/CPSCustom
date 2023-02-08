******* PR29.scr 27 <274223912>
$RELEASE   02.1
*
*
*
*******************************************************************************
*                            SCREEN   PR29  (1)
*******************************************************************************
$FORM      PR291    S
$NOLFCOK
$KEYFCS    CD
$DATAFCS   AC
$NEXTFCS   NP
$ADDFCS    A
$DELFCS    D
$LDATAFCS  ACD
$LADDFCS   A
$FCPAIRS   AA,CA,CC,CD
$LINEDUPS
$COMMENTKN H07
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    PR291    S
[PR29[ ]                  ?Pension Payment
+10] +10]                    +      ]
|               Company:&    ]       ;                              ;
|           Participant:&         ]  ;                                        ;
|        Effective Date:&      ] - &      ]
|         Process Level:;     :                              ; |;             ;
|            Department:;     ;    |        Distribution Amount:;             ;
$TAB-REGION
$TAB-FOLDER
$TAB-FORM  MAIN              " Main "
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
|
|     Distribution Type: &  ]        ;                              ;
|     Distribution Code: [ ] [ ]     ;                                 ;
|      W9 Received Date: ;      ;     Benefit Start Date: &      ]
|          Tax Category:&  ]        ;                              ;
|               R-PPPT#: ;          ;        Review Date: &      ]
|         Process Level:[     ]     ;                              ;
|            Department:[     ]     ;                              ;
|      Addl Federal Tax: &           ]     Review Reason: [   ] ;              ;
|Tax Frequency Override:& ]         ;                              ;
|         Process Group: [ ]               Roster Status: ;          ;
|    Percent Total Dist: &   ]      A-QDRO-CRP Indicator: ;          ;
|           Check Group: ; ;         Benefit Stop Reason: [   ] ;              ;
$END-TAB   MAIN
$TAB-FORM  SOURCE            " Source "
|
 |Source |Description             |Amount]  |Account
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
$END-TAB   SOURCE
$TAB-FORM  TIMEREC           " Time Record "
|
 |FC |Pay                                          |Amount
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
$END-TAB   TIMEREC
$TAB-FORM  PAYMENT           " Payment "
|
|                    Payable To:[                              ]
|                               [                              ]
|                     Address 1:[                              ]
|                     Address 2:[                              ]
|                     Address 3:[                              ]
|                     Address 4:[                              ]
|             City or Address 5:[                              ]
|             State or Province:[  ]
|                   Postal Code:[          ]
|                       Country:[  ]
|       Rollover Account Number:[                 ]
|          Miscellaneous Fields: [      ] [      ]  Seq Nbr ;    ;
$END-TAB   PAYMENT
$TAB-FORM  DEPOSIT           " Direct Deposit "
|
|         Receiving DFI:&         ]
|                  Bank:[                              ]
|               Account:[                 ]
|          Account Type:[ ]        ;                              ;
|   Payment Description:[        ]
|
|        Wire Indicator:& ]        ;                              ;
|          Cross Border:[ ]        ;                              ;
|                   IAT:[ ]        ;                              ;
$END-TAB   DEPOSIT
$TAB-FORM  RETRO             " Retro  "
|
| Source |Description             |Amount   |Account
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
 [      ];               ;  &            ]  ;                              ;
$END-TAB   RETRO
$TAB-FORM  RETRO-TR          " Retro TR "
|
| FC |Pay                                          |Amount
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
 [ ] [    ];                              ;  &             ]
$END-TAB   RETRO-TR
$END-REGION
$END-SCR   PR291    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     PR291    S
  2     TC                    AL    06          SB
  2     FC                    A     01          SF
  2     SRC-CNT               N     02          SB
  2     TRD-CNT               N     02          SB
  2     ORIGINATOR            P     06          RO
  2     PNP-COMPANY           N     04          SK    01  C
  2     PRS-NAME              A     30          SB    CN
  2     PNP-EMPLOYEE          N     09          SN    H07
  2     EMP-FULL-NAME         A     40          SB    HEN
  2     PNP-EFFECT-DATE       F     08          SN    TTE
  2     PNP-END-DATE          F     08          SB   *H38
  2    *PNP-PENS-SEQ-NBR      N     04          SB   *Hu7
  2     EMP-PROCESS-LEVEL     A     05          SB
  2     EMP-PROCESS-LEV-NAME  AL    30          SB   *HH4
  2     DISTRIBUTION-AMT2     N     13        2 SB
  2     EMP-DEPARTMENT        A     05          SB
  2     DISTRIBUTION-AMT      N     13        2 SB
*
  2     MAIN-TAB              AF    06          SB
  2     PNP-PENS-DIST-TYPE    N     02          SR
  2    *PT-DIST-TYPE          N     02          SR
  2     PNP-PENS-DIST-TYPE    X     30          SB
  2     PNP-PENS-DIST-CODE    A     01          SB
  2     PNP-PENS-DST-CODE2    A     01          SB
  2     PNP-PENS-DIST-CODE    X     33          SB
  2     HEU-D-FIELD-W9        F     08          SB
  2     WPN-BEN-START-DATE    F     08          SB
  2     PNP-TAX-CATEGORY      N     02          SB    HEK 
  2     PNP-TAX-CATEGORY      X     30          SB    HEM
  2     HEU-A-FIELD-R-PPPT    A     10          SB
  2     WPN-REVIEW-DATE       F     08          SB
  2     PROCESS-LEVEL         A     05          SB    H03 P
  2     PROCESS-LEV-NAME      AL    30          SB   *HH4
  2     DEPARTMENT            A     05          SB   *HH5
  2     DPT-NAME              A     30          SB   *HH6
  2     PNP-ADDL-FED-TAX      S     11        2 SB
  2     WPN-REVIEW-REASON     A     03          SB   *wb2
  2     WPN-REVIEW-DESC       A     14          SB   *wb2
  2     TAX-FREQ-OVER         N     01          SB
  2     TAX-FREQ-OVER         X     30          SB
  2     PROCESS-GRP           A     01          SB
  2     HEU-A-FIELD-ROSTER    A     10          SB
  2     PNP-PERC-TOT-DIST     N     03          SB
  2     HEU-A-FIELD-QDRO      A     10          SB
  2     STM-CHECK-GRP         A     01          SB
  2     WPN-BEN-STOP-RSN      A     03          SB   *wb1 
  2     WPN-STOP-DESC         A     14          SB   *wb1 
*
  2     SOURCE-TAB            AF    08          SB
  2     PNF-DETAIL-DATA       G     00          SB
   3    PNF-DETAIL-LINE       G     00          SB 10
    4   PNF-FUNDING-SOURCE    A     06          SB    fs
    4   PNF-PAYMENT-DESC      AL    15          SB
    4   PNF-FUND-AMOUNT       N     12        2 SB
    4   GLM-DESCRIPTION       A     30          SB
    4  *PFS-DIST-COMPANY      N     04          SB
    4  *PFS-DST-ACCT-UNIT     A     15          SB
    4  *PFS-DST-ACCOUNT       N     06          SB
    4  *PFS-DST-SUB-ACCT      N     04          SB
*
  2     TIMEREC-TAB           AF    13          SB
  2    *PT-SEQ-NBR            N     04          SB
  2    *PT-TIME-SEQ           N     04          SB
  2    *PT-CHECK-ID           N     12          SB
  2    *PT-PAY-CODE           A     04          SB
  2     DETAIL-DATA           G     00          SB
   3    DETAIL-LINE           G     00          SB 10
    4   LINE-FC               A     01          LF
    4  *SEQ-NBR               N     04          LB
    4  *TIME-SEQ              N     04          LB
    4  *CHECK-ID              N     12          LB
    4  *PAY-SUM-GRP           A     03          LB
    4   PAY-CODE              A     04          LB    HPZ
    4   PCD-DESCRIPTION       A     30          LB
    4   RATE                  S     13        2 LB
    4  *RATE1                 S     13        2 LB
*
  2     PAYMENT-TAB           AF    09          SB
* Database is AL-30, last 8 are reserved
  2     PNP-PAYABLE-TO        AL    30          SB
  2     PNP-FBO-NAME          AL    30          SB
  2     PNP-ADDR1             AL    30          SB    FC1
  2     PNP-ADDR2             AL    30          SB    FC2
  2     PNP-ADDR3             AL    30          SB    FC7
  2     PNP-ADDR4             AL    30          SB    FC8
  2     PNP-CITY              AL    30          SB    FC3
  2     PNP-STATE             A     02          SB    AZ6
  2     PNP-POSTAL-CODE       AL    10          SB
  2     PNP-COUNTRY-CODE      A     02          SB    VA2
  2     PNP-ROLLOVER-ACCT     A     17          SB
  2     PNP-MISCELLANEOUS     A     06          SB
  2     PNP-MISCELLANEOUS2    A     06          SB
  2     PNP-PENS-SEQ-NBR-DISP N     04          SB       
*
  2     DEPOSIT-TAB           AF    16          SB
  2     EAD-EBANK-ID          N     09          SB    DFI
  2     EAD-DESCRIPTION       AL    30          SB    07D
  2     EAD-EBNK-ACCT-NBR     A     17          SB
  2     EAD-ACCOUNT-TYPE      A     01          SB
  2     EAD-ACCOUNT-TYPE      X     30          SB
  2     EAD-CHECK-DESC        AL    08          SB
  2     PNP-WIRE-IND          N     01          SB
  2     PNP-WIRE-IND          X     30          SB
  2     PNP-CA-EFT            A     01          SB
  2     PNP-CA-EFT            X     30          SB
  2     PNP-IAT               A     01          SB
  2     PNP-IAT               X     30          SB
*
  2     RETRO-TAB             AF    08          SB
  2     PNF-DETAIL-DATA2      G     00          SB
   3    PNF-DETAIL-LINE2      G     00          SB 10
    4   PNF-FUNDING-SOURCE2   A     06          SB    fs
    4   PNF-PAYMENT-DESC2     AL    15          SB
    4   PNF-FUND-AMOUNT2      S     12        2 SB
    4   GLM-DESCRIPTION2      A     30          SB
    4  *PFS-DIST-COMPANY2     N     04          SB
    4  *PFS-DST-ACCT-UNIT2    A     15          SB
    4  *PFS-DST-ACCOUNT2      N     06          SB
    4  *PFS-DST-SUB-ACCT2     N     04          SB
*
  2     RETRO-TR-TAB          AF    13          SB
  2    *PT-SEQ-NBR2           N     04          SB
  2    *PT-TIME-SEQ2          N     04          SB
  2    *PT-CHECK-ID2          N     12          SB
  2    *PT-PAY-CODE2          A     04          SB
  2     DETAIL-DATA2          G     00          SB
   3    DETAIL-LINE2          G     00          SB 10
    4   LINE-FC2              A     01          LF
    4  *SEQ-NBR2              N     04          LB
    4  *TIME-SEQ2             N     04          LB
    4  *CHECK-ID2             N     12          LB
    4  *PAY-SUM-GRP2          A     03          LB
    4   PAY-CODE2             A     04          LL    HPZ
    4   PCD-DESCRIPTION2      A     30          LB
    4   RATE2                 S     13        2 LB
    4  *RATE12                S     13        2 LB
    4  *RETRO-PAYMENT2        A     01          LB
  2    *PASS-FC               A     01          SB    HGN
$END-TRANS PR291    S
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     PR291    S
  FC                    I                     I=Inquire
                                              N=Next
                                              P=Previous
                                              A=Add
                                              C=Change
                                              D=Delete
  PNP-PENS-DIST-TYPE                          01=Partial; Hardship
                                              02=Partial; Non-hardship
                                              03=Partial; Rollover to IRA
                                              04=Total; Non-rollover
                                              05=Total Rollover to IRA
                                              06=Total rollover to qual plan
                                              07=Periodic; > 10 years
                                              08=Periodic; < 10 years
                                              09=Periodic; < 10 years; Roll IRA
                                              10=Periodic; Tax not determined
                                              11=Periodic; W-2
                                              12=Lump sum payment; W-2
                                              13=Retro-payment; installment
                                              14=Retro-payment; Rollover to IRA
                                              15=Loan issued
                                              16=Dividend
                                              17=Minimum distribution
                                              18=Excess deferrals
                                              19=Non-spousal death benefit
                                              20=Periodic non-res alien; 1042S
                                              21=Lump sum NRA; 1042S
                                              22=Periodic; 480.6
                                              23=Lump sum; 480.6
                                              24=Retro; W-2
                                              25=Retro-payment; 1099R
                                              26=Retro-payment; 1042S
                                              27=Retro-payment; 480.6
                                              28=Partial (Roll to Qual. Plan)
  PNP-PENS-DIST-CODE                          1=Early distrib; No exception
                                              2=Early distrib; With exception
                                              3=Disability
                                              4=Death benefit
                                              5=Prohibited transaction
                                              6=Section 1035 exchange
                                              7=Normal distribution
                                              8=Excess contrib;Taxable curr yr
                                              9=PS 58 costs
                                              A=Qualified for 5/10yr averaging
                                              B=Designated Roth Acct Distrib
                                              D=Exs cntrb;Txb in pri yr;Pri yr
                                              E=Excess annual addtns; Sec 415
                                              F=Charitable Gift Annuity
                                              G=Direct rollover and contrib
                                              H=R-over to qual plan/tax sh ann
                                              P=Excess contrib;Txbl prior year
                                              L=Loan default
  PNP-PENS-DST-CODE2                          1=Early distrib; No exception
                                              2=Early distrib; With exception
                                              4=Death benefit
                                              8=Excess contrib;Taxable curr yr
                                              A=Qualified for 5/10yr averaging
                                              B=Designated Roth Acct Distrib
                                              D=Exs cntrb;Txb in pri yr;Pri yr
                                              G=Direct rollover and contrib
                                              H=R-over to qual plan/tax sh ann
                                              P=Excess contrib;Txbl prior year
                                              L=Loan default
  PNP-TAX-CATEGORY                            00=Regular
                                              70=Periodic; Normal Tax Tables
                                              71=Non-Periodic; Elig Roll; 20%
                                              72=Non-Periodic; Non-Qual; 10%
  TAX-FREQ-OVER                               1=Weekly
                                              2=Biweekly
                                              3=Semi Monthly
                                              4=Monthly
                                              5=Daily
                                              6=Quarterly
                                              7=Semi Annual
                                              8=Annual
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  EAD-ACCOUNT-TYPE                            C=Checking
                                              S=Savings
  PNP-WIRE-IND                                1=Foreign
                                              2=Domestic
  PNP-CA-EFT                                  Y=Yes
                                              N=No
  PNP-IAT               N                     N=Use Standard ACH Format
                                              Y=Use International ACH Format
  LINE-FC2                                    A=Add
                                              C=Change
                                              D=Delete
$END-EDITS PR291    S
*******************************************************************************
*                            SCREEN   PR29  (2)
*******************************************************************************
$FORM      PR292    S
$WINDOW
$NOTKXFER
$FCPAIRS   I
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    PR292    S
                      ?Roth Initial Contribution Year

|  Company:;    ;|   Employee:;         :                                   ;
                  Year &    ]                            
$END-SCR   PR292    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     PR292    S
  2    *TC                    AL    06          SB
  2    *FC                    A     01          SF
  2    *PT-COMPANY            N     04          SB
  2    *PT-EMPLOYEE           N     09          SB
  2     COMPANY               N     04          SB    01  C
  2     EMPLOYEE              N     09          SB    H07
  2     EMP-FULL-NAME         A     35          SB    HEN
  2     ROTH-YEAR             N     04          SB
  2    *PASS-FC               A     01          SB    HGN
$END-TRANS PR292    S
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     PR292    S
  FC                    I                     I=Inquire
$END-EDITS PR292    S
*

