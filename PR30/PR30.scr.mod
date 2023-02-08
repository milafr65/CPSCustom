******* PR30.scr 23 <3128649233>
$RELEASE   02.1
*
*
*
*******************************************************************************
*                            SCREEN   PR30  (1)
*******************************************************************************
$FORM      PR301    S
$LINEDUPS
$KEYFCS    C
$DATAFCS   AC
$NEXTFCS   NP
$ADDFCS    A
$DELFCS
$LREQFCS   AC
$LDATAFCS  AC
$LADDFCS   A
$FCPAIRS   AA,CA,CC,CD
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    PR301    S
[PR30[ ]                     ?Standard Time Record
                           +0]  +TR]
|          Company:&    ]      ;                              ;
|         Employee:&         ] ;                                        ;
|   Employee Group:[          ];                              ;
|;                 ;;          ; [                 ] [Time Record Comments]; ;
+10]
   ^Pay  ]                                      ^Begin ^End    ^Currency]
|FC|Code |   Hours  |Job Code ]|        Amount  |Date  |Date   |Code    ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
[ ][    ]&         ][         ]&               ]&      &      ][     ]
$TAB-DETAIL
$TAB-FORM  EXPENSES          " Work "
 ^Process ]             ^PR Acct  ] ^Quebec Ent ]
 | Level  |Department   |Nbr Grp  ] |Nbr Grp    ] | Location ]
  [     ]   [     ]      [    ]       [    ]      [          ]
$END-TAB
$TAB-FORM  GL                " General Ledger "
$DRAW      OPTION  LN
$DRAW      GROUP    1, 2, 1, 8
$DRAW      GROUP    1,27, 1, 7
$DRAW      GROUP    1,40, 1, 6
$DRAW      GROUP    1,56, 1, 5
         |Expense Account                    |Activity
&    [               &      &    ]    [               [     ]
$END-TAB
$TAB-FORM  MORE              " More "
| Reason Code:[    ]|         Attendance Code:[  ]|          Occurrence:[ ] 
|    Position:[           ]|           Cycles:[ ][ ][ ][ ][ ][ ][ ][ ][ ]
| Check Group:[ ]|  Process Grp:[ ]|  Tax Frq:[ ]| Daily Timerecord:[ ] ;   ;
$END-TAB
$END-DETAIL
$END-SCR   PR301    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     PR301    S
  2     TC                    AL    06          SB
  2     FC                    A     01          SF
  2    *PT-STM-EFFECT-DATE    F     08          SB
  2    *PT-STM-END-DATE       F     08          SB  
  2    *PT-STM-PAY-CODE       A     04          SB
  2    *PT-STM-SEQ-NBR        N     04          SB
  2     PAC-EMP-APP           N     01          SB   *HU4
  2     PAC-CMT-TYPE          P     02          SB   *H20
  2    *PT-XMIT-NBR-2         N     01          SB
  2     STM-COMPANY           N     04          SK    01  C
  2     PRS-1-NAME            A     30          SB    CN
  2     STM-EMPLOYEE          N     09          SL    H07
  2     EMP-FULL-NAME         A     40          SB    HEN
  2     STM-TIME-GROUP        A     10          SL   *HBX
  2     PRG-DESCRIPTION       A     30          SB   *HBY
  2     AUTO-TR-DESC          A     17          SB                           
  2    *AUTO-TIME-REC         A     01          SB                
  2     AUTO-TIME-REC         X     10          SB                  
  2     EMP-AUTO-TIMREC       W     17          SB          FC='H' 
  2     COMMENTS              W     20          SB          HR90.1
  2     COMMENTS-FLAG         A     01          SB
  2     DETAIL-COUNT          N     02          SB
  2     DETAIL-DATA           G     00          SB
   3    DETAIL-LINE           G     00          SB 10
    4   LINE-FC               A     01          LF
    4   STM-PAY-CODE          A     04          LL   *HPZ
    4   STM-HOURS             S     09        2 LL
    4   STM-JOB-CODE          A     09          LB   *HBP
    4   STM-RATE              S     15        4 LL
    4   STM-EFFECT-DATE       F     08          LB
    4   STM-END-DATE          F     08          LB
    4   STM-CURRENCY-CODE     A     05          LB    98
    4  *STM-CURR-ND           N     01          LB   *98C
    4  *PT-XMIT-NBR           N     01          LB
    4  +WORK-TAB              AF    06          LB
    4  +STM-PROCESS-LEVEL     A     05          LB    H03
    4  +STM-DEPARTMENT        A     05          LB    HH5
    4  +STM-BUS-NBR-GRP       A     04          LB    HZw
    4  +STM-QC-ENT-NBR-GRP    A     04          LB    HZu
    4  +STM-LOCAT-CODE        A     10          LB    HHN
    4  +GL-TAB                AF    16          LB
    4  +STM-DIST-COMPANY      N     04          LB    HEO
    4  +STM-DST-ACCT-UNIT     A     15          LB    02P
    4  +STM-DST-ACCOUNT       N     06          LB    04P
    4  +STM-DST-SUB-ACCT      N     04          LB    05P
    4  +STM-ACTIVITY          A     15          LB    11
    4  +STM-ACCT-CATEGORY     A     05          LB    24A
    4  +MORE-TAB              AF    06          LB
    4  +STM-REASON-CODE       A     04          LL    hUE      
    4  +STM-ATTEND-CODE       A     02          LL   *HZF
    4  +STM-OCCURRENCE        A     01          LB
    4  +STM-POSITION          A     12          LB    HP3
    4  +STM-DED-CYCLE1        A     01          LB
    4  +STM-DED-CYCLE2        A     01          LB
    4  +STM-DED-CYCLE3        A     01          LB
    4  +STM-DED-CYCLE4        A     01          LB
    4  +STM-DED-CYCLE5        A     01          LB
    4  +STM-DED-CYCLE6        A     01          LB
    4  +STM-DED-CYCLE7        A     01          LB
    4  +STM-DED-CYCLE8        A     01          LB
    4  +STM-DED-CYCLE9        A     01          LB
    4  +STM-CHECK-GRP         A     01          LB
    4  +STM-PROCESS-GRP       A     01          LB
    4  +STM-TAX-FREQ-OVER     N     01          LB
    4  +STM-DAILY-TR-FLG      A     01          LB
    4  +STM-DAILY-TR-FLG      X     03          LB
    4  *STM-SEQ-NBR           N     04          LB
    4  *STM-PENS-SEQ-NBR      N     04          LB
    4  *CYCLE-CHANGE          N     01          LB
$END-TRANS PR301    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     PR301    S
  FC                    I                     A=Add
                                              C=Change
                                              I=Inquire
                                              N=Next
                                              P=Previous
                                              +=(+)PageDown
                                              -=(-)PageUp
  AUTO-TIME-REC                               N=No 
                                              Y=Yes
                                              S=Standard
                                              T=Time Group
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  STM-OCCURRENCE                               =Not Assigned
                                              N=No
                                              Y=Yes
  STM-DED-CYCLE1                              X
  STM-DED-CYCLE2                              X
  STM-DED-CYCLE3                              X
  STM-DED-CYCLE4                              X
  STM-DED-CYCLE5                              X
  STM-DED-CYCLE6                              X
  STM-DED-CYCLE7                              X
  STM-DED-CYCLE8                              X
  STM-DED-CYCLE9                              X
  STM-TAX-FREQ-OVER                           1=Weekly
                                              2=Bi Weekly
                                              3=Semi Monthly
                                              4=Monthly
                                              5=Daily
                                              6=Quarterly
                                              7=Semi Annual
                                              8=Annual
  STM-DAILY-TR-FLG      N                     N=No
                                              Y=Yes
$END-EDITS PR301    S
*******************************************************************************
*                            SCREEN   PR30  (2)
*******************************************************************************
$FORM      PR302    S
$KEYFCS    C
$DATAFCS   C
$NOTKXFER    
$WINDOW
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    PR302    S
[PR30[ ]                    ?Employee Automatic Time Record
                   +      ]

 |             Company: ;    ;      ;                              ;
 |            Employee: ;         ; ;                                        ;
 | Automatic Time Record: [ ]       ;          ;
 |
$END-SCR   PR302    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     PR302    S
  2     TC                    AL    06          SB
  2     FC                    A     01          SF
  2     ORIGINATOR            P     06          RO
  2     STM-COMPANY           N     04          SB    01  C
  2     PRS-NAME              AL    30          SB    CN
  2     STM-EMPLOYEE          N     09          SB    H07
  2     EMP-FULL-NAME         AL    40          SB    HEN
  2     STM-AUTO-TIMEREC      A     01          SB    PAF
  2     STM-AUTO-TIMEREC      X     10          SB
$END-TRANS PR302    S
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     PR302    S
  FC                    I                     C=Change
                                              I=Inquire
  STM-AUTO-TIMEREC                            N=No      
                                              Y=Yes        
                                              S=Standard
                                              T=Time Group
$END-EDITS PR302    S
