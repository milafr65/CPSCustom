******* PR134.scr 12 <2561846074>
*******************************************************************************
*                           SCREEN   PR134
*******************************************************************************
$FORM      PR134    R
$NOLFCOK
$KEYFCS    CD
$DATAFCS   AC
$LDATAFCS
$FCPAIRS
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    PR134    R
[PRRP[ ]                    ?Automatic Time Records      +Y]
                             ?;                                        ;
  |          Report:[PR134]    |   Priority:[ ]    |Save Params (Y/N):[ ]
  |  User Name:[          ]
  |   Job Name:[          ] --- This will be the name on your report
  |  Run After:[          ] --- NOW, HOLD, Time (1230 pm), Job Name
  |           Frequency:[ ] --- (D)aily, (W)eekly, (M)onthly, (O)n Demand
\ |   Comments:[                                                  ]\
$TAB-REGION
$TAB-FOLDER
$TAB-FORM  SELECTION         " Selection "
 |         Company:&    ] ;                              ;
 |Time Record Type:[ ]    ;                          ; Batch:&      ]
 |Time Record Date:&      ]                  Deduction Cycle:[ ]
 |Processing Group:[          ]                Pay Frequency:[ ] ;            ;
 |   Process Level:[     ] |    Department:[     ] |      Flex Date:&      ]
 | Mid Period Hire:[ ] ;                      ;
 |Exception Report:[ ] ;                   ;
 |  Employee Group:[          [          [          [          [          ]
                   [          [          [          [          [          ]
    |     Employee:&         ]&         ]&         ]&         ]&         ]
                   &         ]&         ]&         ]&         ]&         ]
                   &         ]&         ]&         ]&         ]&         ]
$END-TAB   SELECTION
$TAB-FORM  ADDL-INFO         " Additional Information "
    |               Check Group:[ ]    |
    |             Process Group:[ ]
 ^Att  ^Reason ]     ^Pay       ]        ^Check ^Proc ]              ^Account ]
 |Code |Code   |Ocr  |Code      ]|Hours] |Group |Group|Activity      |Category]
 [  ]  [    ]  [ ]   [    ]  &         ] [ ]    [ ]   [               [     ]
 [  ]  [    ]  [ ]   [    ]  &         ] [ ]    [ ]   [               [     ]
 [  ]  [    ]  [ ]   [    ]  &         ] [ ]    [ ]   [               [     ]
 [  ]  [    ]  [ ]   [    ]  &         ] [ ]    [ ]   [               [     ]
 [  ]  [    ]  [ ]   [    ]  &         ] [ ]    [ ]   [               [     ]

  |  Employee Sequence:[ ]       ;          ;
  |           Comments:[ ]       ;          ;
$END-TAB   ADDL-INFO
$TAB-FORM  SPECIAL           " Special Run "
  |   Special Check Run:[ ]
  |                     
  | Bypass Check Groups:[ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ]
$END-TAB   SPECIAL
$END-REGION
$END-SCR   PR134    R
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     PR134    R
  2     TC                    AL    06          SB
  2     FC                    A     01          SF
  2     UPDATE-SW             P     01          SR
  2     SCREEN-MSG            O     40          SB
  2     TRANCODE              A     05          RP
  2     HEADER                G     00
   3    Q-PRIORITY            N     01          SR
   3    SAVE-PARAMS           A     01          SR
   3    USER-NAME             A     10          SK
   3    JOB-NAME              A     10          SK
   3    RUN-AFTER             A     10          SR
   3    FREQUENCY             A     01          SR
   3    COMMENTS              A     50          SB
  2     RUN-PARAMS            G     00          SB
*
   3    SELECTION-TAB         AF    11          SB
   3    COMPANY               N     04          SR    01  C
   3    PRS-NAME              A     30          SB    CN
   3    SELECTION             A     01          SR
   3    SELECTION             X     26          SB
   3    BATCH-NBR             N     06          SB   *HZN
   3    TR-DATE               F     08          SR
   3    DED-CYCLE             N     01          SR
   3    PROC-GROUP            A     10          SB   *H09
   3    PAY-FREQUENCY         N     01          SB
   3    PAY-FREQUENCY         X     12          SB
   3    PROCESS-LEVEL         A     05          SB   *H03 P
   3    DEPARTMENT            A     05          SB   *HH5
   3    FLEX-DATE             F     08          SB
   3    MID-PERIOD            N     01          SB
   3    MID-PERIOD            X     22          SB
   3    EXCEPTION-REPORT      N     01          SB
   3    EXCEPTION-REPORT      X     19          SB
   3    PERS-GROUPS           G     00          SB 10
    4   GROUP-NAME            A     10          SB   *HBX
   3    EMP-GROUPS            G     00          SB 15
    4   EMPLOYEE              N     09          SB   *H07
*
   3    ADDL-INFO-TAB         AF    24          SB
   3    CHECK-GRP             A     01          SB
   3    PROCESS-GRP           A     01          SB
   3    PAYCODE-INFO          G     00          SB 05
    4   ATTEND-CODE           A     02          SB   *HZF
    4   REASON-CODE           A     04          SB   *hUE    
    4   OCCURRENCE            A     01          SB
    4   PAY-CODE              A     04          SB   *hPz
    4   HOURS                 S     09        2 SB
    4   PC-CHECK-GRP          A     01          SB
    4   PC-PROCESS-GRP        A     01          SB
    4   ACTIVITY              A     15          SB   *11
    4   ACCT-CATEGORY         A     05          SB   *24A
   3    EMPLOYEE-SEQ          A     01          SB
   3    EMPLOYEE-SEQ          X     10          SB
   3    PRINT-COMMENTS        A     01          SB
   3    PRINT-COMMENTS        X     10          SB
*
   3    SPECIAL-RUN-TAB       AF    13          SB
   3    SPECIAL-RUN           A     01          SB
   3    BYP-INFORMATION       G     00          SB 09
    4   CHECK-GROUP           A     01          SB   *HZF
$END-TRANS PR134    R
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  -------------------------
$EDITS     PR134    R
  FC                                          A,C,D,I,F,R
  Q-PRIORITY            2                     1,2,3
  SAVE-PARAMS           N                     Y,N
  RUN-AFTER             HOLD
  FREQUENCY             O                     D,W,M,O
  SELECTION                                   S=Standard
                                              T=Employee Group
                                              F=Flex Benefit
                                              P=Override Pay Codes
                                              G=Standard and Emp Group
                                              B=Standard and Flex
                                              A=Stand  Emp Grp  Flex
                                              D=All Standard and Emp Group
                                              E=All Stand Emp Grp Flex
  PAY-FREQUENCY                               1=Weekly
                                              2=Bi-weekly
                                              3=Semi-Monthly
                                              4=Monthly
                                              5=Four-Weekly
  MID-PERIOD                                  0=Include All
                                              1=Exclude Hire Date
                                              2=Exclude Adj. Hire Date
  EXCEPTION-REPORT                            0=No Exception Report
                                              1=All
                                              2=Exclude Future Hire
  OCCURRENCE                                  Y=Yes
                                              N=No
  EMPLOYEE-SEQ                                A=Alpha,N=Numeric
  PRINT-COMMENTS        Y                     Y=Yes,N=No
  SPECIAL-RUN           N                     Y=Yes,N=No
  CHECK-GROUP                                 1,2,3,4,5,6,7,8,9
$END-EDITS PR134    R
