******* BN247.scr 5 <2053291433>
*******************************************************************************
*                           SCREEN   BN247
*******************************************************************************
$FORM      BN247    R
$MENU      BNMNB
$SECURITY  NO
$DECIMALS  NO
$NOLFCOK
$KEYFCS    CD
$DATAFCS   AC
$LDATAFCS
$FCPAIRS
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    BN247    R
[BNRP[ ]                  ?    Beneficiary report                      +N]
                             ?;                                        ;
  |          Report:[BN247]    |   Priority:[ ]    |Save Params (Y/N):[ ]
  |  User Name:[          ]
  |   Job Name:[          ] --- This will be the name on your report
  |  Run After:[          ] --- NOW, HOLD, Time (1230 pm), Job Name
  |           Frequency:[ ] --- (D)aily, (W)eekly, (M)onthly, (O)n Demand
\ |   Comments:[                                                  ]\

  |                Company:&    ]  ;                              ;
  |          Report Option:[ ]     ;                         ;
  |          1 - Employees:&         &         &         &         &         ]
  |     3 - Employee Group:[          ] ;                              ;

  |           Benefit Type:[  ] [  ] [  ] [  ] [  ] [  ] [  ] [  ]

    |                 Plan:[    [    [    [    [    [    [    [    ]

    |    Employee Sequence:[ ]     ;                              ;
    |               Detail:[ ]     ;   ;

$END-SCR   BN247    R
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     BN247    R
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2     UPDATE-SW             P     01          SB 
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
  2     RUN-PARAMS            G     00        
   3    COMPANY               N     04          SR    01  C
   3    PRS-NAME              A     30          SB    CN 
   3    RUN-OPTION            N     01          SR
   3    RUN-OPTION            X     30          SB
   3    EMPLOYEE-GRP          G                 SB 05
    4   EMPLOYEE              N     09          SB   *H07
   3    GROUP-NAME            A     10          SB   *HBX
   3    GROUP-NAME            X     30          SB 
   3    PLAN-TYPE-GRP         G                 SB 08
    4   PLAN-TYPE             A     02          SB
   3    PLAN-CODE-GRP         G                 SB 08 
    4   PLAN-CODE             A     04          SB   *HZW 
   3    EMPLOYEE-SEQ          A     01          SB 
   3    EMPLOYEE-SEQ          X     30          SB 
   3    DETAIL                A     01          SB
   3    DETAIL                X     03          SB
$END-TRANS BN247    R
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  -------------------------
$EDITS     BN247    R
  FC                                          A,C,D,I
  Q-PRIORITY            2                     1,2,3
  SAVE-PARAMS           N                     Y,N
  RUN-AFTER             HOLD
  FREQUENCY             O                     D,W,M,O
  RUN-OPTION                                  1=Specified Employees
                                              2=All Employees
                                              3=Specified Employee Group
  PLAN-TYPE                                   DB=Defined Benefit
                                              DC=Defined Contribution
                                              DI=Disability
                                              DL=Dependent Life
                                              EL=Employee Life/AD&D
                                              RS=Spending Account
                                              SB=Savings Bond
                                              SP=Stock Purchase
  EMPLOYEE-SEQ                                A=Alpha
                                              N=Numeric
  DETAIL                N                     N=No
                                              Y=Yes
$END-EDITS BN247    R
