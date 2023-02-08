******* BN245.scr 4 <1818211129>
$RELEASE   02.1
*******************************************************************************
*                           SCREEN   BN245
*******************************************************************************
$FORM      BN245    R
$MENU      BNMN0
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
$SCREEN    BN245    R
[BNRP[ ]               ?*** Future BN245 CSV Program ***           +N]
                             ?;                                        ;
   Report          [BN245]     Priority    [ ]     Save Params (Y/N) [ ]
   User Name  [          ]
   Job Name   [          ] --- This will be the name on your report
   Run After  [          ] --- NOW, HOLD, Time (1230 pm), Job Name
   Frequency           [ ] --- (D)aily, (W)eekly, (M)onthly, (O)n Demand
\  Comments   [                                                  ]\
|                  Company:&    ]       ;                              ;
|                Flex Plan:[    ]
|             Benefit Type:[  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
|            Benefits Date:&      ]|  Election Date:&      ]|  Age Date:&      ]
|             Contribution:[ ]          ;          ;
|         Processing Group:[          ]           
|            Process Level:[     ]      ;                              ;
|           Employee Group:[          ] ;                              ;
|                 Employee:&         &         &         &         &         ]
                           &         &         &         &         &         ]
|               Dependents:[ ]          ;   ;
|       Dependent Benefits:[ ]          ;   ;
|            Beneficiaries:[ ]          ;   ;
|      Investment Accounts:[ ]          ;   ;
|            Report Option:[ ]          ;                   ;
|       Include Social Nbr:[ ]          ;   ;
$END-SCR   BN245    R
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     BN245    R
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2     UPDATE-SW             A     01          SB 
  2     SCREEN-MSG            O     40          SB 
  2     TRANCODE              A     05          RP 
  2     HEADER                G     00        
   3    Q-PRIORITY            N     01          SB 
   3    SAVE-PARAMS           A     01          SB 
   3    USER-NAME             A     10          SK 
   3    JOB-NAME              A     10          SK 
   3    RUN-AFTER             A     10          SB 
   3    FREQUENCY             A     01          SB 
   3    COMMENTS              A     50          SB 
  2     RUN-PARAMS            G     00        
   3    COMPANY               N     04          SR    01  C
   3    PRS-NAME              A     30          SB    CN
   3    FLEX-PLAN             A     04          SB    HTA
   3    PLAN-TYPE-GRP         G                 SB 11
    4   PLAN-TYPE             A     02          SB    HTL
   3    BEN-DATE              F     08          SB
   3    ELEC-DATE             F     08          SB
   3    AGE-DATE              F     08          SB
   3    CONTRIB               A     01          SR 
   3    CONTRIB               X     10          SB 
   3    PROC-GROUP            A     10          SB    H09
   3    PROC-LEVEL            A     05          SB    H03 P
   3    PROC-LEVEL-NAME       AL    30          SB    HH4
   3    GROUP-NAME            A     10          SB    HBX
   3    PRG-DESCRIPTION       AL    30          SB    HBY
   3    EMPLOYEE-GRP          G                 SB 10
    4   EMPLOYEE              N     09          SB
   3    DEPENDENT             A     01          SB 
   3    DEPENDENT             X     03          SB 
   3    DEPENDENT-BEN         A     01          SB 
   3    DEPENDENT-BEN         X     03          SB 
   3    BENEFIC               A     01          SB 
   3    BENEFIC               X     03          SB 
   3    INVEST-ACC            A     01          SB 
   3    INVEST-ACC            X     03          SB 
   3    REPORT-OPT            A     01          SR 
   3    REPORT-OPT            X     19          SB 
   3    INC-FICA-NBR          A     01          SB
   3    INC-FICA-NBR          X     03          SB
$END-TRANS BN245    R
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  -------------------------
$EDITS     BN245    R
  FC                                          A,C,D,I
  Q-PRIORITY            2                     1,2,3
  SAVE-PARAMS           N                     Y,N
  JOB-NAME                                    R.00000000:R.ZZZZZZZZ
  RUN-AFTER             HOLD
  FREQUENCY             O                     D,W,M,O
  PLAN-TYPE                                   DB=Defined Benefit
                                              DC=Defined Contribution
                                              DI=Disability
                                              DL=Dependent Life
                                              DN=Dental
                                              EL=Employee Life
                                              HL=Health
                                              RS=Spending Account
                                              SB=Savings Bonds
                                              SP=Stock Purchase
                                              VA=Vacation
  CONTRIB                                     A=Annual
                                              M=Monthly
                                              P=Pay Period
  DEPENDENT             N                     N=No    
                                              Y=Yes     
  DEPENDENT-BEN         N                     N=No    
                                              Y=Yes     
  BENEFIC               N                     N=No    
                                              Y=Yes     
  INVEST-ACC            N                     N=No     
                                              Y=Yes     
  REPORT-OPT                                  R=Report Only
                                              C=CSV Only   
                                              B=Both Report and CSV
  INC-FICA-NBR                                N=No
                                              Y=Yes
$END-EDITS BN245    R

