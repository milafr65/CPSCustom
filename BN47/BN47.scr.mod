******* BN47.scr 13 <4111473299>
*******************************************************************************
*                            SCREEN   BN47  (1)
*******************************************************************************
$FORM      BN471
$MENU      BNMN3
$SECURITY  NO
$DECIMALS  NO
$KEYFCS    C
$DATAFCS   AC
$LINEDUPS
$LDATAFCS  AC
$FCPAIRS   AA,CA,CC,CD
$LSELECT
$IBCM      A    115
$IBCM      C    116
$IBCM      INP  117
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    BN471
[BN47[ ]                 ?*** BENEFICIARY MAINTENANCE ***
                       +      ]  +0000]  

|                Company:&    ]      ;                              ;
|               Employee:&         ] ;                                        ;
|           Benefit Type:[  ]        ;                              ;
|                   Plan:[    ]      ;                              ;
                          ^Primary or]         ^Percent ]
|FC| Type|                |Contingent|         |Amount  ]          |Amount ]
[ ]  [ ] ;          ;     & ]  ;          ;    [ ] ;       ; &            ]
[ ]  [ ] ;          ;     & ]  ;          ;    [ ] ;       ; &            ]
[ ]  [ ] ;          ;     & ]  ;          ;    [ ] ;       ; &            ]

$TAB-DETAIL
$TAB-FORM  MORE              " More "
 |            Last Name:[    ]  [                             ]  [    ] 
 |           First Name:[              ]|Middle Initial:[ ]
 |         Relationship:[          ]    | Social Number:[                    ]
                                                                 
 |         Trust:[                                                            ]
           
 |      Comments:[                                                            ]
$END-TAB   MORE       
$TAB-FORM  ADDRESS           " Address "

 |      Employee Address:[ ]   ;             ;               
 |             Address 1:[                              ]
 |             Address 2:[                              ]
 |             Address 3:[                              ]
 |             Address 4:[                              ]
 |     City or Address 5:[                              ]   
 |     State or Province:[  ]    Postal Code:[          ]  Country:[  ]
$END-TAB   ADDRESS
$END-DETAIL
$END-SCR   BN471
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     BN471
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2    *WEB-SEQ-NBR           N     04          SB
  2    *WEB-UPDATE-FL         A     01          SB
  2     ORIGINATOR            P     06          RO 
  2     DKEY-PROTECTED        GP    05        
   3    PT-BNF-SEQ-NBR        N     04          SB 
   3    PT-BNF-PRIM-CNTGNT    N     01          SB 
  2     BEN-COMPANY           N     04          SK    01  C
  2     PRS-NAME              A     30          SB    CN 
  2     BEN-EMPLOYEE          N     09          SN    H07 
  2     EMP-NAME              A     40          SB    HEN 
  2     BEN-PLAN-TYPE         A     02          SN    HTL 
  2     BEN-PLAN-TYPE         X     30          SB 
  2     BEN-PLAN-CODE         A     04          SN    HTM 
  2     PLN-DESC              A     30          SB    HTN 
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 03 
    4   LINE-FC               A     01          LF 
    4   BNF-TYPE              A     01          LR
    4   BNF-TYPE              X     10          LB
    4   BNF-PRIM-CNTGNT       N     01          LR 
    4   BNF-PRIM-CNTGNT       X     10          LB 
    4   BNF-PCT-AMT-FLAG      A     01          LB 
    4   BNF-PCT-AMT-FLAG      X     07          LB 
    4   BNF-PMT-AMT           N     12        2 LB 
    4  *BNF-SEQ-NBR           N     04          LB 
    4  *USER-ID               A     10          LB
*
    4  +MORE-TAB              AF    06          LB
    4  +BNF-LAST-NAME-PRE     AL    30          LB
    4  +BNF-LAST-NAME         AL    30          LB 
    4  +BNF-NAME-SUFFIX       AL    04          LB    Hql
    4  +BNF-FIRST-NAME        AL    15          LB 
    4  +BNF-MIDDLE-INIT       A     01          LB 
    4  +BNF-REL-CODE          A     10          LB    HHR
    4  +BNF-FICA-NBR          A     20          LB
    4  +BNF-TRUST             AL    60          LB
    4  +BNF-CMT-TEXT          AL    60          LB
*
    4  +ADDRESS-TAB           AF    09          LB
    4  +EMP-ADDRESS           A     01          LB
    4  +EMP-ADDRESS           X     13          LB
    4  +BNF-ADDR1             AL    30          LB
    4  +BNF-ADDR2             AL    30          LB
    4  +BNF-ADDR3             AL    30          LB
    4  +BNF-ADDR4             AL    30          LB
    4  +BNF-CITY              AL    30          LB
    4  +BNF-STATE             A     02          LB
    4  +BNF-ZIP               A     10          LB
    4  +BNF-COUNTRY-CODE      A     02          LB    VA2
$END-TRANS BN471
*
*        NAME         TYPE  LABEL
* --------------------  -  -------------------------------------------
$LABEL     BN471
  BNF-TYPE                  Type  Descr
  BNF-PCT-AMT-FLAG          Percent Amount   
$END-LABEL     BN471
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     BN471
  FC                    I                     A=Add
                                              C=Change
                                              I=Inquire
                                              N=Next
                                              P=Previous
                                              +=(+)PageDown
                                              -=(-)PageUp
  BEN-PLAN-TYPE                               DB=Defined Benefit
                                              DC=Defined Contribution
                                              DI=Disability
                                              DL=Dependent Life
                                              EL=Employee Life/AD&D
                                              RS=Spending Account
                                              SB=Savings Bond
                                              SP=Stock Purchase
                                              HL=Health
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  BNF-TYPE              0                     0=Individual
                                              1=Trust
  BNF-PRIM-CNTGNT                             1=Primary,2=Contingent
  BNF-PCT-AMT-FLAG                            A=Amount
                                              P=Percent
  EMP-ADDRESS                                 H=Home
                                              S=Supplemental
$END-EDITS BN471
