******* HR13.scr 32.1.5 <1789415557>
$RELEASE   02.1
*
*
*
*******************************************************************************
*                            SCREEN   HR13  (1)
*******************************************************************************
$FORM      HR131    S
$KEYFCS    CD
$DATAFCS   AC
$LREQFCS
$LDATAFCS
$KEYSELECT HR-EMD-S-0069
$FCPAIRS
$LSELECT
$KEYSELECT HR-EMD-S-0069
$DRAW      OPTION  3D,CHISELED
$DRAW      OPTION  LN
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR131    S
[HR13[ ]                          ?Dependents
 +0]  +DP]
|                Company:&    ]       ;                              ;
|               Employee:&         ]  ;                                       ;
|              Dependent:&    ]       ;                              ;
$TAB-REGION
$TAB-FOLDER
$TAB-FORM  MAIN              " Main "
|                   Last Name:[    [                              [    ]
|  First Name, Middle Initial:[               [ ]
|                      Status:[ ]          ;        ;
|               Social Number:[                    ]
|            Employee Address:[ ]          ;            ;
|                Relationship:[          ] ;                              ;
|                     Consent:[ ]          ;              ;
|                  HIC Number:[            ]
$END-TAB   MAIN
$TAB-FORM  ADDRESS           " Address "
  |            Address 1:[                              ]
  |            Address 2:[                              ]
  |            Address 3:[                              ]
  |            Address 4:[                              ]
  |    City or Address 5:[                              ]
  |    State or Province:[  ]
  |          Postal Code:[          ]
  |              Country:[  ]      ;                              ;
  |            Telephone:[      [               ]
  |       Work Telephone:[      [               [     ]
  |                Email:[                                        ]
$END-TAB   ADDRESS
$TAB-FORM  BENEFITS-ANALYSIS " Benefits Analysis "
|            Dependent Type:[ ]          ;                ;
|                 Birthdate:&        ]   |          Age:;   ;
|             Adoption Date:&        ]   |        Placement Date:&        ]
|                    Gender:[ ]       ;      ;   | Marriage Date:&        ]
|                    Smoker:[ ]       ;   ;      |  Divorce Date:&        ]
|                   Student:[ ]       ;        ; |Status Rev Dte:&        ]
|            Effective Date:&        ]           |      Verified:&        ]
|                                          | Create Transactions:[ ]  ;   ;
|Health Insurance Claim Nbr:[           ]        |  HIPAA Reason:[          ]
|                  Disabled:[ ]       ;   ;      | Disabled Date:&        ]
|                  Deceased:[ ]       ;   ;      | Date Of Death:&        ]
|  Disability approved for CDSP:[ ]   ;   ; | Date approved for CDSP:&        ]
|  Disability approved for CHP: [ ]   ;   ; | Date approved for CHP: &        ] 
$END-TAB   BENEFITS-ANALYSIS
$TAB-FORM  COVERAGE          " Coverage "
|       Prior Months Cov:&  ]
| Primary Care Physician:[          ] ;                              ;
|    Established Patient:& ]          ;               ;
|                 Health:[ ]          ;           ;
|                 Dental:[ ]          ;           ;
|                   Life:[ ]          ;           ;
|     Medicare Indicator:[ ]          ;                           ;
                         [ Benefits ]
$END-TAB   COVERAGE
$END-REGION
                         [ Comments ]; ;
$END-SCR   HR131    S
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR131    S
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2    *PT-XMIT-NBR1          N     01          SB
  2    *PT-XMIT-NBR2          N     01          SB
  2    *XMIT-D                N     01          SB
  2    *EFF-DT-XMIT           N     01          SB
  2    *SCREEN-CODE           A     05          SB    D29
  2     PAC-EMP-APP           N     01          SB   *HU4 
  2     PAC-CMT-TYPE          A     02          SB   *H20 
  2     EMD-COMPANY           N     04          SK    01  C
  2     PRS-NAME              A     30          SB    CN 
  2     EMD-EMPLOYEE          N     09          SN    H07 
  2     EMP-FULL-NAME         A     39          SB    HEN 
  2     EMD-SEQ-NBR           N     04          SN    HBV
  2     EMD-SHORT-NAME        A     30          SB    HBW
  2    *EMP-FICA-NBR          A     20          SB
  2    *EMD-USER-ID           AL    10          SB 
*
  2     MAIN-TAB              AF    06          SB
  2     EMD-LAST-NAME-PRE     AL    30          SB
  2     EMD-LAST-NAME         AL    30          SR 
  2     EMD-NAME-SUFFIX       AL    04          SB   *Hql
  2     EMD-FIRST-NAME        AL    15          SR 
  2     EMD-MIDDLE-INIT       A     01          SB 
  2     EMD-ACTIVE-FLAG       A     01          SB 
  2     EMD-ACTIVE-FLAG       X     08          SB 
  2     EMD-FICA-NBR          A     20          SB 
  2     EMD-EMP-ADDRESS       A     01          SR 
  2     EMD-EMP-ADDRESS       X     12          SB 
  2     EMD-REL-CODE          A     10          SR   *HHR 
  2     PCO-DESCRIPTION       A     30          SB   *HHS 
  2     EMD-CONSENT           A     01          SB
  2     EMD-CONSENT           X     14          SB
  2     HIC-NUMBER            A     12          SB
*
  2     ADDRESS-TAB           AF    09          SB
  2     EMD-ADDR1             AL    30          SB
  2     EMD-ADDR2             AL    30          SB
  2     EMD-ADDR3             AL    30          SB
  2     EMD-ADDR4             AL    30          SB
  2     EMD-CITY              AL    30          SB
  2     EMD-STATE             A     02          SB
  2     EMD-ZIP               A     10          SB
  2     EMD-COUNTRY-CODE      A     02          SB   *Hqr
  2     INT-COUNTRY-DESC      AL    30          SB   *Hqs
  2     EMD-HM-PHONE-CNTRY    A     06          SB
  2     EMD-HM-PHONE-NBR      A     15          SB
  2     EMD-WK-PHONE-CNTRY    A     06          SB
  2     EMD-WK-PHONE-NBR      A     15          SB
  2     EMD-WK-PHONE-EXT      A     05          SB
  2     EMD-EMAIL-PERSONAL    AL    60          SB
*
  2     BEN-ANA-TAB           AF    19          SB
  2     EMD-DEP-TYPE          A     01          SR 
  2     EMD-DEP-TYPE          X     16          SB 
  2     EMD-BIRTHDATE         F     08          SB 
  2     EMD-CUR-AGE           N     03          SB
  2     EMD-ADOPTION-DATE     F     08          SB
  2     EMD-PLACEMENT-DATE    F     08          SB  
  2     EMD-SEX               A     01          SR 
  2     EMD-SEX               X     06          SB 
  2     MARRIAGE-DATE         F     08          SB
  2     EMD-SMOKER            A     01          SB
  2     EMD-SMOKER            X     03          SB
  2     DIVORCE-DATE          F     08          SB
  2     EMD-STUDENT           A     01          SR 
  2     EMD-STUDENT           X     08          SB 
  2     STUDENT-DATE          F     08          SB
  2     EFFECT-DATE           F     08          SB
  2     VERIFY-DATE           F     08          SB
  2     CREATE-TRANS          A     01          SB 
  2     CREATE-TRANS          X     03          SB 
  2     EMD-HICN              A     11          SB
  2     HIPAA-REASON          A     10          SB    HBi
  2     EMD-DISABLED          A     01          SR 
  2     EMD-DISABLED          X     03          SB 
  2     DISABLED-DATE         F     08          SB
  2     DECEASED              A     01          SB
  2     DECEASED              X     03          SB
  2     DATE-OF-DEATH         F     08          SB
  2     DIS-APR-CDSP          A     01          SB      
  2     DIS-APR-CDSP          X     03          SB
  2     DIS-APR-CDSP-D        F     08          SB
  2     DIS-APR-CHP           A     01          SB
  2     DIS-APR-CHP           X     03          SB
  2     DIS-APR-CHP-D         F     08          SB
*
  2     COVERAGE-TAB          AF    10          SB
  2     PRIOR-MONTHS-COV      N     02          SB 
  2     EMD-PRIMARY-CARE      A     10          SB   *HFP 
  2     PCO-PC-DESCRIPTION    A     30          SB   *HFQ 
  2     ESTAB-PATIENT         N     01          SB
  2     ESTAB-PATIENT         X     15          SB
  2     EMD-HL-COV-FLAG       A     01          SB 
  2     EMD-HL-COV-FLAG       X     11          SB 
  2     EMD-DN-COV-FLAG       A     01          SB 
  2     EMD-DN-COV-FLAG       X     11          SB 
  2     EMD-DL-COV-FLAG       A     01          SB 
  2     EMD-DL-COV-FLAG       X     21          SB 
  2     EMD-MEDICARE-IND      A     01          SB
  2     EMD-MEDICARE-IND      X     27          SB
  2     BENEFITS              W     10          SB          HR13.2
  2     COMMENTS              W     10          SB          HR90.1
  2     COMMENTS-FLAG         A     01          SB
$END-TRANS HR131    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR131    S
  FC                    I                     A=Add
                                              C=Change
                                              D=Delete
                                              I=Inquire
                                              N=Next
                                              P=Previous
  EMD-ACTIVE-FLAG       A                     A=Active
                                              I=Inactive
  EMD-EMP-ADDRESS       H                     H=Home
                                              S=Supplemental
                                              N=No
  EMD-CONSENT                                  =Not Applicable
                                              N=No
                                              Y=Yes
  EMD-DEP-TYPE                                S=Spouse
                                              P=Domestic Partner
                                              D=Dependent
  EMD-SEX                                     M=Male
                                              F=Female
  EMD-SMOKER            N                     N=No
                                              Y=Yes
  EMD-STUDENT           N                     N=No
                                              Y=Yes
                                              F=Fulltime
                                              P=Parttime
  CREATE-TRANS                                Y=Yes
                                              N=No
  EMD-DISABLED          N                     N=No
                                              Y=Yes
  DECEASED                                     =N/A
                                              N=No
                                              Y=Yes
  DIS-APR-CDSP                                 =N/A
                                              N=No
                                              Y=Yes
                                              R=Under Review
                                              A=Under Appeal
  DIS-APR-CHP                                  =N/A
                                              N=No
                                              Y=Yes
                                              R=Under Review
                                              A=Under Appeal
  ESTAB-PATIENT         0                     0=N/A
                                              1=No
                                              2=Yes
                                              3=Unknown
  EMD-HL-COV-FLAG       N                     N=No Coverage
                                              Y=Coverage
  EMD-DN-COV-FLAG       N                     N=No Coverage
                                              Y=Coverage
  EMD-DL-COV-FLAG       N                     N=No Coverage
                                              Y=Coverage
  EMD-MEDICARE-IND                             =Not Applicable
                                              A=Medicare Part A
                                              B=Medicare Part B
                                              C=Medicare Part A and B
                                              D=Medicare Part Unknown
                                              E=No Medicare
                                              F=Medicare Part A and B and D
                                              G=Medicare Part A and D
                                              H=Medicare Part B and D
                                              I=Medicare Part D
$END-EDITS HR131    S
*******************************************************************************
*                            SCREEN   HR13  (2)
*******************************************************************************
$FORM      HR132    S
$DATAFCS   C
$LDATAFCS  ACD
$FCPAIRS   CA,CC,CD
$NOTKXFER
$WINDOW
$TITLE
$ROW       MAX
$COLM      CENTERED
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR132    S
+HR13.2[ ]                    ?Dependent Benefits
                   +      00000000000000000]
|     Employee:;         ;  ;                                       ;
|    Dependent:     ;    ;  ;                                       ;
                                                    ^Employee^Employee]
|FC|Dep|Start   |Stop |Type|Plan|Description        |  Start |  Stop  ]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
[ ] ; ;&      &        ];  :    :                   :        :        ;[HIPAA]
$END-SCR   HR132    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR132    S
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2     DKEY-PROTECTED        GP    23          SB 
   3    PT-HDB-PLAN-TYPE      A     02          SB 
   3    PT-HDB-PLAN-CODE      A     04          SB 
   3    PT-HDB-EMP-START      F     08          SB 
   3    PT-HDB-START-DATE     F     08          SB 
   3    PT-PLAN-TYPE-SUB      N     01          SB 
  2    *EMD-COMPANY           N     04          SL    01  C
  2    *EMD-EMPLOYEE          N     09          SL    H07 
  2    *EMD-SEQ-NBR           N     04          SL    HBV 
  2    *PT-RECORD-ORIG        A     01          SB 
  2    *SCREEN-CODE           A     05          SB    D29
  2     EMPLOYEE-NBR          N     09          SB    H07 
  2     EMP-FULL-NAME         A     39          SB    HEN 
  2     SEQ-NBR               N     04          SB    HBV
  2     EMD-SHORT-NAME        A     39          SB    HBW
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 10 
    4  *RECORD-ORIG           A     01          LB 
    4  *RECORD-PLAN-TYPE      A     02          LB 
    4   LINE-FC               A     01          LF    Hhe
    4   HILIGHT               A     01          LB 
    4   HDB-START-DATE        F     08          LR 
    4   HDB-STOP-DATE         F     08          LB 
    4   HDB-PLAN-TYPE         A     02          LB 
    4   HDB-PLAN-CODE         A     04          LB 
    4   PLN-DESC              A     19          LB 
    4   BEN-START-DATE        F     08          LB 
    4   BEN-STOP-DATE         F     08          LB 
    4   HIPAA                 W     05          LB          BN32.4
    4  *BNT-CREATE-TRANS      A     01          LB    HBk   HIPAA
    4  *BNT-REASON            A     10          LB    HBi   HIPAA
    4  *BNT-MEMBER-ID         N     01          LB    HBj   HIPAA
    4  *HDB-EMP-START         F     08          LB 
$END-TRANS HR132    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR132    S
  FC                    I                     C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
$END-EDITS HR132    S
*******************************************************************************
*                            SCREEN   HR13  (3)
*******************************************************************************
$FORM      HR133    S
$DATAFCS   C
$LDATAFCS  ACD
$FCPAIRS   CA,CC,CD
$NOTKXFER
$WINDOW
$LINEKN    HBV
$TITLE
$ROW       MAX
$COLM      CENTERED
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR133    S
+HR13.3[ ]                    ?Dependent Benefits
                             +00000000000000000000]
| Plan;                             ;|Start,Stop;        ;;        ;
|FC   |Start |Stop     |Dep |Dependent Name      |Type|Rel Code |Stu|Ds
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
$END-SCR   HR133    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR133    S
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2     DKEY-PROTECTED        GP    20          SB 
   3    PT-HDB-DEPENDENT      N     04          SB 
   3    PT-HDB-EMP-START      F     08          SB 
   3    PT-HDB-START-DATE     F     08          SB 
  2     PLN-DESC              A     30          SB    HTN 
  2     BEN-START             F     08          SB 
  2     BEN-STOP              F     08          SB 
  2    *BEN-COMPANY           N     04          SL    01  C 
  2    *BEN-EMPLOYEE          N     09          SL    H07 
  2    *BEN-PLAN-TYPE         A     02          SL    HTL 
  2    *BEN-PLAN-CODE         A     04          SL    HTM 
  2    *BEN-START-DATE        F     08          SL    H08 
  2    *BEN-STOP-DATE         F     08          SB    GZ3 
  2    *BEN-COV-OPTION        N     01          SB    H82 
  2    *PT-RECORD-ORIG        A     01          SB 
  2    *SCREEN-CODE           A     05          SB    D29
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 10 
    4  *RECORD-ORIG           A     01          LB 
    4   LINE-FC               A     01          LF    Hhe
    4   HILIGHT               A     01          LB 
    4   HDB-START-DATE        F     08          LB 
    4   HDB-STOP-DATE         F     08          LB 
    4   HDB-DEPENDENT         N     04          LB    HBV 
    4   EMD-FULL-NAME         A     21          LB 
    4   EMD-DEP-TYPE          A     01          LB 
    4   EMD-REL-CODE          A     10          LB 
    4   EMD-STUDENT           A     01          LB 
    4   EMD-DISABLED          A     01          LB 
    4   HIPAA                 W     05          LB          BN32.4
    4  *BNT-CREATE-TRANS      A     01          LB    HBk   HIPAA
    4  *BNT-REASON            A     10          LB    HBi   HIPAA
    4  *BNT-MEMBER-ID         N     01          LB    HBj   HIPAA
    4  *HDB-EMP-START         F     08          LB 
    4  *HDB-USER-ID           AL    10          LB
    4  *HDB-PARTICIPNT        N     09          LB
$END-TRANS HR133    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR133    S
  FC                    I                     C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
$END-EDITS HR133    S
*******************************************************************************
*                            SCREEN   HR13  (4)
*******************************************************************************
$FORM      HR134    S
$DATAFCS   C
$LDATAFCS  ACD
$FCPAIRS   CA,CC,CD
$NOTKXFER
$WINDOW
$TITLE
$LINEKN    HBV
$ROW       MAX
$COLM      CENTERED
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR134    S
+HR13.4[ ]                    ?Dependent Benefits
                                      +00000000000000000000]
| Plan;                              ;|Start,Stop;        ;;        ;
|FC   |Start |Stop     |Dep |Dependent Name      |Type|Rel Code |Stu|Ds
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
[ ]; ;&      &        ];    :                     ;; ;;          : ;; ;[HIPAA]
$END-SCR   HR134    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR134    S
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2     DKEY-PROTECTED        GP    20          SB 
   3    PT-HDB-DEPENDENT      N     04          SB 
   3    PT-HDB-EMP-START      F     08          SB 
   3    PT-HDB-START-DATE     F     08          SB 
  2     PLN-DESC              A     30          SB    HTN 
  2     PTB-START             F     08          SB    H37 
  2     PTB-STOP              F     08          SB    H38 
  2    *PTB-COMPANY           N     04          SL    01 C 
  2    *PTB-PARTICIPNT        N     09          SL    HTZ 
  2    *PTB-EMPLOYEE          N     09          SL    H07 
  2    *PTB-PLAN-TYPE         A     02          SL    HTL 
  2    *PTB-PLAN-CODE         A     04          SL    HTO 
  2    *PTB-START-DATE        F     08          SL    H37 
  2    *PTB-STOP-DATE         F     08          SB    H38 
  2    *PTB-COV-OPTION        N     01          SB    H82
  2    *PTB-COVER-TYPE        A     01          SB    Hhu
  2    *PT-RECORD-ORIG        A     01          SB 
  2    *SCREEN-CODE           A     05          SB    D29
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 10 
    4  *RECORD-ORIG           A     01          LB 
    4   LINE-FC               A     01          LF    Hhe
    4   HILIGHT               A     01          LB 
    4   HDB-START-DATE        F     08          LB 
    4   HDB-STOP-DATE         F     08          LB 
    4   HDB-DEPENDENT         N     04          LB    HBV 
    4   EMD-FULL-NAME         A     22          LB 
    4   EMD-DEP-TYPE          A     01          LB 
    4   EMD-REL-CODE          A     10          LB 
    4   EMD-STUDENT           A     01          LB 
    4   EMD-DISABLED          A     01          LB 
    4   HIPAA                 W     05          LB          BN32.4
    4  *BNT-CREATE-TRANS      A     01          LB    HBk   HIPAA
    4  *BNT-REASON            A     10          LB    HBi   HIPAA
    4  *BNT-MEMBER-ID         N     01          LB    HBj   HIPAA
    4  *HDB-EMP-START         F     08          LB 
    4  *HDB-PARTICIPNT        N     09          LB    
$END-TRANS HR134    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR134    S
  FC                    I                     C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
$END-EDITS HR134    S

