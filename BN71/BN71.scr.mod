******* BN71.scr 14.1.2 <608437122>
*******************************************************************************
*                            SCREEN   BN71  (1)
*******************************************************************************
$FORM      BN711
$MENU      BNMN7
$SECURITY  NO
$DECIMALS  NO
$KEYFCS    C
$DATAFCS   C
$LDATAFCS  AC
$FCPAIRS   CA,CC,CD,IX,CS
$LSELECT   X
$LINEKN    HTM
$USERXFER
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    BN711
[BN71[ ]           ?*** PARTICIPANT/RETIREE HEALTH ELECTION ***
+      ]  +PT]  +C] +0006]

|                Company:&    ]      ;                              ;
|            Participant:&         ] ;                                        ;
|             As of Date:&      ]                  Go To: Type [  ] Plan [    ]

|FC|*|Tp|Plan|Description       |Start    |Stop  |Option  |Smoker
[ ]; :  :    :               ;  &      ] &      ]  &  ]     [ ]
    |Monthly &        ] |Annual &        ] |Coverage &           ] [Dep ][HIPAA]
[ ]; :  :    :               ;  &      ] &      ]  &  ]     [ ]
    |Monthly &        ] |Annual &        ] |Coverage &           ] [Dep ][HIPAA]
[ ]; :  :    :               ;  &      ] &      ]  &  ]     [ ]
    |Monthly &        ] |Annual &        ] |Coverage &           ] [Dep ][HIPAA]
[ ]; :  :    :               ;  &      ] &      ]  &  ]     [ ]
    |Monthly &        ] |Annual &        ] |Coverage &           ] [Dep ][HIPAA]
[ ]; :  :    :               ;  &      ] &      ]  &  ]     [ ]
    |Monthly &        ] |Annual &        ] |Coverage &           ] [Dep ][HIPAA]
[ ]; :  :    :               ;  &      ] &      ]  &  ]     [ ]
    |Monthly &        ] |Annual &        ] |Coverage &           ] [Dep ][HIPAA]

                             [      ]     [Comments]; ;
$END-SCR   BN711
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     BN711
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2    *FIRST-XMIT            A     01          SB
  2    *PT-PTB-PLAN-TYPE      A     02          SB
  2    *PT-PTB-PLAN-CODE      A     04          SB 
  2    *PT-PTB-START-DATE     F     08          SB 
  2     ORIGINATOR            P     06          RO 
  2    *PT-RECORD-ORIG        A     01          SB 
  2     BCM-CMT-TYPE          A     02          SB   *H20 
  2     PTB-COVER-TYPE        A     01          SB   *Hhu
  2     DETAIL-SIZE           N     04          SB 
  2     PTB-COMPANY           N     04          SK    01  C
  2     PRS-NAME              A     30          SB    CN 
  2     PTB-PARTICIPNT        N     09          SN    HTZ 
  2     PAR-NAME              A     40          SB    HZ0 
  2    *PAR-EMP               N     09          SB    H07 
  2     ENROLLMENT-DATE       F     08          SL    H10 
    2     GO-TO-PLAN-TYPE       A     02          SB
  2     GO-TO-PLAN-CODE       A     04          SB
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 06 
    4  *RECORD-ORIG           A     01          LB 
    4  *PLAN-INFO             A     01          LB 
    4  *PTB-EMPLOYEE          N     09          LB    H07
    4   LINE-FC               A     01          LF    Hhe
    4   HILIGHT               A     01          LB
    4   PTB-PLAN-TYPE         A     02          LB    HTL 
    4   PTB-PLAN-CODE         A     04          LB    HTM 
    4   PLN-DISPLAY-DESC      AL    15          LB    HTN
    4   PTB-START-DATE        F     08          LB    H37 
    4   PTB-STOP-DATE         F     08          LB    H38 
    4   PTB-COV-OPTION        N     02          LB    HAs
    4   PTB-SMOKER            A     01          LB
    4   PTB-PROC-LEVEL        A     05          LB    H03
    4   PTB-PAY-PER-AMT       N     08       02 LB
    4   PTB-ANNUAL-AMT        N     08       02 LB
    4   PTB-COVER-AMT         N     11       02 LB
    4   DEP                   W     04          LB          HR13.4
    4   HIPAA                 W     05          LB          BN32.4
    4  *BNT-CREATE-TRANS      A     01          LB    HBk   HIPAA
    4  *BNT-REASON            A     10          LB    HBi   HIPAA
    4  *BNT-MEMBER-ID         N     01          LB    HBj   HIPAA
    4  *PT-CREATE-TRANS       A     01          LB
    4  *CREATION-DATE         N     08          LB
    4  *UPD-DATE              N     08          LB
    4  *TIME-STAMP            N     06          LB
    4  *USER-ID               A     10          LB
  2     RETRO-BTN             W     06          SB          ZN35.1
  2     COMMENTS              W     08          SB          BN90.1
  2     COMMENTS-FLAG         A     01          SB
$END-TRANS BN711
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     BN711
  FC                    I                     C=Change
                                              I=Inquire
                                              N=Next
                                              P=Previous
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
                                              S=Stop
  PTB-PLAN-TYPE                               HL=Health
                                              DN=Dental
                                              EL=Life
                                              RS=Spending Accounts
  PTB-SMOKER                                  N=No
                                              Y=Yes
$END-EDITS BN711
*
