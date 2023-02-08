******* BN72.scr 14.1.2 <2619477278>
********************************************************************************
*                            SCREEN   BN72  (1)
*******************************************************************************
$FORM      BN721    S
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
$SCREEN    BN721     
[BN72[ ]           ?*** PARTICIPANT/RETIREE HEALTH ELECTION ***
+      ]  +PT]  +R]  +0012]
|                Company:&    ]      ;                              ;
|                Retiree:&         ] ;                                        ;
|             As of Date:&      ]                  Go To: Type [  ] Plan [    ]

|FC |*|Tp|Plan|Description     |Start   |Stop  |Opt|SM   |Coverage |Dep]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
[ ] ; :  :    :               ;&      ]&      ]&  ][ ]&           ][Dep ][HIPAA]
$TAB-DETAIL
$TAB-FORM  INFO              " Additional Info " 
|Process Level
    [     ]
$END-TAB
$END-DETAIL
                             [      ]     [ Comments ]; ;
$END-SCR   BN721     
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- ------------ -------- --  ----        - -- -- --- - -----------------------
$TRANS     BN721     
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
  2     PTB-EMPLOYEE          N     09          SN    H07 
  2     EMP-NAME              A     40          SB    HEN 
  2     ENROLLMENT-DATE       F     08          SL    H10 
  2     GO-TO-PLAN-TYPE       A     02          SB
  2     GO-TO-PLAN-CODE       A     04          SB
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 12 
    4   LINE-FC               A     01          LF    Hhe
    4  *RECORD-ORIG           A     01          LB 
    4  *PLAN-INFO             A     01          LB 
    4   HILIGHT               A     01          LB
    4   PTB-PLAN-TYPE         A     02          LB    HTL 
    4   PTB-PLAN-CODE         A     04          LB    HTM 
    4   PLN-DISPLAY-DESC      AL    15          LB    HTN
    4   PTB-START-DATE        F     08          LB    H37 
    4   PTB-STOP-DATE         F     08          LB    H38 
    4   PTB-COV-OPTION        N     02          LB    HAs
    4   PTB-SMOKER            A     01          LB
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
*
    4  +INFO                  AF    17          LB
    4  +PTB-PROC-LEVEL        A     05          LB    H03 P
  2     RETRO-BTN             W     06          SB          ZN35.1
  2     COMMENTS              W     10          SB          BN90.1
  2     COMMENTS-FLAG         A     01          SB
$END-TRANS BN721    S
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     BN721    S
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
                                              DL=Dependent Life
  PTB-SMOKER                                  N=No
                                              Y=Yes
$END-EDITS BN721    S
*
