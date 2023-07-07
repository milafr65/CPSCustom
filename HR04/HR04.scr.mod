******* HR04.scr 32 <3030309995>
$RELEASE   02.1
*
*
*
*******************************************************************************
*                            SCREEN   HR04  (1)
*******************************************************************************
$FORM      HR041    S
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR041    S
                             ?Code Setup


                           [ Human Resources Codes ]

                           [   Alpha User Fields   ]

                           [  Numeric User Fields  ]

                           [    Date User Fields   ]


$END-SCR   HR041    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR041    S
  2    *TC                    A     06          SR
  2    *FC                    A     01          SF
  2     HR-CODES              W     23          SB          HR04.2
  2     TYPE-A                W     23          SB          HR04.4
  2     TYPE-N                W     23          SB          HR04.5
  2     TYPE-D                W     23          SB          HR04.6
$END-TRANS HR041    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR041    S
$END-EDITS HR041    S
*******************************************************************************
*                            SCREEN   HR04  (2)
*******************************************************************************
$FORM      HR042    S
$KEYFCS    C
$DATAFCS   AC
$LREQFCS   AC
$LDATAFCS  AC
$FCPAIRS   AA,CA,CC,CD
$LSELECT
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR042    S
[ ]                ?    HR Codes

|      Type:[  ] ;                              ;

|FC |Code        |Description                      |Status
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
[ ] [          ] [                              ]  [ ]
$END-SCR   HR042    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR042    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2     PCO-TYPE              A     02          SN    Jpo
  2     PCS-DESCRIPTION       A     32          SB    Jpp
  2    *PT-PCO-CODE           A     10          SB
  2    *AR-TYPE               A     02          SB
  2    *HS-TYPE               A     02          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 10
    4   LINE-FC               A     01          LF
    4   PCO-CODE              A     10          LK
    4   PCO-DESCRIPTION       AL    30          LB
    4   PCO-ACTIVE-FLAG       A     01          LB
$END-TRANS HR042    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR042    S
  FC                    I                     A=Add
                                              C=Change
                                              I=Inquire
                                              N=Next
                                              P=Previous
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  PCO-ACTIVE-FLAG       A                     A=Active,I=Inactive
$END-EDITS HR042    S
*******************************************************************************
*                            SCREEN   HR04  (3)
*******************************************************************************
$FORM      HR043    S
$KEYFCS    C
$DATAFCS   AC
$LREQFCS   AC
$LDATAFCS  AC
$FCPAIRS   AA,CA,CC,CD
$LSELECT
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR043    S
[ ]                ?    HR Codes

|      Type:[  ] ;                              ;

|FC |Code        |Description                      |Level   |Status
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
[ ] [          ] [                              ]  &   ]    [ ]
$END-SCR   HR043    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR043    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2     PCO-TYPE              A     02          SN    Jpo
  2     PCS-DESCRIPTION       A     30          SB    Jpp
  2    *PT-PCO-CODE           A     10          SB
  2    *AR-TYPE               A     02          SB
  2    *HS-TYPE               A     02          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 10
    4   LINE-FC               A     01          LF
    4   PCO-CODE              A     10          LK
    4   PCO-DESCRIPTION       AL    30          LB
    4   PCO-EDUC-LEVEL        N     03          LB
    4   PCO-ACTIVE-FLAG       A     01          LB
$END-TRANS HR043    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR043    S
  FC                                          A=Add
                                              C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  PCO-TYPE                                    PR=Performance Rating
                                              PF=Proficiency Level
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  PCO-ACTIVE-FLAG       A                     A=Active,I=Inactive
$END-EDITS HR043    S
*******************************************************************************
*                            SCREEN   HR04  (4)
*******************************************************************************
$FORM      HR044    S
$KEYFCS    C
$DATAFCS   AC
$NEXTFCS
$ADDFCS    A
$DELFCS
$LREQFCS   X
$LDATAFCS  AC
$LADDFCS   A
$FCPAIRS   AA,CA,CD,CC
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR044    S
[ ]                 ?Alphanumeric User Fields
+A]
                                                  [ Required ]
                               |      Position To:[                    ]

                                ^Value]             
|FC|Field Name          |Use|Sts|List |Action|Required|History|Security
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]
[ ][                    ][ ] [ ] [ ]     [ ]    ; ;      [ ]      [ ] [      ]

$END-SCR   HR044    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR044    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2     HRU-FIELD-TYPE        A     01          SB   *WFT
  2     COUNTRY-REQ           W     10          SB          FC='R'
  2     PT-HRU-FIELD-NAME     AL    20          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 12
    4   LINE-FC               A     01          LF
    4   HRU-FIELD-NAME        AL    20          LK   *HEY
    4   HRU-INDICATOR         A     01          LB
    4   HRU-ACTIVE-FLAG       A     01          LB
    4   HRU-REQ-VALUE         A     01          LB
    4   HRU-PERS-ACTION       A     01          LB
    4   REQ-FLAG              A     01          LB
    4   HRU-LOG-FLAG          A     01          LB
    4   HRU-SEC-LEVEL         N     01          LB
    4   VALUE-LIST            W     06          LB          HR04.7
    4  *HRU-FIELD-KEY         A     02          LB    HZS
$END-TRANS HR044    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR044    S
  FC                                          A=Add
                                              C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  HRU-INDICATOR         B                     E=Employee
                                              A=Applicant
                                              B=Both
  HRU-ACTIVE-FLAG       A                     A=Active
                                              I=Inactive
  HRU-REQ-VALUE         N                     Y=Yes
                                              N=No
  HRU-PERS-ACTION                             X=Personnel Action Required
  HRU-LOG-FLAG                                X=Log Changes
  HRU-SEC-LEVEL         9                     1:9
$END-EDITS HR044    S
*******************************************************************************
*                            SCREEN   HR04  (5)
*******************************************************************************
$FORM      HR045    S
$KEYFCS    C
$DATAFCS   AC
$NEXTFCS
$ADDFCS    A
$DELFCS
$LREQFCS   X
$LDATAFCS  AC
$LADDFCS   A
$FCPAIRS   AA,CA,CC,CD
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR045    S
[ ]                 ?Numeric User Fields
+N]
                                                      [ Required ]
                                  |       Position To:[                    ]

|FC|Field Name         |Use|Sts|Beg Value   | End Value    |Cur|Act|Rq|Hst|Sec
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]
[ ][                    [ ][ ]&             ]-&             [ ] [ ]; ; [ ] [ ]

$END-SCR   HR045    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR045    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2     HRU-FIELD-TYPE        A     01          SB   *WFT
  2     COUNTRY-REQ           W     10          SB          FC='R'
  2     PT-HRU-FIELD-NAME     AL    20          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 12
    4   LINE-FC               A     01          LF
    4   HRU-FIELD-NAME        AL    20          LK   *HEY
    4   HRU-INDICATOR         A     01          LB
    4   HRU-ACTIVE-FLAG       A     01          LB
    4   HRU-BEG-NUMBER        S     13        2 LB
    4   HRU-END-NUMBER        S     13        2 LB
    4   HRU-CURRENCY-FLAG     A     01          LB
    4   HRU-PERS-ACTION       A     01          LB
    4   REQ-FLAG              A     01          LB
    4   HRU-LOG-FLAG          A     01          LB
    4   HRU-SEC-LEVEL         N     01          LB
    4  *HRU-FIELD-KEY         A     02          LB    HZS
$END-TRANS HR045    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR045    S
  FC                                          A=Add
                                              C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  HRU-INDICATOR         B                     E=Employee
                                              A=Applicant
                                              B=Both
  HRU-ACTIVE-FLAG       A                     A=Active
                                              I=Inactive
  HRU-CURRENCY-FLAG     N                     N=Not Currency
                                              Y=Currency Formatting
  HRU-PERS-ACTION                             X=Personnel Action Required
  HRU-LOG-FLAG                                X=Log Changes
  HRU-SEC-LEVEL         9                     1:9
$END-EDITS HR045    S
*******************************************************************************
*                            SCREEN   HR04  (6)
*******************************************************************************
$FORM      HR046    S
$KEYFCS    C
$DATAFCS   AC
$NEXTFCS
$ADDFCS    A
$DELFCS
$LREQFCS   X
$LDATAFCS  AC
$LADDFCS   A
$FCPAIRS   AA,CA,CC,CD
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR046    S
[ ]                 ?Date User Fields
+D]
                                                 [ Required ]
                             |       Position To:[                    ]

|FC |Field Name            |Use|Sts|Begin     |End     |Action|Reqd |Hist|Sec
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]
[ ] [                    ] [ ] [ ] &        ]-&        ] [ ]    ; ;  [ ]  [ ]

$END-SCR   HR046    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR046    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2     HRU-FIELD-TYPE        A     01          SB   *WFT
  2     COUNTRY-REQ           W     10          SB          FC='R'
  2     PT-HRU-FIELD-NAME     AL    20          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 12
    4   LINE-FC               A     01          LF
    4   HRU-FIELD-NAME        AL    20          LK   *HEY
    4   HRU-INDICATOR         A     01          LB
    4   HRU-ACTIVE-FLAG       A     01          LB
    4   HRU-BEG-DATE          F     08          LB
    4   HRU-END-DATE          F     08          LB
    4   HRU-PERS-ACTION       A     01          LB
    4   REQ-FLAG              A     01          LB
    4   HRU-LOG-FLAG          A     01          LB
    4   HRU-SEC-LEVEL         N     01          LB
    4  *HRU-FIELD-KEY         A     02          LB    HZS
$END-TRANS HR046    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR046    S
  FC                                          A=Add
                                              C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  HRU-INDICATOR         B                     E=Employee
                                              A=Applicant
                                              B=Both
  HRU-ACTIVE-FLAG       A                     A=Active
                                              I=Inactive
  HRU-PERS-ACTION                             X=Personnel Action Required
  HRU-LOG-FLAG                                X=Log Changes
  HRU-SEC-LEVEL         9                     1:9
$END-EDITS HR046    S
*******************************************************************************
*                            SCREEN   HR04  (7)
*******************************************************************************
$FORM      HR047    S
$KEYFCS    C
$DATAFCS   AC
$NEXTFCS
$ADDFCS    A
$DELFCS
$LREQFCS   AC
$LDATAFCS  AC
$LADDFCS   A
$FCPAIRS   AA,CA,CC,CD
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR047    S
[ ]                ?Value List

                  ;                    ;               Position To:[          ]
                                                 
 |FC |Code        |Description                      |Status
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
 [ ] [          ] [                              ]  [ ]
$END-SCR   HR047    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR047    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2    *PCO-TYPE              A     02          SN    HZS
  2     HRU-FIELD-NAME        A     20          SB    HEY
  2    *PT-PCO-CODE           A     10          SB
  2     PS-PCO-CODE           A     10          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 10
    4   LINE-FC               A     01          LF
    4   PCO-CODE              A     10          LK
    4   PCO-DESCRIPTION       AL    30          LB
    4   PCO-ACTIVE-FLAG       A     01          LB
$END-TRANS HR047    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR047    S
  FC                                          A=Add
                                              C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  PCO-ACTIVE-FLAG       A                     A=Active,I=Inactive
$END-EDITS HR047    S
*******************************************************************************
*                            SCREEN   HR04  (8)
*******************************************************************************
$FORM      HR048    S
$KEYFCS    C
$DATAFCS   AC
$LREQFCS   AC
$LDATAFCS  AC
$FCPAIRS   AA,CA,CC,CD
$LSELECT
$WINDOW
$TITLE
$NOTKXFER
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR048    S
[ ]                ?    HR Codes

                 ;                              ;

|FC |Code        |Description                     |Count|Description  |Status
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
[ ] [          ] [                              ] & ]   ;          ;  [ ]
$END-SCR   HR048    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR048    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2    *AR-TYPE               A     02          SB
  2    *HS-TYPE               A     02          SB
  2    *PCO-TYPE              A     02          SN    HZS
  2     PCO-TYPE              X     30          SB
  2    *PT-PCO-CODE           A     10          SB
  2     DETAIL-GROUP          G                 SB
   3    DETAIL-LINE           G                 SB 10
    4   LINE-FC               A     01          LF
    4   PCO-CODE              A     10          LK
    4   PCO-DESCRIPTION       AL    30          LB
    4   PCO-COUNT             N     01          LB
    4   PCO-COUNT             X     10          LB
    4   PCO-ACTIVE-FLAG       A     01          LB
$END-TRANS HR048    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR048    S
  FC                                          A=Add
                                              C=Change
                                              I=Inquire
                                              +=(+)PageDown
                                              -=(-)PageUp
  PCO-TYPE                                    RQ=Requisition Status
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
  PCO-COUNT             1                     1=Excluded
                                              2=Active
  PCO-ACTIVE-FLAG       A                     A=Active
                                              I=Inactive
$END-EDITS HR048    S
*******************************************************************************
*                            SCREEN   HR04  (9)
*******************************************************************************
$FORM      HR049    S
$KEYFCS    CD
$DATAFCS   AC
$WINDOW
$TITLE
$NOTKXFER
$DBLXMTFCS D
$IBCM INPAC 102
$IBCM A 209
$IBCM C 210
$IBCM INP 211
$ROW       MAX
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    HR049    S
[ ]                ?    HR Codes

 |               Type:[  ]           ;                       ;
 |               Code:[          ]   [                              ]
 |             Status:[ ]            ;        ;

$TAB-REGION
$TAB-FOLDER
$TAB-FORM  ADDRESS           " Address "
 |     Address 1:[                                                         ]
 |     Address 2:[                              ]
 |     Address 3:[                              ]
 |     Address 4:[                              ]
 |City/Address 5:[                              ]
 |State/Province:[  ]           ;                              ;
 |   Postal Code:[          ]
 |        County:[                         ]
 |       Country:[  ]           ;                              ;
$END-TAB   ADDRESS
$TAB-FORM  MAIL-ADDRESS      " Mailing Address "
 |          Address 1:[                              ]
 |          Address 2:[                              ]
 |          Address 3:[                              ]
 |          Address 4:[                              ]
 |  City or Address 5:[                              ]
 |  State or Province:[  ]           ;                              ;
 |        Postal Code:[          ]
 |            Country:[  ]           ;                              ;
$END-TAB   MAIL-ADDRESS
$TAB-FORM  CONTACT           " Contact "
 |            Contact:[                              ][               ][ ]
 |          Telephone:[      ]  [               ]  [     ]
 |                Fax:[      ]  [               ]  [     ]
 |     E-mail Address:[                                                     ]
 |            Field 1:[          ]
 |            Field 2:[          ]
 |    Numeric Field 1:&             ]
 |    Numeric Field 2:&             ]
$END-TAB   CONTACT
$TAB-FORM  LOCATION          " Location Analysis "
 | Census Metropolitan Area:[          ]   ;                              ;
 |          Report Province:[          ]   ;                              ;
$END-TAB  LOCATION
$END-REGION
$END-SCR   HR049    S
*                            ELEMENT NAME
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     HR049    S
  2    *TC                    A     06          SR
  2     FC                    A     01          SF
  2     PCO-TYPE              A     02          SK    Jpo
  2     PCO-TYPE              X     23          SB    
  2     PCO-CODE              A     10          SN    Jpq
  2     PCO-DESCRIPTION       AL    30          SB    Jpr
  2     PCO-ACTIVE-FLAG       A     01          SB
  2     PCO-ACTIVE-FLAG       X     08          SB
*
  2     ADDRESS-TAB           AF    09          SB
  2     PDD-ADDR1             AL    57          SB
  2     PDD-ADDR2             AL    30          SB
  2     PDD-ADDR3             AL    30          SB
  2     PDD-ADDR4             AL    30          SB
  2     PDD-CITY              AL    30          SB
  2     PDD-STATE             A     02          SB        
  2     PDD-DESCRIPTION1      AL    30          SB        
  2     PDD-ZIP               A     10          SB
  2     PDD-COUNTY            AL    25          SB
  2     PDD-COUNTRY-CODE      A     02          SB    VA2
  2     INT-COUNTRY-DESC      AL    30          SB    VAD
*
  2     MAIL-ADDRESS-TAB      AF    17          SB
  2     PDD-SUPP-ADDR1        AL    30          SB
  2     PDD-SUPP-ADDR2        AL    30          SB
  2     PDD-SUPP-ADDR3        AL    30          SB
  2     PDD-SUPP-ADDR4        AL    30          SB
  2     PDD-SUPP-CITY         AL    30          SB
  2     PDD-SUPP-STATE        A     02          SB       
  2     PDD-DESCRIPTION2      AL    30          SB       
  2     PDD-SUPP-ZIP          A     10          SB
  2     PDD-SUPP-CNTRY-CODE   A     02          SB    VA2
  2     INT-SUPP-CNTRY-DESC   AL    30          SB    VAD
*
  2     CONTACT-TAB           AF    09          SB
  2     PDD-CONTACT-LAST      AL    30          SB
  2     PDD-CONTACT-FIRST     AL    15          SB
  2     PDD-CONTACT-MI        A     01          SB
  2     PDD-PHONE-COUNTRY     A     06          SB
  2     PDD-PHONE             A     15          SB
  2     PDD-PHONE-EXT         A     05          SB
  2     PDD-FAX-COUNTRY       A     06          SB
  2     PDD-FAX-NUMBER        A     15          SB
  2     PDD-FAX-EXT           A     05          SB
  2     PDD-EMAIL-ADDRESS     AL    53          SB
  2     PDD-USER1             AL    10          SB
  2     PDD-USER2             AL    10          SB
  2     PDD-NUMERIC1          N     13        2 SB
  2     PDD-NUMERIC2          N     13        2 SB
*
  2     LOCATION-TAB          AF    19          SB
  2     PDD-CMA               A     10          SB    Hza
  2     PDD-DESC              AL    30          SB    Hzb
  2     PDD-REPORT-PROV       A     10          SB    Hzc
  2     RPT-PROV-DESC         AL    30          SB    Hzd
$END-TRANS HR049    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     HR049    S
  FC                                          A=Add
                                              C=Change
                                              D=Delete
                                              I=Inquire
                                              N=Next
                                              P=Previous
  PCO-TYPE                                    EI=Education Institution
                                              HO=Incident Hospital
                                              PY=Incident Physician
                                              LO=Location
                                              TO=Test Location
                                              UN=Union
  PCO-ACTIVE-FLAG       A                     A=Active
                                              I=Inactive
$END-EDITS HR049    S
