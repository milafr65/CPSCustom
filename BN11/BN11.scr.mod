******* BN11.scr 3 <4135828097>
$RELEASE   02.1
*******************************************************************************
*                            SCREEN   BN11  (1)
*******************************************************************************
$FORM      BN111    S
$KEYFCS    C
$DATAFCS   AC
$NEXTFCS   NP
$ADDFCS    A
$DELFCS     
$NOLFCOK
$LREQFCS   A
$LDATAFCS  AC 
$LADDFCS   A
$FCPAIRS   AA,CA,CC,CD
*        1    1    2    2    3    3    4    4    5    5    6    6    7    7   7
*...5....0....5....0....5....0....5....0....5....0....5....0....5....0....5...9
*------------------------------------------------------------------------------
$SCREEN    BN111    S
[BN11[ ]                         ?Eligibility Postal Codes
+          ]  +      ]
|Postal Code Table:[                              ]   GoTo Zip [          ]

          |FC      |Postal Code |Effective Date |End Date |Insurer  |Network
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
          [ ]      [          ]    &      ]      &      ]  [    ]  [          ]
$END-SCR   BN111    S
*                            ELEMENT NAME                                          
* LEVEL       NAME           TYPE LENGTH    DEC REQ OC KN SC      PICTURE          
* ----- --------------------- --  ----        - -- -- --- - -----------------------
$TRANS     BN111    S
  2     TC                    AL    06          SB 
  2     FC                    A     01          SF 
  2     DKEY-PROTECTED        GP    22          SB 
   3    PT-BPC-POSTAL-CODE    A     10          SB 
   3    PT-BPC-INS-CARRIER    A     04          SB
   3    PT-BPC-START-DATE     N     08          SB
  2     ORIGINATOR            P     06          RO 
  2     BPC-POST-CODE-TBL     A     30          SN    Hca
  2     GO-TO-ZIP             A     10          SB
  2     DETAIL-GROUP          G                 SB 
   3    DETAIL-LINE           G                 SB 15 
    4   LINE-FC               A     01          LF 
    4   BPC-POSTAL-CODE       A     10          LL        
    4   BPC-START-DATE        F     08          LK        
    4   BPC-STOP-DATE         F     08          LL        
    4   BPC-INS-CARRIER       A     04          LK    HAZ    
    4   BPC-NETWORK           A     10          LR        
    4  *SPLAT-POSTAL-CODE     A     10          LB        
    4  *SPLAT-START-DATE      F     08          LB        
    4  *SPLAT-INS-CARRIER     A     04          LB
    4  *SPLAT-STOP-DATE       F     08          LB
    4  *SPLAT-NETWORK         A     10          LB
    4  *SPLAT-FUNCTION        A     01          LB        
$END-TRANS BN111    S
*
*
*        NAME               DEFAULT VALUE            EDIT LIST
* --------------------  --------------------  ------------------------
$EDITS     BN111    S
  FC                    I                     I=Inquire
                                              N=Next
                                              P=Previous
                                              A=Add
                                              C=Change
                                              D=Delete
                                              +=(+)PageDown
                                              -=(-)PageUp
  LINE-FC                                     A=Add
                                              C=Change
                                              D=Delete
$END-EDITS BN111    S


