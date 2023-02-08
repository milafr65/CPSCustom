******* HR01PD 30 <3186625627>
ACS001******************************************************************
ACS001*CHANGE LOG                                                      *
ACS001*                                                                *
ACS001* MOD NBR   DATE        PROGRAMMER   DESCRIPTION                 *
ACS001*--------  ----------  ------------  ----------------------------*
ACS001*ACS001    04/22/2009  M. HUNTER     REAPPLIED CUSTOMIZATIONS    *
ACS001*                                    AFTER CTP 58237 WAS APPLIED *
ACS002*ACS002    10/27/2010  M. Hunter     Reapplied customizations    *
ACS002*                      ACS           After 9.0 Apps Upgrade.     *
ACS003*ACS003    08/24/2011  M. Hunter     Reapplied customizations    *
ACS003*                      ACS           After 9.0.1 Apps Upgrade.   *
000100******************************************************************
000200 HR01S1-TRANSACTION              SECTION.
000300******************************************************************
000400 HR01S1-START.
000500
000600     PERFORM 200-EDIT-TRAN
000700     THRU    200-END.
000800
000900     IF (NO-ERROR-FOUND)
001000         PERFORM 400-PROCESS-TRAN
001100         THRU    400-END.
001200
001300     GO TO HR01S1-TRANSACTION-END.
001400
001500******************************************************************
001600 200-EDIT-TRAN.
001700******************************************************************
001800
001900     PERFORM 201-MOVE-SCR-TO-WS
002000     THRU    201-END.
002100
002200     PERFORM 2000-HRPRS-EDIT-TRAN.
002300
002400     PERFORM 202-MOVE-WS-TO-SCR
002500     THRU    202-END.
002600
002700     IF (ERROR-FOUND)
002800         GO TO 200-END.
002900
003000 200-END.
003100
003200******************************************************************
003300 201-MOVE-SCR-TO-WS.
003400******************************************************************
003500
003600     INITIALIZE HRPRS-SCR-FIELDS.
003700
003800     MOVE HR01F1-FC                    TO HRPRS-FC.
003900     MOVE HR01F1-FC-FN                 TO HRPRS-FC-FN.
004000     MOVE HR01F1-PRS-COMPANY           TO HRPRS-COMPANY.
004100     MOVE HR01F1-PRS-COMPANY-FN        TO HRPRS-COMPANY-FN.
004200     MOVE HR01F1-PRS-PROCESS-LEVEL     TO HRPRS-PROCESS-LEVEL.
004300     MOVE HR01F1-PRS-PROCESS-LEVEL-FN  TO HRPRS-PROCESS-LEVEL-FN.
004400     MOVE HR01F1-PRS-NAME              TO HRPRS-NAME.
004500     MOVE HR01F1-PRS-NAME-FN           TO HRPRS-NAME-FN.
004600     MOVE HR01F1-PRS-ADDR1             TO HRPRS-ADDR1.
004700     MOVE HR01F1-PRS-ADDR1-FN          TO HRPRS-ADDR1-FN.
004800     MOVE HR01F1-PRS-ADDR2             TO HRPRS-ADDR2.
004900     MOVE HR01F1-PRS-ADDR2-FN          TO HRPRS-ADDR2-FN.
005000     MOVE HR01F1-PRS-ADDR3             TO HRPRS-ADDR3.
005100     MOVE HR01F1-PRS-ADDR3-FN          TO HRPRS-ADDR3-FN.
005200     MOVE HR01F1-PRS-ADDR4             TO HRPRS-ADDR4.
005300     MOVE HR01F1-PRS-ADDR4-FN          TO HRPRS-ADDR4-FN.
005400     MOVE HR01F1-PRS-CITY              TO HRPRS-CITY.
005500     MOVE HR01F1-PRS-CITY-FN           TO HRPRS-CITY-FN.
005600     MOVE HR01F1-PRS-STATE             TO HRPRS-STATE.
005700     MOVE HR01F1-PRS-STATE-FN          TO HRPRS-STATE-FN.
005800     MOVE HR01F1-PRS-ZIP               TO HRPRS-ZIP.
005900     MOVE HR01F1-PRS-ZIP-FN            TO HRPRS-ZIP-FN.
006000     MOVE HR01F1-PRS-COUNTRY-CODE      TO HRPRS-COUNTRY-CODE.
006100     MOVE HR01F1-PRS-COUNTRY-CODE-FN   TO HRPRS-COUNTRY-CODE-FN.
           MOVE HR01F1-PRS-WORK-COUNTRY      TO HRPRS-WORK-COUNTRY.
           MOVE HR01F1-PRS-WORK-COUNTRY-FN   TO HRPRS-WORK-COUNTRY-FN.
P33418     MOVE HR01F1-PRS-PHONE-CNTRY       TO HRPRS-PHONE-CNTRY.
P33418     MOVE HR01F1-PRS-PHONE-CNTRY-FN    TO HRPRS-PHONE-CNTRY-FN.
P33418     MOVE HR01F1-PRS-PHONE-NBR         TO HRPRS-PHONE-NBR.
P33418     MOVE HR01F1-PRS-PHONE-NBR-FN      TO HRPRS-PHONE-NBR-FN.
P33418     MOVE HR01F1-PRS-PHONE-EXT         TO HRPRS-PHONE-EXT.
P33418     MOVE HR01F1-PRS-PHONE-EXT-FN      TO HRPRS-PHONE-EXT-FN.
006200     MOVE HR01F1-PRS-PL-OPTION         TO HRPRS-PL-OPTION.
006300     MOVE HR01F1-PRS-PL-OPTION-FN      TO HRPRS-PL-OPTION-FN.
006400     MOVE HR01F1-PRS-PRD-OPTION        TO HRPRS-PRD-OPTION.
006500     MOVE HR01F1-PRS-PRD-OPTION-FN     TO HRPRS-PRD-OPTION-FN.
006600     MOVE HR01F1-PRS-INCLUDE-TP135     TO HRPRS-INCLUDE-TP135.
006700     MOVE HR01F1-PRS-INCLUDE-TP135-FN  TO HRPRS-INCLUDE-TP135-FN.
006800     MOVE HR01F1-PRS-INCLUDE-PR132     TO HRPRS-INCLUDE-PR132.
006900     MOVE HR01F1-PRS-INCLUDE-PR132-FN  TO HRPRS-INCLUDE-PR132-FN.

      *** US TAX TAB FIELDS ***
007000     MOVE HR01F1-PRS-US-BSI-COMPANY    TO HRPRS-US-BSI-COMPANY.
007100     MOVE HR01F1-PRS-US-BSI-COMPANY-FN TO HRPRS-US-BSI-COMPANY-FN.
007200     MOVE HR01F1-PRS-RECIP-CALC        TO HRPRS-RECIP-CALC.
007300     MOVE HR01F1-PRS-RECIP-CALC-FN     TO HRPRS-RECIP-CALC-FN.
007400     MOVE HR01F1-PRS-LOCAL-MAX         TO HRPRS-LOCAL-MAX.
007500     MOVE HR01F1-PRS-LOCAL-MAX-FN      TO HRPRS-LOCAL-MAX-FN.
           MOVE HR01F1-PRS-EMP-TAX-ADDR      TO HRPRS-EMP-TAX-ADDR.
           MOVE HR01F1-PRS-EMP-TAX-ADDR-FN   TO HRPRS-EMP-TAX-ADDR-FN.
           MOVE HR01F1-PRS-TAX-FILTER        TO HRPRS-TAX-FILTER.
           MOVE HR01F1-PRS-TAX-FILTER-FN     TO HRPRS-TAX-FILTER-FN.
P76018*    MOVE HR01F1-PRS-TAX-LOC-MSGLVL    TO HRPRS-TAX-LOC-MSGLVL.
P76018     MOVE HR01F1-PRS-TAX-LOC-MSGLVL    TO WS-TAX-LOC-MSGLVL.
P76018     IF  (WS-TAX-LOC-MSGLVL IS NUMERIC)
P76018         MOVE WS-TAX-LOC-MSGLVL-N      TO HRPRS-TAX-LOC-MSGLVL
P76018     END-IF.
           MOVE HR01F1-PRS-TAX-LOC-MSGLVL-FN TO HRPRS-TAX-LOC-MSGLVL-FN.
           MOVE HR01F1-PRS-TAX-FORMS         TO HRPRS-TAX-FORMS.
           MOVE HR01F1-PRS-TAX-FORMS-FN      TO HRPRS-TAX-FORMS-FN.
TF90       MOVE HR01F1-PRS-LOCAL-RECIP-CD    TO HRPRS-LOCAL-RECIP-CD.
TF90       MOVE HR01F1-PRS-LOCAL-RECIP-CD-FN TO HRPRS-LOCAL-RECIP-CD-FN.

      *** CA TAX TAB FIELDS ***
           MOVE HR01F1-PRS-BUS-NBR-GRP       TO HRPRS-BUS-NBR-GRP.
           MOVE HR01F1-PRS-BUS-NBR-GRP-FN    TO HRPRS-BUS-NBR-GRP-FN.
           MOVE HR01F1-PBG-BUS-NBR           TO HRPRS-BUS-NBR.
           MOVE HR01F1-PBG-BUS-NBR-FN        TO HRPRS-BUS-NBR-FN.
           MOVE HR01F1-PBG-CA-BSI-COMPANY    TO HRPRS-CA-BSI-COMPANY.
           MOVE HR01F1-PBG-CA-BSI-COMPANY-FN TO HRPRS-CA-BSI-COMPANY-FN.
           MOVE HR01F1-PRS-QC-ENT-NBR-GRP    TO HRPRS-QC-ENT-NBR-GRP.
           MOVE HR01F1-PRS-QC-ENT-NBR-GRP-FN TO HRPRS-QC-ENT-NBR-GRP-FN.
           MOVE HR01F1-PQC-QC-ENT-NBR        TO HRPRS-QC-ENT-NBR.
           MOVE HR01F1-PQC-QC-ENT-NBR-FN     TO HRPRS-QC-ENT-NBR-FN.
           MOVE HR01F1-PRS-MAX-DED-IND       TO HRPRS-MAX-DED-IND.
           MOVE HR01F1-PRS-MAX-DED-IND-FN    TO HRPRS-MAX-DED-IND-FN.

007600     MOVE HR01F1-PRS-AP-COMPANY        TO HRPRS-AP-COMPANY.
007700     MOVE HR01F1-PRS-AP-COMPANY-FN     TO HRPRS-AP-COMPANY-FN.
007800     MOVE HR01F1-PRS-SEC-LVL           TO HRPRS-SEC-LVL.
007900     MOVE HR01F1-PRS-SEC-LVL-FN        TO HRPRS-SEC-LVL-FN.
008000     MOVE HR01F1-PRS-SEC-LOCATION      TO HRPRS-SEC-LOCATION.
008100     MOVE HR01F1-PRS-SEC-LOCATION-FN   TO HRPRS-SEC-LOCATION-FN.
           MOVE HR01F1-PRS-CURRENCY-CODE     TO HRPRS-CURRENCY-CODE.
           MOVE HR01F1-PRS-CURRENCY-CODE-FN  TO HRPRS-CURRENCY-CODE-FN.
           MOVE HR01F1-PRS-RECRUIT-FLAG      TO HRPRS-RECRUIT-FLAG.
           MOVE HR01F1-PRS-RECRUIT-FLAG-FN   TO HRPRS-RECRUIT-FLAG-FN.
           MOVE HR01F1-PT-XMIT-NBR-CUR       TO HRPRS-PT-XMIT-NBR-CUR.
           MOVE HR01F1-PT-XMIT-NBR-PL        TO HRPRS-PT-XMIT-NBR-PL.
           MOVE HR01F1-PT-XMIT-NBR-BUS       TO HRPRS-PT-XMIT-NBR-BUS.
           MOVE HR01F1-PT-XMIT-NBR-QC        TO HRPRS-PT-XMIT-NBR-QC.
           MOVE HR01F1-PT-XMIT-NBR-BSI       TO HRPRS-PT-XMIT-NBR-BSI.
           MOVE HR01F1-PT-XMIT-NBR-HIST      TO HRPRS-PT-XMIT-NBR-HIST.
           MOVE HR01F1-PT-XMIT-NBR-P1        TO HRPRS-PT-XMIT-NBR-P1.  
           MOVE HR01F1-PT-XMIT-NBR-PRD       TO HRPRS-PT-XMIT-NBR-PRD.

      ** BANK TAB **
008200     MOVE HR01F1-PRS-BANK-CODE         TO HRPRS-BANK-CODE.
008300     MOVE HR01F1-PRS-BANK-CODE-FN      TO HRPRS-BANK-CODE-FN.
008400     MOVE HR01F1-PRS-ACH-COMP-ID       TO HRPRS-ACH-COMP-ID.
008500     MOVE HR01F1-PRS-ACH-COMP-ID-FN    TO HRPRS-ACH-COMP-ID-FN.
J61485     MOVE HR01F1-PRS-ACH-COMP-NAME     TO HRPRS-ACH-COMP-NAME.
J61485     MOVE HR01F1-PRS-ACH-COMP-NAME-FN  TO HRPRS-ACH-COMP-NAME-FN.
008600     MOVE HR01F1-PRS-IMM-DEST          TO HRPRS-IMM-DEST.
008700     MOVE HR01F1-PRS-IMM-DEST-FN       TO HRPRS-IMM-DEST-FN.
008800     MOVE HR01F1-PRS-IMM-DEST-NAME     TO HRPRS-IMM-DEST-NAME.
008900     MOVE HR01F1-PRS-IMM-DEST-NAME-FN  TO HRPRS-IMM-DEST-NAME-FN.
009000     MOVE HR01F1-PRS-IMM-ORIG          TO HRPRS-IMM-ORIG.
009100     MOVE HR01F1-PRS-IMM-ORIG-FN       TO HRPRS-IMM-ORIG-FN.
009200     MOVE HR01F1-PRS-IMM-ORIG-NAME     TO HRPRS-IMM-ORIG-NAME.
009300     MOVE HR01F1-PRS-IMM-ORIG-NAME-FN  TO HRPRS-IMM-ORIG-NAME-FN.
      *
022000     MOVE HR01F1-BFL-BNK-ACCT-NBR      TO HRPRS-BNK-ACCT-NBR.
022100     MOVE HR01F1-BFL-BNK-ACCT-NBR-FN   TO HRPRS-BNK-ACCT-NBR-FN.
022400     MOVE HR01F1-BFL-CSH-DIST-CO       TO HRPRS-CSH-DIST-CO.
022500     MOVE HR01F1-BFL-CSH-DIST-CO-FN    TO HRPRS-CSH-DIST-CO-FN.
022600     MOVE HR01F1-BFL-CSH-ACCT-UNIT     TO HRPRS-CSH-ACCT-UNIT.
022700     MOVE HR01F1-BFL-CSH-ACCT-UNIT-FN  TO HRPRS-CSH-ACCT-UNIT-FN.
022800     MOVE HR01F1-BFL-CSH-ACCOUNT       TO HRPRS-CSH-ACCOUNT.
022900     MOVE HR01F1-BFL-CSH-ACCOUNT-FN    TO HRPRS-CSH-ACCOUNT-FN.
023000     MOVE HR01F1-BFL-CSH-SUB-ACCT      TO HRPRS-CSH-SUB-ACCT.
023100     MOVE HR01F1-BFL-CSH-SUB-ACCT-FN   TO HRPRS-CSH-SUB-ACCT-FN.
      *
022200     MOVE HR01F1-BFL-BNK-ACCT-NBR2     TO HRPRS-BNK-ACCT-NBR2.
022300     MOVE HR01F1-BFL-BNK-ACCT-NBR2-FN  TO HRPRS-BNK-ACCT-NBR2-FN.
022400     MOVE HR01F1-BFL-CSH-DIST-CO2      TO HRPRS-CSH-DIST-CO2.
022500     MOVE HR01F1-BFL-CSH-DIST-CO2-FN   TO HRPRS-CSH-DIST-CO2-FN.
022600     MOVE HR01F1-BFL-CSH-ACCT-UNIT2    TO HRPRS-CSH-ACCT-UNIT2.
022700     MOVE HR01F1-BFL-CSH-ACCT-UNIT2-FN TO HRPRS-CSH-ACCT-UNIT2-FN.
022800     MOVE HR01F1-BFL-CSH-ACCOUNT2      TO HRPRS-CSH-ACCOUNT2.
022900     MOVE HR01F1-BFL-CSH-ACCOUNT2-FN   TO HRPRS-CSH-ACCOUNT2-FN.
023000     MOVE HR01F1-BFL-CSH-SUB-ACCT2     TO HRPRS-CSH-SUB-ACCT2.
023100     MOVE HR01F1-BFL-CSH-SUB-ACCT2-FN  TO HRPRS-CSH-SUB-ACCT2-FN.
023200*
009400     MOVE HR01F1-PRS-EXP-DIST-CO       TO HRPRS-EXP-DIST-CO.
009500     MOVE HR01F1-PRS-EXP-DIST-CO-FN    TO HRPRS-EXP-DIST-CO-FN.
009600     MOVE HR01F1-PRS-EXP-ACCT-UNIT     TO HRPRS-EXP-ACCT-UNIT.
009700     MOVE HR01F1-PRS-EXP-ACCT-UNIT-FN  TO HRPRS-EXP-ACCT-UNIT-FN.
009800     MOVE HR01F1-PRS-EXP-ACCOUNT       TO HRPRS-EXP-ACCOUNT.
009900     MOVE HR01F1-PRS-EXP-ACCOUNT-FN    TO HRPRS-EXP-ACCOUNT-FN.
010000     MOVE HR01F1-PRS-EXP-SUB-ACCT      TO HRPRS-EXP-SUB-ACCT.
010100     MOVE HR01F1-PRS-EXP-SUB-ACCT-FN   TO HRPRS-EXP-SUB-ACCT-FN.
010200     MOVE HR01F1-PRS-CL-DIST-CO        TO HRPRS-CL-DIST-CO.
010300     MOVE HR01F1-PRS-CL-DIST-CO-FN     TO HRPRS-CL-DIST-CO-FN.
010400     MOVE HR01F1-PRS-CL-ACCT-UNIT      TO HRPRS-CL-ACCT-UNIT.
010500     MOVE HR01F1-PRS-CL-ACCT-UNIT-FN   TO HRPRS-CL-ACCT-UNIT-FN.
010600     MOVE HR01F1-PRS-CL-ACCOUNT        TO HRPRS-CL-ACCOUNT.
010700     MOVE HR01F1-PRS-CL-ACCOUNT-FN     TO HRPRS-CL-ACCOUNT-FN.
010800     MOVE HR01F1-PRS-CL-SUB-ACCT       TO HRPRS-CL-SUB-ACCT.
010900     MOVE HR01F1-PRS-CL-SUB-ACCT-FN    TO HRPRS-CL-SUB-ACCT-FN.
011000     MOVE HR01F1-PRS-PIK-DIST-CO       TO HRPRS-PIK-DIST-CO.
011100     MOVE HR01F1-PRS-PIK-DIST-CO-FN    TO HRPRS-PIK-DIST-CO-FN.
011200     MOVE HR01F1-PRS-PIK-ACCT-UNIT     TO HRPRS-PIK-ACCT-UNIT.
011300     MOVE HR01F1-PRS-PIK-ACCT-UNIT-FN  TO HRPRS-PIK-ACCT-UNIT-FN.
011400     MOVE HR01F1-PRS-PIK-ACCOUNT       TO HRPRS-PIK-ACCOUNT.
011500     MOVE HR01F1-PRS-PIK-ACCOUNT-FN    TO HRPRS-PIK-ACCOUNT-FN.
011600     MOVE HR01F1-PRS-PIK-SUB-ACCT      TO HRPRS-PIK-SUB-ACCT.
011700     MOVE HR01F1-PRS-PIK-SUB-ACCT-FN   TO HRPRS-PIK-SUB-ACCT-FN.
011800     MOVE HR01F1-PRS-ACR-DIST-CO       TO HRPRS-ACR-DIST-CO.
011900     MOVE HR01F1-PRS-ACR-DIST-CO-FN    TO HRPRS-ACR-DIST-CO-FN.
012000     MOVE HR01F1-PRS-ACR-ACCT-UNIT     TO HRPRS-ACR-ACCT-UNIT.
012100     MOVE HR01F1-PRS-ACR-ACCT-UNIT-FN  TO HRPRS-ACR-ACCT-UNIT-FN.
           MOVE HR01F1-PRS-ACTIVE-FLAG       TO HRPRS-ACTIVE-FLAG.
           MOVE HR01F1-PRS-ACTIVE-FLAG-FN    TO HRPRS-ACTIVE-FLAG-FN.
012200     MOVE "Y"                          TO HRPRS-PROC-LEV.
           MOVE HR01F1-PT-XMIT-ST-CHG        TO HRPRS-PT-XMIT-ST-CHG.
012300
012400 201-END.
012500
012600******************************************************************
012700 202-MOVE-WS-TO-SCR.
012800******************************************************************
012900
013000     MOVE HRPRS-NAME             TO HR01F1-PRS-NAME.
           MOVE HRPRS-ACTIVE-FLAG      TO HR01F1-PRS-ACTIVE-FLAG.
013100     MOVE HRPRS-ADDR1            TO HR01F1-PRS-ADDR1.
013200     MOVE HRPRS-ADDR2            TO HR01F1-PRS-ADDR2.
013300     MOVE HRPRS-ADDR3            TO HR01F1-PRS-ADDR3.
013400     MOVE HRPRS-ADDR4            TO HR01F1-PRS-ADDR4.
013500     MOVE HRPRS-CITY             TO HR01F1-PRS-CITY.
013600     MOVE HRPRS-STATE            TO HR01F1-PRS-STATE.
013700     MOVE HRPRS-ZIP              TO HR01F1-PRS-ZIP.
013800     MOVE HRPRS-COUNTRY-CODE     TO HR01F1-PRS-COUNTRY-CODE.
           MOVE HRPRS-WORK-COUNTRY     TO HR01F1-PRS-WORK-COUNTRY.
P33418     MOVE HRPRS-PHONE-CNTRY      TO HR01F1-PRS-PHONE-CNTRY.
P33418     MOVE HRPRS-PHONE-NBR        TO HR01F1-PRS-PHONE-NBR.
P33418     MOVE HRPRS-PHONE-EXT        TO HR01F1-PRS-PHONE-EXT.
013900     MOVE HRPRS-PL-OPTION        TO HR01F1-PRS-PL-OPTION.
014000     MOVE HRPRS-PRD-OPTION       TO HR01F1-PRS-PRD-OPTION.
014100     MOVE HRPRS-INCLUDE-TP135    TO HR01F1-PRS-INCLUDE-TP135.
014200     MOVE HRPRS-INCLUDE-PR132    TO HR01F1-PRS-INCLUDE-PR132.

014300     MOVE HRPRS-US-BSI-COMPANY   TO HR01F1-PRS-US-BSI-COMPANY.
014400     MOVE HRPRS-RECIP-CALC       TO HR01F1-PRS-RECIP-CALC.
014500     MOVE HRPRS-LOCAL-MAX        TO HR01F1-PRS-LOCAL-MAX.
           MOVE HRPRS-EMP-TAX-ADDR     TO HR01F1-PRS-EMP-TAX-ADDR.
           MOVE HRPRS-TAX-FILTER       TO HR01F1-PRS-TAX-FILTER.
           MOVE HRPRS-TAX-LOC-MSGLVL   TO HR01F1-PRS-TAX-LOC-MSGLVL.
           MOVE HRPRS-TAX-FORMS        TO HR01F1-PRS-TAX-FORMS.
TF90       MOVE HRPRS-LOCAL-RECIP-CD   TO HR01F1-PRS-LOCAL-RECIP-CD.  

           MOVE HRPRS-BUS-NBR-GRP      TO HR01F1-PRS-BUS-NBR-GRP.
           MOVE HRPRS-BUS-NBR          TO HR01F1-PBG-BUS-NBR.
           MOVE HRPRS-CA-BSI-COMPANY   TO HR01F1-PBG-CA-BSI-COMPANY.
           MOVE HRPRS-QC-ENT-NBR-GRP   TO HR01F1-PRS-QC-ENT-NBR-GRP.
           MOVE HRPRS-QC-ENT-NBR       TO HR01F1-PQC-QC-ENT-NBR.
           MOVE HRPRS-MAX-DED-IND      TO HR01F1-PRS-MAX-DED-IND.

014600     MOVE HRPRS-AP-COMPANY       TO HR01F1-PRS-AP-COMPANY.
014700     MOVE HRPRS-SEC-LVL          TO HR01F1-PRS-SEC-LVL.
014800     MOVE HRPRS-SEC-LOCATION     TO HR01F1-PRS-SEC-LOCATION.
           MOVE HRPRS-CURRENCY-CODE    TO HR01F1-PRS-CURRENCY-CODE.
           MOVE HRPRS-RECRUIT-FLAG     TO HR01F1-PRS-RECRUIT-FLAG.
           MOVE HRPRS-PT-XMIT-NBR-CUR  TO HR01F1-PT-XMIT-NBR-CUR.
           MOVE HRPRS-PT-XMIT-NBR-PL   TO HR01F1-PT-XMIT-NBR-PL.
           MOVE HRPRS-PT-XMIT-NBR-BUS  TO HR01F1-PT-XMIT-NBR-BUS.
           MOVE HRPRS-PT-XMIT-NBR-QC   TO HR01F1-PT-XMIT-NBR-QC.
           MOVE HRPRS-PT-XMIT-NBR-BSI  TO HR01F1-PT-XMIT-NBR-BSI.
           MOVE HRPRS-PT-XMIT-NBR-HIST TO HR01F1-PT-XMIT-NBR-HIST.
           MOVE HRPRS-PT-XMIT-ST-CHG   TO HR01F1-PT-XMIT-ST-CHG.
           MOVE HRPRS-PT-XMIT-NBR-P1   TO HR01F1-PT-XMIT-NBR-P1.
           MOVE HRPRS-PT-XMIT-NBR-PRD  TO HR01F1-PT-XMIT-NBR-PRD.
      *
014900     MOVE HRPRS-BANK-CODE        TO HR01F1-PRS-BANK-CODE.
015000     MOVE HRPRS-ACH-COMP-ID      TO HR01F1-PRS-ACH-COMP-ID.
J61485     MOVE HRPRS-ACH-COMP-NAME    TO HR01F1-PRS-ACH-COMP-NAME.
015100     MOVE HRPRS-IMM-DEST         TO HR01F1-PRS-IMM-DEST.
015200     MOVE HRPRS-IMM-DEST-NAME    TO HR01F1-PRS-IMM-DEST-NAME.
015300     MOVE HRPRS-IMM-ORIG         TO HR01F1-PRS-IMM-ORIG.
015400     MOVE HRPRS-IMM-ORIG-NAME    TO HR01F1-PRS-IMM-ORIG-NAME.
           MOVE HRPRS-BNK-ACCT-NBR     TO HR01F1-BFL-BNK-ACCT-NBR.
           MOVE HRPRS-CSH-DIST-CO      TO HR01F1-BFL-CSH-DIST-CO.
           MOVE HRPRS-CSH-ACCT-UNIT    TO HR01F1-BFL-CSH-ACCT-UNIT.
           MOVE HRPRS-CSH-ACCOUNT      TO HR01F1-BFL-CSH-ACCOUNT.
           MOVE HRPRS-CSH-SUB-ACCT     TO HR01F1-BFL-CSH-SUB-ACCT.
           MOVE HRPRS-BNK-ACCT-NBR2    TO HR01F1-BFL-BNK-ACCT-NBR2.
           MOVE HRPRS-CSH-DIST-CO2     TO HR01F1-BFL-CSH-DIST-CO2.
           MOVE HRPRS-CSH-ACCT-UNIT2   TO HR01F1-BFL-CSH-ACCT-UNIT2.
           MOVE HRPRS-CSH-ACCOUNT2     TO HR01F1-BFL-CSH-ACCOUNT2.
           MOVE HRPRS-CSH-SUB-ACCT2    TO HR01F1-BFL-CSH-SUB-ACCT2.
      *
015500     MOVE HRPRS-EXP-DIST-CO      TO HR01F1-PRS-EXP-DIST-CO.
015600     MOVE HRPRS-EXP-ACCT-UNIT    TO HR01F1-PRS-EXP-ACCT-UNIT.
015700     MOVE HRPRS-EXP-ACCOUNT      TO HR01F1-PRS-EXP-ACCOUNT.
015800     MOVE HRPRS-EXP-SUB-ACCT     TO HR01F1-PRS-EXP-SUB-ACCT.
015900     MOVE HRPRS-CL-DIST-CO       TO HR01F1-PRS-CL-DIST-CO.
016000     MOVE HRPRS-CL-ACCT-UNIT     TO HR01F1-PRS-CL-ACCT-UNIT.
016100     MOVE HRPRS-CL-ACCOUNT       TO HR01F1-PRS-CL-ACCOUNT.
016200     MOVE HRPRS-CL-SUB-ACCT      TO HR01F1-PRS-CL-SUB-ACCT.
016300     MOVE HRPRS-PIK-DIST-CO      TO HR01F1-PRS-PIK-DIST-CO.
016400     MOVE HRPRS-PIK-ACCT-UNIT    TO HR01F1-PRS-PIK-ACCT-UNIT.
016500     MOVE HRPRS-PIK-ACCOUNT      TO HR01F1-PRS-PIK-ACCOUNT.
016600     MOVE HRPRS-PIK-SUB-ACCT     TO HR01F1-PRS-PIK-SUB-ACCT.
016700     MOVE HRPRS-ACR-DIST-CO      TO HR01F1-PRS-ACR-DIST-CO.
016800     MOVE HRPRS-ACR-ACCT-UNIT    TO HR01F1-PRS-ACR-ACCT-UNIT.
016900
017000 202-END.
017100
017200******************************************************************
017300 400-PROCESS-TRAN.
017400******************************************************************
017500
017600     IF (HR01F1-FC = "A" OR "C" OR "D")
017700         PERFORM 910-AUDIT-BEGIN
017800         IF (DMS-ABORTED)
017900             GO TO 400-END
018000         END-IF
018100
018200         PERFORM 410-UPDATE
018300         THRU    410-END
018400
018500         PERFORM 920-AUDIT-END
018600     ELSE
018700     IF (HR01F1-FC = "I" OR "N" OR "P")
018800         PERFORM 480-INQUIRE
018900         THRU    480-END.
019000
019100 400-END.
019200
019300******************************************************************
019400 410-UPDATE.
019500******************************************************************
019600
019700     IF (HR01F1-FC = "A" OR "C" OR "D")
019800         PERFORM 3000-HRPRS-PROCESS-TRAN
019900         IF (HR01F1-FC = "A" OR "C")
020000             PERFORM 600-MOVE-TO-SCREEN
020100             THRU    600-END.
020200
020300     IF (HR01F1-FC = "A")
               IF  (HRPRS-CO-COUNTRY-FLAG = "Y")
               AND (NEW-PRS-COUNTRY)
                   PERFORM 700-SET-REQ-FLDS
                   MOVE 121                TO CRT-MSG-NBR
                   MOVE HRPRS-WORK-COUNTRY TO CRT-ERR-VAR1
                   PERFORM 790-GET-MSG
               ELSE
020400             MOVE CRT-ADD-COMPLETE   TO CRT-MESSAGE
               END-IF
020500     ELSE
020600     IF (HR01F1-FC = "C")
               IF  (HRPRS-EMDISTMAST-FOUND)
                   MOVE 122                TO CRT-MSG-NBR
               ELSE
                   IF (HRPRS-WARNING = 3)
                       MOVE 100            TO CRT-MSG-NBR
                       MOVE HRPRS-PROCESS-LEVEL
                                           TO CRT-ERR-VAR1
                       PERFORM 790-GET-MSG
                   ELSE
020700                MOVE CRT-CHG-COMPLETE
020700                                     TO CRT-MESSAGE
                   END-IF
               END-IF
020800     ELSE
020900         IF (HR01F1-FC = "D")
021000             MOVE CRT-RECS-DELETED   TO CRT-MESSAGE.
021100
021200 410-END.
021300
021400******************************************************************
021500 480-INQUIRE.
021600******************************************************************
021700
021800     PERFORM 600-MOVE-TO-SCREEN
021900     THRU    600-END.
022000     MOVE CRT-INQ-COMPLETE       TO CRT-MESSAGE.
022100
022200 480-END.
022300
022400******************************************************************
022500 600-MOVE-TO-SCREEN.
022600******************************************************************
022700
022800     MOVE PRS-COMPANY                TO HR01F1-PRS-COMPANY.
022900     MOVE PRS-PROCESS-LEVEL          TO HR01F1-PRS-PROCESS-LEVEL.
023000     MOVE PRS-NAME                   TO HR01F1-PRS-NAME.
023100     MOVE PRS-ADDR1                  TO HR01F1-PRS-ADDR1.
023200     MOVE PRS-ADDR2                  TO HR01F1-PRS-ADDR2.
023300     MOVE PRS-ADDR3                  TO HR01F1-PRS-ADDR3.
023400     MOVE PRS-ADDR4                  TO HR01F1-PRS-ADDR4.
023500     MOVE PRS-CITY                   TO HR01F1-PRS-CITY.
023600     MOVE PRS-STATE                  TO HR01F1-PRS-STATE.
023700     MOVE PRS-ZIP                    TO HR01F1-PRS-ZIP.
023800     MOVE PRS-COUNTRY-CODE           TO HR01F1-PRS-COUNTRY-CODE.
           MOVE PRS-WORK-COUNTRY           TO HR01F1-PRS-WORK-COUNTRY.
P33418     MOVE PRS-PHONE-CNTRY            TO HR01F1-PRS-PHONE-CNTRY.
P33418     MOVE PRS-PHONE-NBR              TO HR01F1-PRS-PHONE-NBR.
P33418     MOVE PRS-PHONE-EXT              TO HR01F1-PRS-PHONE-EXT.
023900     MOVE PRS-PL-OPTION              TO HR01F1-PRS-PL-OPTION.
024000     MOVE PRS-PRD-OPTION             TO HR01F1-PRS-PRD-OPTION.
024100     MOVE PRS-INCLUDE-TP135          TO HR01F1-PRS-INCLUDE-TP135.
024200     MOVE PRS-INCLUDE-PR132          TO HR01F1-PRS-INCLUDE-PR132.

024300     MOVE PRS-BSI-COMPANY            TO HR01F1-PRS-US-BSI-COMPANY.
024400     MOVE PRS-RECIP-CALC             TO HR01F1-PRS-RECIP-CALC.
024500     MOVE PRS-LOCAL-MAX              TO HR01F1-PRS-LOCAL-MAX.
           MOVE PRS-EMP-TAX-ADDR           TO HR01F1-PRS-EMP-TAX-ADDR.
           MOVE PRS-TAX-FILTER             TO HR01F1-PRS-TAX-FILTER.
           MOVE PRS-TAX-LOC-MSGLVL         TO HR01F1-PRS-TAX-LOC-MSGLVL.
           MOVE PRS-TAX-FORMS              TO HR01F1-PRS-TAX-FORMS.
TF90       MOVE PRS-LOCAL-RECIP-CD         TO HR01F1-PRS-LOCAL-RECIP-CD.
J61485     MOVE PRS-ACH-COMP-NAME          TO HR01F1-PRS-ACH-COMP-NAME.

           INITIALIZE HR01F1-PRS-BUS-NBR-GRP
                      HR01F1-PBG-BUS-NBR
                      HR01F1-PRS-QC-ENT-NBR-GRP
                      HR01F1-PQC-QC-ENT-NBR
                      HR01F1-PBG-CA-BSI-COMPANY.

           IF (PRS-WORK-COUNTRY     = HRWS-CA-WORK-COUNTRY)
               MOVE PRS-BUS-NBR-GRP        TO HR01F1-PRS-BUS-NBR-GRP
                                              DB-BUS-NBR-GRP
               MOVE PRS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PBGSET1
               IF (PRBUSGRP-FOUND)
                   MOVE PBG-CA-BSI-COMPANY TO HR01F1-PBG-CA-BSI-COMPANY
                   MOVE PBG-BUS-NBR        TO HR01F1-PBG-BUS-NBR
               END-IF
               MOVE PRS-QC-ENT-NBR-GRP     TO HR01F1-PRS-QC-ENT-NBR-GRP
                                              DB-QC-ENT-NBR-GRP
               MOVE PRS-PROCESS-LEVEL      TO DB-PROCESS-LEVEL
               PERFORM 840-FIND-PQCSET1
               IF (PRQCENTGRP-FOUND)
                   MOVE PQC-QC-ENT-NBR     TO HR01F1-PQC-QC-ENT-NBR
               END-IF
               INITIALIZE HR01F1-PRS-ACH-COMP-ID
                          HR01F1-PRS-IMM-DEST
                          HR01F1-PRS-IMM-DEST-NAME
                          HR01F1-PRS-IMM-ORIG
                          HR01F1-PRS-IMM-ORIG-NAME
           ELSE
025000         MOVE PRS-ACH-COMP-ID        TO HR01F1-PRS-ACH-COMP-ID
025100         MOVE PRS-IMM-DEST           TO HR01F1-PRS-IMM-DEST
025200         MOVE PRS-IMM-DEST-NAME      TO HR01F1-PRS-IMM-DEST-NAME
025300         MOVE PRS-IMM-ORIG           TO HR01F1-PRS-IMM-ORIG
025400         MOVE PRS-IMM-ORIG-NAME      TO HR01F1-PRS-IMM-ORIG-NAME.

           MOVE PRS-MAX-DED-IND            TO HR01F1-PRS-MAX-DED-IND.

024600     MOVE PRS-AP-COMPANY             TO HR01F1-PRS-AP-COMPANY.
024700     MOVE PRS-SEC-LVL                TO HR01F1-PRS-SEC-LVL.
024800     MOVE PRS-SEC-LOCATION           TO HR01F1-PRS-SEC-LOCATION.
           MOVE PRS-CURRENCY-CODE          TO HR01F1-PRS-CURRENCY-CODE
                                              DB-CURRENCY-CODE.
           PERFORM 840-FIND-CUCSET1.
           IF (CUCODES-FOUND)
               MOVE CUC-FORMS-EXP          TO HR01F1-CURR-FORMS-EXP
           ELSE
               MOVE SPACES                 TO HR01F1-CURR-FORMS-EXP.

           MOVE PRS-RECRUIT-FLAG           TO HR01F1-PRS-RECRUIT-FLAG.

           IF (PRS-RECRUIT-FLAG            = "Y")
      ******** Process level uses eRecruiting
               MOVE 123                    TO CRT-MSG-NBR
           ELSE
           IF (HRPRS-CO-RECRUIT-FLAG       = "1")
      ******** Entire company uses eRecruiting
               MOVE 124                    TO CRT-MSG-NBR
           ELSE
           IF (HRPRS-CO-RECRUIT-FLAG       = SPACES)
      ******** Not applicable; eRecruting not used
               MOVE 125                    TO CRT-MSG-NBR
           ELSE
           IF (HRPRS-CO-RECRUIT-FLAG       = "2")
      ******** Process level does not use eRecruiting
               MOVE 126                    TO CRT-MSG-NBR.

           MOVE "HR01"                     TO CRT-ERROR-CAT.
           PERFORM 790-GET-MSG.
           MOVE CRT-MESSAGE                TO HR01F1-PRS-RECRUIT-FLAG-D.

024900     MOVE PRS-BANK-CODE              TO HR01F1-PRS-BANK-CODE.

025500     MOVE PRS-EXP-DIST-CO            TO HR01F1-PRS-EXP-DIST-CO.
025600     MOVE PRS-EXP-ACCT-UNIT          TO HR01F1-PRS-EXP-ACCT-UNIT.
025700     MOVE PRS-EXP-ACCOUNT            TO HR01F1-PRS-EXP-ACCOUNT.
025800     MOVE PRS-EXP-SUB-ACCT           TO HR01F1-PRS-EXP-SUB-ACCT.
025900     MOVE PRS-CL-DIST-CO             TO HR01F1-PRS-CL-DIST-CO.
026000     MOVE PRS-CL-ACCT-UNIT           TO HR01F1-PRS-CL-ACCT-UNIT.
026100     MOVE PRS-CL-ACCOUNT             TO HR01F1-PRS-CL-ACCOUNT.
026200     MOVE PRS-CL-SUB-ACCT            TO HR01F1-PRS-CL-SUB-ACCT.
026300     MOVE PRS-PIK-DIST-CO            TO HR01F1-PRS-PIK-DIST-CO.
026400     MOVE PRS-PIK-ACCT-UNIT          TO HR01F1-PRS-PIK-ACCT-UNIT.
026500     MOVE PRS-PIK-ACCOUNT            TO HR01F1-PRS-PIK-ACCOUNT.
026600     MOVE PRS-PIK-SUB-ACCT           TO HR01F1-PRS-PIK-SUB-ACCT.
026700     MOVE PRS-ACR-DIST-CO            TO HR01F1-PRS-ACR-DIST-CO.
026800     MOVE PRS-ACR-ACCT-UNIT          TO HR01F1-PRS-ACR-ACCT-UNIT.
           MOVE PRS-ACTIVE-FLAG            TO HR01F1-PRS-ACTIVE-FLAG.
026900
027000     MOVE HR01F1-PRS-COMPANY         TO DB-COMPANY.
027100     MOVE HR01F1-PRS-BANK-CODE       TO DB-BANK-CODE.
027200
027300     INITIALIZE HR01F1-CPY-NAME.
027400     IF (PRS-AP-COMPANY              NOT = ZEROES)
027500         MOVE PRS-AP-COMPANY         TO IFCOWS-COMPANY
027600         MOVE "AP"                   TO IFCOWS-SYSTEM
027700         PERFORM 615-EDIT-COMPANY-60
027800         IF (NO-ERROR-FOUND)
027900             MOVE IFCOWS-GLS-NAME    TO HR01F1-CPY-NAME
028000         END-IF
028100         MOVE HR01F1-PRS-COMPANY     TO DB-COMPANY.
028200
028300     IF (HR01F1-PRS-BANK-CODE NOT = SPACES)
028400         PERFORM 840-FIND-BFLSET1
028500         MOVE BFL-BNK-ACCT-NBR       TO HR01F1-BFL-BNK-ACCT-NBR
                                              DB-BNK-ACCT-NBR
               PERFORM 840-FIND-BACSET1
               IF (BANKACCT-FOUND)
                   MOVE BAC-BANK-NAME      TO HR01F1-BAC-BANK-NAME
               ELSE
                   MOVE SPACES             TO HR01F1-BAC-BANK-NAME
               END-IF
028600         MOVE BFL-CSH-DIST-CO        TO HR01F1-BFL-CSH-DIST-CO
028700                                        IFACWS-COMPANY
028800         MOVE BFL-CSH-ACCOUNT        TO HR01F1-BFL-CSH-ACCOUNT
028900                                        IFACWS-ACCOUNT
029000         MOVE BFL-CSH-ACCT-UNIT      TO HR01F1-BFL-CSH-ACCT-UNIT
029100                                        IFACWS-ACCT-UNIT
029200         MOVE BFL-CSH-SUB-ACCT       TO HR01F1-BFL-CSH-SUB-ACCT
029300                                        IFACWS-SUB-ACCOUNT
029400         MOVE 4                      TO IFACWS-EDIT-TYPE
029500         PERFORM 635-EDIT-GLMASTER-60
029600         MOVE IFACWS-GLM-DESCRIPTION TO HR01F1-CSH-DESCRIPTION
029700         MOVE BFL-BNK-ACCT-NBR2      TO HR01F1-BFL-BNK-ACCT-NBR2
                                              DB-BNK-ACCT-NBR
               PERFORM 840-FIND-BACSET1
               IF (BANKACCT-FOUND)
                   MOVE BAC-BANK-NAME      TO HR01F1-BAC-BANK-NAME2
               ELSE
                   MOVE SPACES             TO HR01F1-BAC-BANK-NAME2
               END-IF
029800         MOVE BFL-CSH-DIST-CO2       TO HR01F1-BFL-CSH-DIST-CO2
                                              IFACWS-COMPANY
029900         MOVE BFL-CSH-ACCT-UNIT2     TO HR01F1-BFL-CSH-ACCT-UNIT2
                                              IFACWS-ACCT-UNIT
030000         MOVE BFL-CSH-ACCOUNT2       TO HR01F1-BFL-CSH-ACCOUNT2
                                              IFACWS-ACCOUNT
030100         MOVE BFL-CSH-SUB-ACCT2      TO HR01F1-BFL-CSH-SUB-ACCT2
                                              IFACWS-SUB-ACCOUNT
               MOVE 4                      TO IFACWS-EDIT-TYPE
               PERFORM 635-EDIT-GLMASTER-60
               MOVE IFACWS-GLM-DESCRIPTION TO HR01F1-CSH-DESCRIPTION2
030200
031300     ELSE
031400         MOVE SPACES                 TO HR01F1-BFL-BNK-ACCT-NBR
031500                                        HR01F1-BFL-CSH-ACCT-UNIT
                                              HR01F1-CSH-DESCRIPTION
031600                                        HR01F1-BFL-BNK-ACCT-NBR2
031700                                        HR01F1-BFL-CSH-ACCT-UNIT2
031800                                        HR01F1-CSH-DESCRIPTION2
032300         MOVE ZEROS                  TO HR01F1-BFL-CSH-ACCOUNT
032400                                        HR01F1-BFL-CSH-SUB-ACCT
032500                                        HR01F1-BFL-CSH-DIST-CO
032600                                        HR01F1-BFL-CSH-ACCOUNT2
032700                                        HR01F1-BFL-CSH-SUB-ACCT2
                                              HR01F1-BFL-CSH-DIST-CO2.
034200
034300     IF (HR01F1-PRS-EXP-ACCT-UNIT NOT = SPACES)
034400     OR (HR01F1-PRS-EXP-ACCOUNT   NOT = ZEROS)
034500         MOVE HR01F1-PRS-EXP-DIST-CO    TO IFACWS-COMPANY
034600         MOVE HR01F1-PRS-EXP-ACCT-UNIT  TO IFACWS-ACCT-UNIT
034700         MOVE HR01F1-PRS-EXP-ACCOUNT    TO IFACWS-ACCOUNT
034800         MOVE HR01F1-PRS-EXP-SUB-ACCT   TO IFACWS-SUB-ACCOUNT
034900         MOVE 4                         TO IFACWS-EDIT-TYPE
035000         PERFORM 635-EDIT-GLMASTER-60
035100         MOVE IFACWS-GLM-DESCRIPTION    TO HR01F1-EXP-DESCRIPTION
035200     ELSE
035300         MOVE SPACES                    TO HR01F1-EXP-DESCRIPTION.
035400
035500     IF (HR01F1-PRS-CL-ACCT-UNIT NOT = SPACES)
035600     OR (HR01F1-PRS-CL-ACCOUNT   NOT = ZEROS)
035700         MOVE HR01F1-PRS-CL-DIST-CO     TO IFACWS-COMPANY
035800         MOVE HR01F1-PRS-CL-ACCT-UNIT   TO IFACWS-ACCT-UNIT
035900         MOVE HR01F1-PRS-CL-ACCOUNT     TO IFACWS-ACCOUNT
036000         MOVE HR01F1-PRS-CL-SUB-ACCT    TO IFACWS-SUB-ACCOUNT
036100         MOVE 4                         TO IFACWS-EDIT-TYPE
036200         PERFORM 635-EDIT-GLMASTER-60
036300         MOVE IFACWS-GLM-DESCRIPTION    TO HR01F1-CL-DESCRIPTION
036400     ELSE
036500         MOVE SPACES                    TO HR01F1-CL-DESCRIPTION.
036600
036700     IF (HR01F1-PRS-PIK-ACCT-UNIT NOT = SPACES)
036800     OR (HR01F1-PRS-PIK-ACCOUNT   NOT = ZEROS)
036900         MOVE HR01F1-PRS-PIK-DIST-CO    TO IFACWS-COMPANY
037000         MOVE HR01F1-PRS-PIK-ACCT-UNIT  TO IFACWS-ACCT-UNIT
037100         MOVE HR01F1-PRS-PIK-ACCOUNT    TO IFACWS-ACCOUNT
037200         MOVE HR01F1-PRS-PIK-SUB-ACCT   TO IFACWS-SUB-ACCOUNT
037300         MOVE HR01F1-PRS-COMPANY        TO IFACWS-COMPANY
037400         MOVE 4                         TO IFACWS-EDIT-TYPE
037500         PERFORM 635-EDIT-GLMASTER-60
037600         MOVE IFACWS-GLM-DESCRIPTION    TO HR01F1-PIK-DESCRIPTION
037700     ELSE
037800         MOVE SPACES                    TO HR01F1-PIK-DESCRIPTION.
037900
           IF  (HR01F1-PRS-ACR-DIST-CO   NOT = ZEROES)
038000     AND (HR01F1-PRS-ACR-ACCT-UNIT NOT = SPACES)
038100         MOVE HR01F1-PRS-ACR-DIST-CO    TO IFAUWS-COMPANY
038200         MOVE HR01F1-PRS-ACR-ACCT-UNIT  TO IFAUWS-ACCT-UNIT
038400         PERFORM 645-EDIT-ACCT-UNIT-60
038500         MOVE IFAUWS-GLN-DESCRIPTION    TO HR01F1-ACR-DESCRIPTION
038600     ELSE
038700         MOVE SPACES                    TO HR01F1-ACR-DESCRIPTION.
038800
           INITIALIZE                         HR01F1-INT-COUNTRY-DESC.
           IF (HR01F1-PRS-COUNTRY-CODE NOT = SPACES)
               MOVE HR01F1-PRS-COUNTRY-CODE   TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC   TO HR01F1-INT-COUNTRY-DESC.

           INITIALIZE                         HR01F1-INT-WRK-CNTRY-DESC.
           IF (HR01F1-PRS-WORK-COUNTRY NOT = SPACES)
               MOVE HR01F1-PRS-WORK-COUNTRY   TO DB-COUNTRY-CODE
               PERFORM 840-FIND-INTSET1
               IF (INSTCTRYCD-FOUND)
                   MOVE INT-COUNTRY-DESC   TO HR01F1-INT-WRK-CNTRY-DESC.

038900     MOVE SPACES                        TO DB-PROCESS-LEVEL.
039000     PERFORM 840-FIND-PRSSET1.
039100     MOVE PRS-NAME                      TO HR01F1-PRS1-NAME.
039200
039300 600-END.
039400
039500******************************************************************
039600 HR01S1-TRANSACTION-END.
039700******************************************************************
ACS001     PERFORM 1000-OPEN-WORKFLOW-DB.
ACS002
ACS002     INITIALIZE WFAPI-I-SERVICE.
ACS002
ACS001     MOVE HR01F1-PRS-COMPANY       TO WFAPI-CRITERION-1.
ACS001     MOVE "COMPANY"                TO WFAPI-I-VARIABLE-NAME (1).
ACS001     MOVE HR01F1-PRS-COMPANY       TO WFAPI-I-VARIABLE-VAL  (1).
ACS001     MOVE "PROCESS_LEVEL"          TO WFAPI-I-VARIABLE-NAME (2).
ACS001     MOVE HR01F1-PRS-PROCESS-LEVEL TO WFAPI-I-VARIABLE-VAL  (2).
ACS001     MOVE "NAME"                   TO WFAPI-I-VARIABLE-NAME (3).
ACS001     MOVE HR01F1-PRS-NAME          TO WFAPI-I-VARIABLE-VAL  (3).
ACS001     MOVE "ADDR1"                  TO WFAPI-I-VARIABLE-NAME (4).
ACS001     MOVE HR01F1-PRS-ADDR1         TO WFAPI-I-VARIABLE-VAL  (4).
ACS001     MOVE "ADDR2"                  TO WFAPI-I-VARIABLE-NAME (5).
ACS001     MOVE HR01F1-PRS-ADDR2         TO WFAPI-I-VARIABLE-VAL  (5).
ACS001     MOVE "ADDR3"                  TO WFAPI-I-VARIABLE-NAME (6).
ACS001     MOVE HR01F1-PRS-ADDR3         TO WFAPI-I-VARIABLE-VAL  (6).
ACS001*    MOVE "ADDR4"                  TO WFAPI-I-VARIABLE-NAME (7).
ACS001*    MOVE HR01F1-PRS-ADDR4         TO WFAPI-I-VARIABLE-VAL  (7).
ACS001     MOVE "NTUSER"                 TO WFAPI-I-VARIABLE-NAME (7).
ACS001     MOVE CRT-USER-NAME            TO WFAPI-I-VARIABLE-VAL  (7).
ACS001     MOVE "CITY"                   TO WFAPI-I-VARIABLE-NAME (8).
ACS001     MOVE HR01F1-PRS-CITY          TO WFAPI-I-VARIABLE-VAL  (8).
ACS001     MOVE "STATE"                  TO WFAPI-I-VARIABLE-NAME (9).
ACS001     MOVE HR01F1-PRS-STATE         TO WFAPI-I-VARIABLE-VAL  (9).
ACS001     MOVE "ZIP"                    TO WFAPI-I-VARIABLE-NAME (10).
ACS001     MOVE HR01F1-PRS-ZIP           TO WFAPI-I-VARIABLE-VAL  (10).
ACS001
ACS001     PERFORM 1000-PROCESS-FLOW.
039800******************************************************************