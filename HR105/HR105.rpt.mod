******* HR105.rpt 3 <3747276142>
******************************************************************
$PROGRAM   HR105
******************************************************************

****************************************************
$REPORT    HR105
****************************************************

**************************************
$GROUP     GN1-LOG-COMPANY
**************************************
$PREFIX    G1-
$PGBRK
$LEVEL     A
$FORMAT
HR105 Date Z9/99/99                                Company ZZZZ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          Page ZZZZ
      |  ; A1                                      |     ; B1   C1                                                      |  ; D1
      Time 99.99                                   Employee Audit Report  
      |  ; E1                                      |                   ;   
                                                   For ZZ/ZZ/ZZ   -  ZZ/ZZ/ZZ 
                                                   | ; F1       |  ; G1
$ENDFMT                            
$FIELDS                            
FIELD  NAME                   ELEMENT        TYPE  PRTCNTL     VALUES                   
 ---   --------------------   --------------  --   ----------  -------------------------
 A1    DATE                                   TD    
 B1    LOG-COMPANY                        
 C1    PRS-NAME                        
 D1    PAGE                                   PN    
 E1    TIME                                   CT    
 F1    BEG-DATE                               CD    
 G1    END-DATE                               CD    
$ENDFLDS

$ENDGRP


**************************************
$GROUP     GN2-LOG-EMPLOYEE
**************************************
$PREFIX    G2-
$LEVEL     B
$FORMAT
%
%
Employee ZZZZZZZZZ  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
|      ; A1         B1                                                
$ENDFMT
$FIELDS
FIELD  NAME                   ELEMENT        TYPE  PRTCNTL     VALUES                   
 ---   --------------------   --------------  --   ----------  -------------------------
 A1    LOG-EMPLOYEE                        
 B1    EMP-NAME                        
$ENDFLDS

$ENDGRP


**************************************
$GROUP     GN3H-LOG-SEQ-NBR
**************************************
$PREFIX    GLSN-
$LEVEL     C
$FORMAT
%
Effective Pos Lv  Field Name            Previous Value       Curr   New Value            Curr   Date Chg  Time Chg  User Id    Error
|       ; |    ;  |                  ;  |                  ; |   ;  |                  ; |   ;  |      ;  |      ;  |       ;  |   ;
--------- ------  --------------------  -------------------- -----  -------------------- -----  --------  --------  ---------  -----

$ENDFMT
$FIELDS
FIELD  NAME                   ELEMENT        TYPE  PRTCNTL     VALUES                   
 ---   --------------------   --------------  --   ----------  -------------------------
$ENDFLDS

$ENDGRP


**************************************
$GROUP     GN3D-LOG-SEQ-NBR
**************************************
$PREFIX    G3-
$LEVEL     D
$FORMAT
ZZ/ZZ/ZZ    ZZ    XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXX XXXXX  XXXXXXXXXXXXXXXXXXXX XXXXX  ZZ/ZZ/ZZ  ZZ:ZZ:ZZ  XXXXXXXXXX   X
A1          A2    B1                    C1                   C2     D1                   D2     D3        D4        E1           F1
$ENDFMT
$FIELDS
FIELD  NAME                   ELEMENT        TYPE  PRTCNTL     VALUES                   
 ---   --------------------   --------------  --   ----------  -------------------------
 A1    LOG-EFFECT-DATE                        CD
 A2    LOG-POS-LEVEL
 B1    PAD-ITEM-NAME                        
 C1    LOG-PRE-VALUE                           
 C2    LOG-PRE-CURR-CODE
 D1    LOG-NEW-VALUE                            
 D2    LOG-CURRENCY-CODE
 D3    LOG-DATE-STAMP                         CD
 D4    LOG-TIME-STAMP                         TF
 E1    LOG-USER-ID                        
 F1    LOG-ERR
$ENDFLDS

$ENDGRP

$ENDRPT

$ENDPGM
