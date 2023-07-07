******* AP.or 58.1.5.1.26 <35836844>
*** APVENCLASS

DEFINE OBJWIN    "Vendor Class"
       ID         AP-VCL-W-0001
       FILENAME   APVENCLASS
       WINFLDS    DESCRIPTION         :"            Description:"
                  CURRENCY-CODE       :"       Invoice Currency:"
                  POST-OPTION         :"         Posting Option:"
                  INCOME-CODE         :"            Income Code:"
                  TAX-CODE            :"               Tax Code:"
                  TAX-USAGE-CD        :"         Tax Usage Code:"
                  DIST-CODE           :"      Distribution Code:"
                  HANDLING-CODE       :"  Invoice Handling Code:"
                  VALIDATE-PO         :"Validate Purchase Order:"
                  REQUIRE-PO          :" Require Purchase Order:"
                  TAX-OVERRIDE        :"           Tax Override:"

DEFINE OBJWIN    "Vendor Class Options"
       ID         AP-VCL-W-0002
       FILENAME   APVENCLASS
       WINFLDS    CASH-CODE           :"              Cash Code:"
                  BANK-INST-CODE      :"           Payment Code:"
                  VEN-PRIORITY        :"       Payment Priority:"
                  ZERO-CHECK          :"  Zero Payments Allowed:"
                  ZERO-PMT-CODE       :"      Zero Payment Code:"
                  MAX-PMT-AMT         :" Maximum Payment Amount:"
                  MAX-PMT-HOLD        :"      Maximum Hold Code:"
                  ENCLOSURE           :"              Enclosure:"

DEFINE OBJWIN    "Vendor Class Match"
       ID         AP-VCL-W-0003
       FILENAME   APVENCLASS
       WINFLDS    MATCH-TABLE         :"Match Table:"
                  RULE-GROUP          :" Rule Group:"

*** APVENGROUP

DEFINE OBJWIN    "Vendor Group"
       ID         AP-VGP-W-0001
       FILENAME   APVENGROUP
       WINFLDS    DESCRIPTION         :"           Description:"

                  NUM-VEN-NBR         :"      Vendor Numbering:"
                  AUTO-VENDOR         :" Auto Vendor Numbering:"
                  LAST-VENDOR         :"  Beginning Vendor Nbr:"

                  VENDOR-AUDIT        :"  Audit Vendor Changes:"


DEFINE OBJWIN    "Vendor Group Period Ending Dates"
      ID         AP-VGP-W-0002
      FILENAME   APVENGROUP
      WINFLDS    GRP-END-DTE-01   :"       Period 01:"
                 GRP-END-DTE-02   :"       Period 02:"
                 GRP-END-DTE-03   :"       Period 03:"
                 GRP-END-DTE-04   :"       Period 04:"
                 GRP-END-DTE-05   :"       Period 05:"
                 GRP-END-DTE-06   :"       Period 06:"
                 GRP-END-DTE-07   :"       Period 07:"
                 GRP-END-DTE-08   :"       Period 08:"
                 GRP-END-DTE-09   :"       Period 09:"
                 GRP-END-DTE-10   :"       Period 10:"
                 GRP-END-DTE-11   :"       Period 11:"
                 GRP-END-DTE-12   :"       Period 12:"
                 GRP-END-DTE-13   :"       Period 13:"

DEFINE OBJVIEW   "Vendor Group Companies"
      ID         AP-VGP-V-0001
      FILENAME   APVENGROUP
      RELATION   APCOMPANY
      DSPFLDS    COMPANY:"Co", NAME:"Name", BASE-CURRENCY:"Curr"

*** APVENMAST ***

DEFINE OBJWIN    "Address"
       ID         AP-VEN-W-0001
       FILENAME   APVENMAST
       WINFLDS    VENDOR-VNAME:           "       Name:"
                  LEGAL-NAME=60:          " Legal Name:"
                  ADDRESS1.EFFECTIVE-DATE:"Effect Date:"         

                  ADDRESS1.ADDR1:         "    Address:"
                  ADDRESS1.ADDR2:         "            "
                  ADDRESS1.ADDR3:         "            "
                  ADDRESS1.ADDR4:         "            "
                  ADDRESS1.CITY-ADDR5:    "            ",ADDRESS1.STATE-PROV:" ",ADDRESS1.POSTAL-CODE:" "
                  ADDRESS1.COUNTY         "      County"
                  ADDRESS1.COUNTRY:       "            "
                  VENDOR-CONTCT:          "    Contact:"
                  PHONE-PREFIX:           "  Telephone:",PHONE-NUM:" ",PHONE-EXT:" "
                  FAX-PREFIX:             "        Fax:",FAX-NUM:"   ",FAX-EXT:"   "
                  VEND-ACCT:              " Account Nbr:"
                  E-MAIL-ADDRESS:         "      E-mail:"
                  URL-ADDR:               "         URL:"
                  GLN-NBR:                "  GLN Number:"

DEFINE OBJWIN    "Address"
       ID         AP-VPY-W-0001
       FILENAME   APVENPAY
       WINFLDS    APVENMAST.VENDOR-VNAME:           "       Name:"
                  APVENMAST.LEGAL-NAME=60:          " Legal Name:"
                  ADDRESS1.EFFECTIVE-DATE:"Effect Date:"         

                  ADDRESS1.ADDR1:         "    Address:"
                  ADDRESS1.ADDR2:         "            "
                  ADDRESS1.ADDR3:         "            "
                  ADDRESS1.ADDR4:         "            "
                  ADDRESS1.CITY-ADDR5:    "            ",ADDRESS1.STATE-PROV:" ",ADDRESS1.POSTAL-CODE:" "
                  ADDRESS1.COUNTY         "      County"
                  ADDRESS1.COUNTRY:       "            "
                  APVENMAST.VENDOR-CONTCT:          "    Contact:"
                  APVENMAST.PHONE-PREFIX:           "  Telephone:",APVENMAST.PHONE-NUM:" ",APVENMAST.PHONE-EXT:" "
                  APVENMAST.FAX-PREFIX:             "        Fax:",APVENMAST.FAX-NUM:"   ",APVENMAST.FAX-EXT:"   "
                  APVENMAST.VEND-ACCT:              " Account Nbr:"
                  APVENMAST.E-MAIL-ADDRESS:         "      E-mail:"
                  APVENMAST.URL-ADDR:               "         URL:"

DEFINE OBJWIN    "Vendor Origin"
       ID         AP-VEN-W-0003
       FILENAME   APVENMAST
       WINFLDS    OPERATOR      :"            Operator:"
                  CREATE-DATE   :"       Creation Date:"

DEFINE OBJWIN    "Vendor Origin"
       ID         AP-VPY-W-0002
       FILENAME   APVENPAY
       WINFLDS    APVENMAST.OPERATOR      :"            Operator:"
                  APVENMAST.CREATE-DATE   :"       Creation Date:"

DEFINE OBJWIN    "Tax ID"
       ID         AP-VEN-W-0004
       FILENAME   APVENMAST
       VALIDITY   TAX-VENDOR
       WINFLDS    TAX-CODE     :"             Tax Code:"
                  TAX-ID       :"   Tax Identification:"
                  TAX-USAGE-CD :"       Tax Usage Code:"
                  VAT-REG-CTRY :"      VAT Reg Country:"
                  VAT-REG-NBR  :"       VAT Reg Number:"

DEFINE OBJWIN    "Reportable Income Tax ID"
       ID         AP-VEN-W-0005
       FILENAME   APVENMAST
       VALIDITY   REP-INC-VENDOR
       WINFLDS    INCOME-CODE    :"           Income Code:"
                  TAX-ID         :"    Tax Identification:"
                  INCOME-WH-FLG  :"    Income Withholding:"

DEFINE OBJWIN    "Vendor Defaults"
       ID         AP-VEN-W-0006
       FILENAME   APVENMAST
       WINFLDS    VENDOR-VNAME  :"                 Name:"
                  BANK-INST-CODE:"         Payment Code:"
                  CASH-CODE     :"            Cash Code:"
                  TERM-CODE     :"        Payment Terms:"
                  DIST-CODE     :"    Distribution Code:"
                  ACCR-CODE     :" Invoice Accrual Code:"
                  INCOME-CODE   :"          Income Code:"
                  DISCOUNT-CODE :"        Discount Code:"
                  INVOICE-GROUP :"        Invoice Group:"
                  AUTH-CODE     :"       Authority Code:"
                  TAX-CODE      :"             Tax Code:"
                  TAX-USAGE-CD  :"       Tax Usage Code:"
                  ACTIVITY      :"             Activity:",ACCT-CATEGORY:"  "     
                  HANDLING-CODE :"        Handling Code:"
                  MATCH-TABLE   :"          Match Table:",RULE-GROUP:"Rule Group:"
                  POOL-OPTION   :"          Pool Option:"
                  REQ-MATCH-REF :" Match Ref Processing:"

DEFINE OBJWIN    "Vendor Defaults"
       ID         AP-VPY-W-0003
       FILENAME   APVENPAY 
       WINFLDS    APVENMAST.VENDOR-VNAME  :"                 Name:"
                  APVENMAST.BANK-INST-CODE:"         Payment Code:"
                  APVENMAST.CASH-CODE     :"            Cash Code:"
                  APVENMAST.TERM-CODE     :"        Payment Terms:"
                  APVENMAST.DIST-CODE     :"    Distribution Code:"
                  APVENMAST.ACCR-CODE     :" Invoice Accrual Code:"
                  APVENMAST.INCOME-CODE   :"          Income Code:"
                  APVENMAST.DISCOUNT-CODE :"        Discount Code:"
                  APVENMAST.INVOICE-GROUP :"        Invoice Group:"
                  APVENMAST.AUTH-CODE     :"       Authority Code:"
                  APVENMAST.TAX-CODE      :"             Tax Code:"
                  APVENMAST.TAX-USAGE-CD  :"       Tax Usage Code:"
                  APVENMAST.ACTIVITY      :"             Activity:",APVENMAST.ACCT-CATEGORY:"  "     
                  APVENMAST.HANDLING-CODE :"        Handling Code:"
                  APVENMAST.MATCH-TABLE   :"          Match Table:",APVENMAST.RULE-GROUP:"Rule Group:"
                  APVENMAST.POOL-OPTION   :"          Pool Option:"
                  APVENMAST.REQ-MATCH-REF :" Match Ref Processing:"

DEFINE OBJWIN    "Vendor Options"
       ID         AP-VEN-W-0007
       FILENAME   APVENMAST
       WINFLDS    VENDOR-VNAME  :"                 Name:"
                  VENDOR-STATUS :"               Status:"

               APHLDINV.HLD-CODE:"            Hold Code:"
                  INV-CURRENCY  :"     Invoice Currency:"
                  BAL-CURRENCY  :"     Balance Currency:"
                  CURR-RECALC   :"              Revalue:"

                  INCOME-WH-FLG :"   Income Withholding:"
                  ACH-PRENOT    :"  ACH Prenotification:"
                  ERS-CAPABLE   :"          ERS Capable:"
                  
                  DISC-CALC-DATE:"    Terms Calculation:"
                  POOL-OPTION   :"          Pool Option:"
                  REQ-MATCH-REF :" Match Ref Processing:"    
                  VALIDATE-PO   :" Validate Purch Order:"
                  REQUIRE-PO    :"  Require Purch Order:"
                  P-CARD-FLAG   :"        P-Card Vendor:"
                  PCARD-NBR     :"        P-Card Number:"

DEFINE OBJWIN    "Vendor Options"
       ID         AP-VPY-W-0004
       FILENAME   APVENPAY
       WINFLDS    APVENMAST.VENDOR-VNAME  :"                 Name:"
                  APVENMAST.VENDOR-STATUS :"               Status:"

               APHLDINV1.HLD-CODE:"            Hold Code:"
                  APVENMAST.INV-CURRENCY  :"     Invoice Currency:"
                  APVENMAST.BAL-CURRENCY  :"     Balance Currency:"
                  APVENMAST.CURR-RECALC   :"              Revalue:"

                  APVENMAST.INCOME-WH-FLG :"   Income Withholding:"
                  APVENMAST.ACH-PRENOT    :"  ACH Prenotification:"
                  APVENMAST.ERS-CAPABLE   :"          ERS Capable:"
                  
                  APVENMAST.DISC-CALC-DATE:"    Terms Calculation:"
                  APVENMAST.POOL-OPTION   :"          Pool Option:"
                  APVENMAST.REQ-MATCH-REF :" Match Ref Processing:"    
                  APVENMAST.VALIDATE-PO   :" Validate Purch Order:"
                  APVENMAST.REQUIRE-PO    :"  Require Purch Order:"
                  APVENMAST.P-CARD-FLAG   :"        P-Card Vendor:"
                  APVENMAST.PCARD-NBR     :"        P-Card Number:"

DEFINE OBJVIEW   "Vendor Locations"
       ID         AP-VEN-V-0001
       FILENAME   APVENMAST
       VALIDITY   LOCATION-EXIST
       RELATION   APVENLOC
       DSPFLDS    LOCATION-CODE:"Code",VENDOR-VNAME:"Name", LOC-TYPE:"Type"

DEFINE OBJVIEW   "Vendor Locations"
       ID         AP-VPY-V-0001
       FILENAME   APVENPAY
       VALIDITY   LOC1-EXIST
       RELATION   APVENLOC1
       DSPFLDS    LOCATION-CODE:"Code",VENDOR-VNAME:"Name", LOC-TYPE:"Type"

DEFINE OBJVIEW   "Vendor Contacts"
       ID         AP-VCT-V-0001 
       FILENAME   APVENMAST
       RELATION   APCONTACTS  
       DSPFLDS    RESP-CODE:"Contact Code", CONTACT-LVL:"Contact Lvl",NAME:"Contact Name"

DEFINE OBJVIEW   "Vendor Location Contacts"
       ID         AP-VCT-V-0002 
       FILENAME   APVENLOC 
       RELATION   APCONTACTS  
       DSPFLDS    RESP-CODE:"Contact Code", CONTACT-LVL:"Contact Lvl",NAME:"Contact Name"

DEFINE OBJVIEW   "Comments"
       ID         AP-VEN-V-0002
       FILENAME   APVENMAST
       VALIDITY   COMMENT-EXIST
       RELATION   APVCOMMENTS
       DSPFLDS    PRINT-CODE:"Type", BEG-DATE:"From", END-DATE:"To"

DEFINE OBJVIEW   "Comments"
       ID         AP-VPY-V-0002
       FILENAME   APVENPAY
       VALIDITY   COMMENT1-EXIST
       RELATION   APVCOMMENTS1
       DSPFLDS    COMMENT:""

DEFINE OBJWIN     "User Fields"
       ID         AP-VEN-W-0008
       FILENAME   APVENMAST
       WINFLDS    USER-NAME-01:"User Field 1:"
                  USER-NAME-02:"User Field 2:"
                  USER-NAME-03:"User Field 3:"
                  USER-NAME-04:"User Field 4:"
                  USER-NAME-05:"User Field 5:"
                  USER-NAME-06:"User Field 6:"

DEFINE OBJWIN     "User Fields"
       ID         AP-VPY-W-0005   
       FILENAME   APVENPAY
       WINFLDS    APVENMAST.USER-NAME-01:"User Field 1:"
                  APVENMAST.USER-NAME-02:"User Field 2:"
                  APVENMAST.USER-NAME-03:"User Field 3:"
                  APVENMAST.USER-NAME-04:"User Field 4:"
                  APVENMAST.USER-NAME-05:"User Field 5:"

DEFINE OBJWIN     "Vendor Certification"
       ID         AP-AVT-W-001 
       FILENAME   APVENCERT 
       WINFLDS    VENDOR :       "  Vendor Name:"
                  LOCATION-CODE: "Location Code:"
                  CERTIF-CODE    "    Cert Code:"
                  CERTIF-DESC:   "    Cert Desc:"
                  CERTIF-EFF-DT  "Cert Eff Date:"
                  CERTIF-EXP-DT  "Cert Exp Date:"                             

DEFINE OBJWIN     "Vendor Certification Comments"
       ID         AP-AVT-W-0001
       FILENAME   APVENCERT 
       COMMENT    TYPE=A
       WINFLDS    VENDOR-GROUP : " Vendor Group:"
                  VENDOR       : "       Vendor:"
                  LOCATION-CODE: "     Location:"
                  CERTIF-CODE  : "    Cert-Code:"
                  CERTIF-EFF-DT: "Cert-eff-date:"

DEFINE OBJWIN     "Vendor Certification"
       ID         AP-CVN-W-0001 
       FILENAME   APCVENCERT
       WINFLDS    VENDOR :       "  Vendor Name:"
                  LOCATION-CODE: "Location Code:"
                  CERTIF-CODE    "    Cert Code:"
                  CERTIF-DESC:   "    Cert Desc:"
                  CERTIF-EFF-DT  "Cert Eff Date:"
                  CERTIF-EXP-DT  "Cert Exp Date:"                             

DEFINE OBJWIN     "Vendor Certification Comments"
       ID         AP-CVN-W-0002
       FILENAME   APCVENCERT
       COMMENT    TYPE=A
       WINFLDS    VENDOR-GROUP : " Vendor Group:"
                  VENDOR       : "       Vendor:"
                  LOCATION-CODE: "     Location:"
                  CERTIF-CODE  : "    Cert-Code:"
                  CERTIF-EFF-DT: "Cert-eff-date:"



DEFINE OBJWIN     "Vendor Contact"
       ID         AP-VCT-W-0001 
       FILENAME   APCONTACT 
       WINFLDS    VENDOR :       "  Vendor Name:"
                  LOCATION-CODE: "Location Code:"
                  RESP-CODE:     " Contact Code:"
                  CONTACT-LVL:   "Contact Level:"
                  NAME:          " Contact Name:"
                  INT-PREFIX:    "    Telephone:",PHONE-NMBR:" ",PHONE-EXT:" "
                  MBL-INT-PREFIX:" Mobile phone:",MOBILE-NUM:" ",MOBILE-EXT:" "
                  FAX-PREFIX:    "          Fax:",FAX-NMBR:"   ",FAX-EXT:"   "
                  E-MAIL-ADDRESS:"       E-mail:"

DEFINE OBJWIN     "Vendor Contact Address"
       ID         AP-VDR-W-0001
       FILENAME   APVENADDR 
       WINFLDS    VENDOR :       "  Vendor Name:"
                  LOCATION-CODE: "Location Code:"
                  EFFECTIVE-DATE:"Effective Date:"
                  ADDR1     :    "       Address:"
                  ADDR2     :    "               "
                  ADDR3     :    "               "
                  ADDR4     :    "               "                              
                  CITY-ADDR5:    "          City:"                              
                  STATE-PROV:    "         State:",POSTAL-CODE:":"       
                  COUNTRY   :    "       Country:"


DEFINE OBJWIN     "Vendor Contact Comments"
       ID         AP-VCT-W-0002
       FILENAME   APCONTACT 
       COMMENT    TYPE=A
       WINFLDS    VENDOR-GROUP : " Vendor Group:"
                  VENDOR :       "       Vendor:"
                  LOCATION-CODE: "Location Code:"
                  RESP-CODE:     " Contact Code:"
                  CONTACT-LVL:   "Contact Level:"

DEFINE OBJWIN     "Vendor Contact"
       ID         AP-CVH-W-0001
       FILENAME   APCCONTACT
       WINFLDS    VENDOR :       "  Vendor Name:"
                  LOCATION-CODE: "Location Code:"
                  RESP-CODE:     " Contact Code:"
                  CONTACT-LVL:   "Contact Level:"
                  NAME:          " Contact Name:"
                  INT-PREFIX:    "    Telephone:",PHONE-NMBR:" ",PHONE-EXT:" "
                  MBL-INT-PREFIX:" Mobile phone:",MOBILE-NUM:" ",MOBILE-EXT:" "
                  FAX-PREFIX:    "          Fax:",FAX-NMBR:"   ",FAX-EXT:"   "
                  E-MAIL-ADDRESS:"       E-mail:"

DEFINE OBJWIN     "Vendor Conversion Contact Comments"
       ID         AP-CVH-W-0002
       FILENAME   APCCONTACT 
       COMMENT    TYPE=A
       WINFLDS    VENDOR-GROUP : " Vendor Group:"
                  VENDOR :       "       Vendor:"
                  LOCATION-CODE: "Location Code:"
                  RESP-CODE:     " Contact Code:"
                  CONTACT-LVL:   "        Level:"

DEFINE OBJVIEW   "Current Vendor Balances"
       ID         AP-VEN-V-0003
       FILENAME   APVENMAST
       RELATION   APVENDORBAL
       DSPFLDS    LOCATION-CODE:"Loc", COMPANY:"Co", CURRENT-BAL:"Invoice Balance", DRAFT-BAL:"BOE Balance", BAL-CURRENCY:""
 
DEFINE OBJWIN    "AP Vendor URL Attachment"
       ID         AP-VEN-W-0009
       FILENAME   APVENMAST
       URL        TYPE=I
       URLSCHEME "http"
       WINFLDS    VENDOR-GROUP:"Vendor Group:"
                  VENDOR:      "      Vendor:"
                  VENDOR-VNAME:" Vendor Name:"

DEFINE OBJVIEW "Vendor Unpaid Invoices"
      ID        AP-VEN-V-0004
      FILENAME  APVENMAST
      RELATION  APINVOICE
      CONDITION OPEN-INVOICES
      DSPFLDS   COMPANY:"Cpy", INVOICE:"Invoice", SUFFIX:"", DUE-DATE:"Due Date",
                TRAN-INV-AMT:"Amount Due", INV-CURRENCY:"", INV-STATUS:"Status"
 
DEFINE OBJVIEW "Vendor Paid Invoices"
      ID        AP-VEN-V-0005
      FILENAME  APVENMAST
      RELATION  APINVOICE
      CONDITION INV-HISTORY
      DSPFLDS   COMPANY:"Co", INVOICE:"Invoice", SUFFIX:"",
                TRAN-INV-AMT:"Invoice Amt",TRAN-DISC-AMT:"Discount Amt", INV-CURRENCY:""
 
DEFINE OBJVIEW "Archived Vendor Paid Invoices"
      ID        AP-VEN-V-0006
      FILENAME  APVENMAST
      RELATION  APAPIHIST
      CONDITION INV-HISTORY
      DSPFLDS   COMPANY:"Co", INVOICE:"Invoice", SUFFIX:"",
                TRAN-INV-AMT:"Invoice Amt",TRAN-DISC-AMT:"Discount Amt", INV-CURRENCY:""

DEFINE OBJVIEW "Vendor Cash Payments"
     ID         AP-VEN-V-0007
     FILENAME   APVENMAST
     VALIDITY   CHECK-EXISTS
     RELATION   CBCHECK
     DSPFLD     COMPANY:"Co", CHECK-DATE:"Pmt Date", TRANS-NBR:"Pmt Nbr", BANK-INST-CODE:"", BANK-CURR-AMT:"Payment Amount", CBCASHCODE.CURRENCY-CODE:""

DEFINE OBJVIEW "Vendor Company Defaults"
     ID         AP-VEN-V-0008
     FILENAME   APVENMAST
     VALIDITY   VDRCPY-EXISTS
     RELATION   APCPYVND
     DSPFLDS    COMPANY:"Company",VENDOR:"Vendor",LOCATION-CODE:"Location"

DEFINE OBJVIEW "Pay Vendors"
     ID         AP-VEN-V-0009
     FILENAME   APVENMAST
     RELATION   APVENPAY
     DSPFLDS    PAY-VENDOR:"Pay Vendor",PAY-TYPE:"Pay Type",DEFAULT:"Default"

DEFINE OBJWIN  "Vendor Company Codes"
    ID         AP-APX-V-0001
    FILENAME   APCPYVND
    WINFLDS    COMPANY       :"        Company:"

               HANDLING-CODE :"       Handling:"
               ACCR-CODE     :"        Accrual:"
               DISC-CODE     :"       Discount:"
               DIST-CODE     :"   Distribution:"
               AUTH-CODE     :"      Authority:"
               INCOME-CODE   :"         Income:"
               TAX-CODE      :"            Tax:"
               TAX-USAGE-CD  :"      Tax Usage:"

               TERM-CODE     :"          Terms:"
               CASH-CODE     :"           Cash:"
               BANK-INST-CODE:"        Payment:"

DEFINE OBJWIN   "Vendor Location User Fields"
       ID        AP-VLO-W-0002
       FILENAME  APVENLOC
       WINFLDS   USER-NAME-01:"User Field 1"
                 USER-NAME-02:"User Field 2"
                 USER-NAME-03:"User Field 3"
                 USER-NAME-04:"User Field 4"
                 USER-NAME-05:"User Field 5"
                 USER-NAME-06:"User Field 6"

*DEFINE OBJWIN     "Vendor AR Contact E-mail Addresses"
*       ID         AP-VEN-W-0009
*       FILENAME   APVENMAST
*       URL        TYPE=A
*       URLSCHEME  "Mailto"
*       WINFLDS    VENDOR-CONTCT:"Contact Name:"

*DEFINE OBJWIN     "Vendor Home Pages"
*       ID         AP-VEN-W-0010
*       FILENAME   APVENMAST
*       URL        TYPE=H
*       URLSCHEME  "http"
*       WINFLDS    VENDOR-VNAME:"Vendor Name:"

DEFINE OBJWIN    "Invoice URL Attachment"
       ID         AP-API-W-0001
       FILENAME   APINVOICE
       URL        TYPE=I
       URLSCHEME "http"
       WINFLDS    VENDOR:     "Vendor :"
                  INVOICE:    "Invoice:"
                  SUFFIX:     "Suffix :"

DEFINE OBJWIN    "Archived Invoice URL Attachment"
       ID         AP-APH-W-0001
       FILENAME   APAPIHIST
       URL        TYPE=I
       URLSCHEME "http"
       WINFLDS    VENDOR:     "Vendor :"
                  INVOICE:    "Invoice:"
                  SUFFIX:     "Suffix :"

*** APVENLOC ***
DEFINE OBJWIN    "Address"
       ID         AP-VLO-W-0001
       FILENAME   APVENLOC
       WINFLDS    VENDOR-VNAME:            "       Name:"

                  ADDRESS1.EFFECTIVE-DATE: "Effect Date:"

                  ADDRESS1.ADDR1:          "    Address:"
                  ADDRESS1.ADDR2:          "            "
                  ADDRESS1.ADDR3:          "            "
                  ADDRESS1.ADDR4:          "            "
                  ADDRESS1.CITY-ADDR5:     "            ",ADDRESS1.STATE-PROV:" ",ADDRESS1.POSTAL-CODE:" "
                  ADDRESS1.COUNTY          "      County"
                  ADDRESS1.COUNTRY:        "            "
                  VENDOR-CONTCT:           "    Contact:"
                  PHONE-PREFIX:            "  Telephone:",PHONE-NUM:" ",PHONE-EXT:" "
                  FAX-PREFIX:              "        Fax:",FAX-NUM:"   ",FAX-EXT:"   "

                  VEND-ACCT:               "Account Nbr:"
                  E-MAIL-ADDRESS:          "     E-mail:"

DEFINE OBJVIEW   "Location Balance"
       ID         AP-VLO-V-0001
       FILENAME   APVENLOC
       RELATION   APLOCBAL
       DSPFLDS    COMPANY:"Co", CURRENT-BAL:"Current Balance", DRAFT-BAL:"BOE Balance", BAL-CURRENCY:""

DEFINE OBJVIEW   "Comments"
       ID         AP-VLO-V-0002
       FILENAME   APVENLOC
       VALIDITY   COMMENT-EXIST
       RELATION   APCOMMENTS
       DSPFLDS    PRINT-CODE:"Type", BEG-DATE:"From", END-DATE:"To Date"

DEFINE OBJVIEW "Location Company Defaults"
     ID         AP-VLO-V-0003
     FILENAME   APVENLOC
     RELATION   APCPYVND
     DSPFLD     COMPANY:"Co", TERM-CODE:"Term", TAX-CODE:"Tax", DIST-CODE:"Dist", ACCR-CODE:"Accrual",
                DISC-CODE:"Discount", CASH-CODE:"Cash", INCOME-CODE:"Inc", AUTH-CODE:"Auth", BANK-INST-CODE:"Pmt",
                TAX-USAGE-CD:"Tax Usage"

*** APVENBAL ***

DEFINE OBJWIN    "Current Year Payments/Nbr of Pmts"
       ID         AP-VBA-W-0001
       FILENAME   APVENBAL
       WINFLDS    BAL-CURRENCY:" Currency:"

                  CP-PMT-01   :" Period 1:",CP-NBR-PMTS-01:" "
                  CP-PMT-02   :" Period 2:",CP-NBR-PMTS-02:" "
                  CP-PMT-03   :" Period 3:",CP-NBR-PMTS-03:" "
                  CP-PMT-04   :" Period 4:",CP-NBR-PMTS-04:" "
                  CP-PMT-05   :" Period 5:",CP-NBR-PMTS-05:" "
                  CP-PMT-06   :" Period 6:",CP-NBR-PMTS-06:" "
                  CP-PMT-07   :" Period 7:",CP-NBR-PMTS-07:" "
                  CP-PMT-08   :" Period 8:",CP-NBR-PMTS-08:" "
                  CP-PMT-09   :" Period 9:",CP-NBR-PMTS-09:" "
                  CP-PMT-10   :"Period 10:",CP-NBR-PMTS-10:" "
                  CP-PMT-11   :"Period 11:",CP-NBR-PMTS-11:" "
                  CP-PMT-12   :"Period 12:",CP-NBR-PMTS-12:" "
                  CP-PMT-13   :"Period 13:",CP-NBR-PMTS-13:" "

DEFINE OBJWIN    "Current Year Discounts Taken/Lost"
       ID         AP-VBA-W-0002
       FILENAME   APVENBAL
       WINFLDS    BAL-CURRENCY:" Currency:"

                  CP-DISC-TK-01:"     Period 1:", CP-DISC-LST-01:""
                  CP-DISC-TK-02:"     Period 2:", CP-DISC-LST-02:""
                  CP-DISC-TK-03:"     Period 3:", CP-DISC-LST-03:""
                  CP-DISC-TK-04:"     Period 4:", CP-DISC-LST-04:""
                  CP-DISC-TK-05:"     Period 5:", CP-DISC-LST-05:""
                  CP-DISC-TK-06:"     Period 6:", CP-DISC-LST-06:""
                  CP-DISC-TK-07:"     Period 7:", CP-DISC-LST-07:""
                  CP-DISC-TK-08:"     Period 8:", CP-DISC-LST-08:""
                  CP-DISC-TK-09:"     Period 9:", CP-DISC-LST-09:""
                  CP-DISC-TK-10:"    Period 10:", CP-DISC-LST-10:""
                  CP-DISC-TK-11:"    Period 11:", CP-DISC-LST-11:""
                  CP-DISC-TK-12:"    Period 12:", CP-DISC-LST-12:""
                  CP-DISC-TK-13:"    Period 13:", CP-DISC-LST-13:""

DEFINE OBJVIEW  "Current Balance Detail"
       ID        AP-VBA-V-0001
       FILENAME  APVENBAL
       VALIDITY  APVENMAST
       RELATION  APPAYMENT
       CONDITION CURRENT-BAL
       DSPFLDS   INVOICE:"Invoice", SUFFIX:"", DUE-DATE:"Due Date", TRAN-PMT-AMT:"Amount Due",
                 INV-CURRENCY:" "

DEFINE OBJVIEW  "Remit To Location Balance Detail"
       ID        AP-VBA-V-0002
       FILENAME  APVENBAL
       VALIDITY  APVENLOC
       RELATION  APPAYMENT
       CONDITION CURR-BAL-LOC
       DSPFLDS   REMIT-TO-CODE:"Loc", INVOICE:"Invoice", SUFFIX:"", DUE-DATE:"Due Date", TRAN-PMT-AMT:"Amount Due",
                 INV-CURRENCY:" "


DEFINE OBJVIEW "BOE Balance Detail"
      ID        AP-VBA-V-0003
      FILENAME  APVENBAL
      VALIDITY  APVENMAST
      RELATION  APDRAFTS
      CONDITION DRAFT-BAL
      DSPFLDS   DRAFT-NBR:"BOE Nbr", SUFFIX:"", BANK-INST-CODE:"", ACCEPT-DATE:"Accept Date", MATURITY-DATE:"Due Date",
                DRAFT-AMOUNT:"Amount"

DEFINE OBJVIEW "Remit To Location BOE Balance Detail"
      ID        AP-VBA-V-0004
      FILENAME  APVENBAL
      VALIDITY  APVENLOC
      RELATION  APDRAFTS
      CONDITION DRAFT-BAL-LOC
      DSPFLDS   REMIT-TO-CODE:"Loc", DRAFT-NBR:"BOE Nbr", SUFFIX:"", BANK-INST-CODE:"", ACCEPT-DATE:"Accept Date", MATURITY-DATE:"Due Date",
                DRAFT-AMOUNT:"Amount"

*** APCOMPANY ***

DEFINE OBJWIN     "Address"
       ID          AP-CPY-W-0001
       FILENAME    APCOMPANY
       WINFLDS     ADDRDATA.NAME      :"          Name:"
                   ADDRDATA.ADDR1     :"       Address:"
                   ADDRDATA.ADDR2     :"               "
                   ADDRDATA.ADDR3     :"               "
                   ADDRDATA.ADDR4     :"               "
                   ADDRDATA.ADDR5     :"               ",ADDRDATA.STATE-PROV:" ",ADDRDATA.POSTAL-CODE:""
                   ADDRDATA.COUNTY    :"         County"
                   ADDRDATA.COUNTRY   :"               "

DEFINE OBJWIN    "Last Batch Number"
       ID         AP-CPY-W-0002
       FILENAME   APCOMPANY
       VALIDITY   BATCH-COMPANY
       WINFLDS    LAST-BATCH    :"        Last Batch Number Used:"

DEFINE OBJWIN    "Last Voucher"
       ID         AP-CPY-W-0003
       FILENAME   APCOMPANY
       VALIDITY   HAS-LST-VOUCHR
       WINFLDS    LAST-VOUCHER  :"          Last Voucher:"

DEFINE OBJWIN    "Options"
       ID         AP-CPY-W-0004
       FILENAME   APCOMPANY
       WINFLDS    BASE-CURRENCY :"                     Base Currency:"

                  INV-APPROVAL  :"                  Invoice Approval:"
                  BATCH-OPTION  :"                     Batch Release:"
                  WORKFLOW      :"                          Workflow:"

                  POST-OPTION   :"     General Ledger Posting Option:"
                  INVOICE-AUDIT :"              Audit Invoice Change:"
                  ADV-EXP-OPT   :"   Employee Expense Reconciliation:"

                  CASH-CODE     :"                         Cash Code:"
                  ACCR-CODE     :"              Invoice Accrual Code:"
                  DISCOUNT-CODE :"                     Discount Code:"
                  INC-ACCR-CODE :"               Income Accrual Code:"

                  CREATE-TR-DIST:"      Create Transit Distributions:"
                  DIST-CODE     :"                 Distribution Code:"

DEFINE OBJVIEW   "Cash Codes"
       ID         AP-CPY-V-0001
       FILENAME   APCOMPANY
       RELATION   CBCPYCASH
       DSPFLDS    CASH-CODE:"Code", CBCASHCODE.CURRENCY-CODE:"Curr", CBCASHCODE.BANK-ACCT-NBR:"Account Number", CBCASHCODE.DESCRIPTION:"Description"

DEFINE OBJVIEW   "Monitor"
       ID         AP-CPY-V-0002
       FILENAME   APCOMPANY
       VALIDITY   MONITOR-EXISTS
       RELATION   APMONITOR
       DSPFLDS    RUN-PROG:"Program", PROC-GRP:"Proc Group", PROC-LEVEL:"PL", BATCH-NUM:"Batch",
                  AUTH-CODE:"Auth"

DEFINE OBJVIEW    "Unaccepted BOE"
       ID          AP-CPY-V-0003
       FILENAME    APCOMPANY
       VALIDITY    DRAFT-UNACCEPT
       RELATION    DRAFT-UNACCEPT
       DSPFLDS     VENDOR-VNAME:"Vendor Name", DRAFT-NBR:"BOE",
                   BANK-INST-CODE:"", MATURITY-DATE:"Due Date", DRAFT-AMOUNT:"Amount"
 
DEFINE OBJVIEW    "Unreleased BOE"
       ID          AP-CPY-V-0004
       FILENAME    APCOMPANY
       VALIDITY    DRAFT-UNREL
       RELATION    DRAFT-UNREL
       DSPFLDS     VENDOR-VNAME:"Vendor Name", DRAFT-NBR:"BOE", SUFFIX:"",
                   BANK-INST-CODE:"Typ", DRAFT-AMOUNT:"Amount"

DEFINE OBJVIEW    "Accepted BOE"
       ID          AP-CPY-V-0005
       FILENAME    APCOMPANY
       VALIDITY    DRAFT-ACCEPTED
       RELATION    DRAFT-ACCEPTED
       DSPFLDS     VENDOR-VNAME:"Vendor Name", DRAFT-NBR:"BOE",
                   BANK-INST-CODE:"", MATURITY-DATE:"Due Date", DRAFT-AMOUNT:"Amount"

DEFINE OBJVIEW    "Unclosed Cash Payments"
      ID           AP-CPY-V-0006
      FILENAME     APCOMPANY
      RELATION     CBCHECK
      CONDITION    UNCLOSE-CHECK
      DSPFLDS      TRANS-NBR:"Payment Nbr", BANK-INST-CODE:"Typ", CASH-CODE:"Bank",
                   CHECK-DATE:"Pmt Date", BANK-CURR-AMT:"Payment Amount"

*** APMONITOR ***

DEFINE OBJWIN    "Monitor"
       ID         AP-MON-W-0001
       FILENAME   APMONITOR
       WINFLDS    PAY-GROUP     :"     Pay Group:"
                  PROC-GRP      :" Process Group:"
                  COMPANY       :"       Company:"
                  RUN-PROG      :"       Program:"
                  PROC-LEVEL    :" Process Level:"
                  BATCH-NUM     :"         Batch:"
                  AUTH-CODE     :"     Authority:"
                  REC-STATUS    :"        Status:"
                  CREATE-DATE   :"          Date:"
                  CREATION-TIME :"          Time:"

*** APBATCH ***

DEFINE OBJWIN     "Batch Totals"
       ID          AP-APB-W-0001
       FILENAME    APBATCH
       WINFLDS     BATCH-DATE   :"                       Batch Date:"

                   ACT-NBR-INV  :"               Number of Invoices:"
                   CTL-NBR-INV  :"       Control Number of Invoices:"

                   ACT-NET-AMT  :"                  Net Amount:"
                   CTL-NET-TOT  :"          Control Net Amount:"

                   ACT-VEND-HASH:"                 Vendor Hash:"
                   CTL-VEND-HASH:"         Control Vendor Hash:"

DEFINE OBJVIEW    "Batched Invoices"
       ID          AP-APB-V-0001
       FILENAME    APBATCH
       RELATION    APIBATCH
       DSPFLDS     APVENMAST.VENDOR-VNAME:"Vendor",INVOICE:"Invoice",TRAN-INV-AMT:"Invoice Amount",
                   INV-CURRENCY:"",OUT-BAL-FLAG:""

DEFINE OBJVIEW   "Out of Balance Invoices"
       ID         AP-APB-V-0002
       FILENAME   APBATCH
       RELATION   APINVOICE
       CONDITION  OUT-OF-BALANCE
       DSPFLDS    APVENMAST.VENDOR-VNAME:"Vendor",INVOICE:"Invoice",TRAN-INV-AMT:"Invoice Amount",
                  INV-CURRENCY:""

DEFINE OBJVIEW     "Distribution Code"
       ID           AP-HDR-V-0001
       FILENAME     APDISTHDR
       RELATION     APDISTCODE
       DSPFLDS      DIST-COMPANY:"Co", DIS-ACCT-UNIT:"Acct Unit", DIS-ACCOUNT:"Account", DIS-SUB-ACCT:"", ACTIVITY:"Activity",
                    DIST-RATE:"Percent", TRAN-DIST-AMT:"Amount"

DEFINE OBJVIEW     "Withholding Rates"
       ID           AP-SWH-V-0001
       FILENAME     APSECWTH
       RELATION     APSECWTHRATES
       DSPFLDS      EFF-DATE:"Effective Date", WITHHOLD-PCT:"Withholding Rate"

DEFINE OBJVIEW     "Invoice Errors"
       ID           AP-CVI-V-0001
       FILENAME     APCINVOICE 
       RELATION     APCINVERR 
       DSPFLDS      RECORD-TYPE:"",ERR-MESSAGE:"" 

DEFINE OBJWIN      "Line Detail"
       ID           AP-DTC-W-0001
       FILENAME     APDISTCODE
       WINFLDS      DIS-ACCT-UNIT :"         GL Account:", DIS-ACCOUNT:"", DIS-SUB-ACCT:""
                    DIST-COMPANY  :"    Posting Company:"
                    ACTIVITY      :"           Activity:", ACCT-CATEGORY:" "
                    Bill-CATEGORY :"   Billing Category:"
                    
                    TRAN-DIST-AMT :"             Amount:"
                    UNT-AMOUNT    :"        Unit Amount:"
                    DIST-RATE     :"            Percent:"
                    ASSET         :"              Asset:"
                    ASSET-TEMPLATE:"     Asset Template:"

                    DESCRIPTION  :"         Description:"
                    DST-REFERENCE:"           Reference:"

*** APAUTHOR ***

DEFINE OBJWIN     "Authority Name"
       ID          AP-ATR-W-0001
       FILENAME    APAUTHOR
       WINFLDS     AUTH-CODE  :"      Authority Code:"
                   DESCRIPTION:"         Description:"

DEFINE OBJVIEW    "Unapproved Invoices"
       ID          AP-ATR-V-0001
       FILENAME    APAUTHOR
       RELATION    APINVOICE
       DSPFLDS     APVENMAST.VENDOR-VNAME:"Vendor",INVOICE:"Invoice",TRAN-INV-AMT:"Invoice Amount",
                   INV-CURRENCY:""

DEFINE OBJVIEW   "Out of Balance Invoices"
       ID         AP-ATR-V-0002
       FILENAME   APAUTHOR
       RELATION   APINVOICE
       CONDITION  OUT-OF-BALANCE
       DSPFLDS    APVENMAST.VENDOR-VNAME:"Vendor",INVOICE:"Invoice",TRAN-INV-AMT:"Invoice Amount",
                  INV-CURRENCY:""

*** APINVOICE ***

DEFINE OBJWIN      "Invoice Detail"
       ID           AP-API-W-0002
       FILENAME     APINVOICE
       WINFLDS      APVENMAST.VENDOR-VNAME    :"                 Vendor:"
                    APVENLOCREMIT.VENDOR-VNAME:"      Remit To Location:"
                    INVOICE                   :"                Invoice:", SUFFIX:"", INVOICE-TYPE:""
                    TRAN-INV-AMT              :"         Invoice Amount:",INV-CURRENCY:""
                    TRAN-DISC-AMT             :"        Discount Amount:"
                    NET-PAYABLE               :"            Net Payable:"
                    TRAN-TXBL-AMT             :"         Taxable Amount:" 
                    TRAN-TAX-AMT              :"             Tax Amount:",TAX-CODE:"Tax Code"
                    TRAN-PAID-AMT             :"            Paid Amount:"
                    TAX-CODE-CNTL             :"             Tax Adjust:"                   
                    DESCRIPTION               :"    Invoice Description:"
                    INV-STATUS                :"                 Status:",CANCEL-DATE:""
                    APPROVED-FLAG             :"               Approved:",APPRV-OPERATOR:""
                    APPAYMENT.TRANS-NBR       :"         Payment Number:",APPAYMENT.BANK-INST-CODE:"", APPAYMENT.BANK-STATUS:""
                    INVOICE-DTE               :"           Invoice Date:",APPAYMENT.CHECK-DATE      :"      Payment Date:"
                    DUE-DATE                  :"               Due Date:",TERMS-CD:""
                    VOUCHER-NBR               :"                Voucher:"
                    PO-NUMBER                 :"         Purchase Order:",PO-RELEASE:"",PO-CODE:""
                    REFERENCE-NO              :"   Return Reference Nbr:"
                    REF-TYPE                  :"         Reference Type:",CUSTOMER-ID:" Reference Nbr:"

DEFINE OBJWIN      "Archived Invoice Detail"
       ID           AP-AH1-W-0001
       FILENAME     APAPIHIST
       WINFLDS      APVENMAST.VENDOR-VNAME    :"                 Vendor:"
                    APVENLOCREMIT.VENDOR-VNAME:"      Remit To Location:"
                    INVOICE                   :"                Invoice:", SUFFIX:"", INVOICE-TYPE:""
                    TRAN-INV-AMT              :"         Invoice Amount:",INV-CURRENCY:""
                    TRAN-DISC-AMT             :"        Discount Amount:"
                    NET-PAYABLE               :"            Net Payable:"
                    TRAN-TXBL-AMT             :"         Taxable Amount:"
                    TRAN-TAX-AMT              :"             Tax Amount:"
                    TRAN-PAID-AMT             :"            Paid Amount:"
                    DESCRIPTION               :"    Invoice Description:"
                    INV-STATUS                :"                 Status:",CANCEL-DATE:""
                    APPROVED-FLAG             :"               Approved:"
                    APAPPHIST.TRANS-NBR       :"         Payment Number:",APAPPHIST.BANK-INST-CODE:"", APAPPHIST.BANK-STATUS:""
                    INVOICE-DTE               :"           Invoice Date:",APAPPHIST.CHECK-DATE      :"      Payment Date:"
                    DUE-DATE                  :"               Due Date:",TERMS-CD:""
                    VOUCHER-NBR               :"                Voucher:"
                    PO-NUMBER                 :"         Purchase Order:",PO-RELEASE:"",PO-CODE:""
                    REFERENCE-NO              :"   Return Reference Nbr:"

DEFINE OBJWIN      "Invoice Parameters"
       ID           AP-API-W-0003
       FILENAME     APINVOICE
       WINFLDS      PROC-LEVEL   :"            Process Level:", APPROCLEV.NAME        :""
                    JRNL-BOOK-NBR:"             Journal Book:"
                    CASH-CODE    :"                Cash Code:", CBCASHCODE.DESCRIPTION: ""
                    INCOME-CODE  :"              Income Code:", APINCCODE.DESCRIPTION :" "
                    ACCR-CODE    :"     Invoice Accrual Code:"
                    DISCOUNT-CODE:"            Discount Code:"
                    HANDLING-CODE:"            Handling Code:"
                    MATCH-TABLE  :"              Match Table:",RULE-GROUP:"Rule Group:"
                    REASON-CODE  :"              Reason Code:",REASONCD.DESCRIPTION:" " 
                    DIVERSE-CODE :"          Diversity Codes:" 
                    SEC-WITHHOLD :"    Secondary Withholding:"
                    FOR-ECON-CODE:"            For Econ Code:"
                    LTR-OF-GUARAN:"      Letter of Guarantee:"
                    NBR-RECUR-PMT:"    Number of Recurrences:"
                    RECUR-FREQ   :"     Recurrence Frequency:"

                    PAY-VENDOR   :"               Pay Vendor:", APPAYVENMAST.VENDOR-VNAME :"   "
                    REMIT-TO-CODE:"        Remit To Location:", APVENLOCREMIT.VENDOR-VNAME:""
                    PURCH-FR-LOC :"   Purchase From Location:", APVENLOCPURCH.VENDOR-VNAME:""

DEFINE OBJWIN      "Archived Invoice Parameters"
       ID           AP-AH1-W-0002
       FILENAME     APAPIHIST
       WINFLDS      PROC-LEVEL   :"            Process Level:", APPROCLEV.NAME        :""
                    JRNL-BOOK-NBR:"             Journal Book:"

                    CASH-CODE    :"                Cash Code:" 
                    INCOME-CODE  :"              Income Code:", APINCCODE.DESCRIPTION :" "
                    ACCR-CODE    :"     Invoice Accrual Code:"
                    DISCOUNT-CODE:"            Discount Code:"
                    HANDLING-CODE:"            Handling Code:"
                    MATCH-TABLE  :"              Match Table:", RULE-GROUP:"Rule Group:"
                    REASON-CODE  :"              Reason Code:", REASONCD.DESCRIPTION:" " 

                    NBR-RECUR-PMT:"    Number of Recurrences:"
                    RECUR-FREQ   :"     Recurrence Frequency:"

                    PAY-VENDOR   :"               Pay Vendor:", APPAYVENMAST.VENDOR-VNAME :"   "
                    REMIT-TO-CODE:"        Remit To Location:", APVENLOCREMIT.VENDOR-VNAME:""
                    PURCH-FR-LOC :"   Purchase From Location:", APVENLOCPURCH.VENDOR-VNAME:""

DEFINE OBJWIN      "Remit To Payment Location"
       ID           AP-API-W-0004
       FILENAME     APINVOICE
       VALIDITY     APVENLOCREMIT
       WINFLDS      APVENLOCREMIT.LOCATION-CODE :" Location Code:"
                    APVENLOCREMIT.VENDOR-VNAME  :"          Name:"
                    APVENLOCADDR.ADDR1          :"       Address:"
                    APVENLOCADDR.ADDR2          :"               "
                    APVENLOCADDR.ADDR3          :"               "
                    APVENLOCADDR.ADDR4          :"               "
                    APVENLOCADDR.CITY-ADDR5     :"               ",APVENLOCADDR.STATE-PROV:" ", APVENLOCADDR.POSTAL-CODE:" "
                    APVENLOCADDR.COUNTRY        :"               "
                    APVENLOCREMIT.VENDOR-CONTCT :"       Contact:"
                    APVENLOCREMIT.PHONE-PREFIX  :"     Telephone:",APVENLOCREMIT.PHONE-NUM :" ",APVENLOCREMIT.PHONE-EXT :" "
                    APVENLOCREMIT.MBL-INT-PREFIX:"        Mobile:",APVENLOCREMIT.MOBILE-NUM:" ",APVENLOCREMIT.MOBILE-EXT:" "
                    APVENLOCREMIT.FAX-PREFIX    :"           Fax:",APVENLOCREMIT.FAX-NUM   :" ",APVENLOCREMIT.FAX-EXT   :" "

DEFINE OBJWIN      "Alternate Payment Vendor"
       ID           AP-API-W-0005
       FILENAME     APINVOICE
       VALIDITY     FACTOR-VENDOR
       WINFLDS      APPAYVENMAST.VENDOR-VNAME  :"          Name:"

                    APPAYVENMAST.VENDOR-CONTCT :"       Contact:"
                    APPAYVENMAST.PHONE-PREFIX  :"     Telephone:",APPAYVENMAST.PHONE-NUM:" ",APPAYVENMAST.PHONE-EXT:" "
                    APPAYVENMAST.FAX-PREFIX    :"           Fax:",APPAYVENMAST.FAX-NUM:"   ",APPAYVENMAST.FAX-EXT:"   "

DEFINE OBJWIN      "Archived Alternate Payment Vendor"
       ID           AP-AH1-W-0003
       FILENAME     APAPIHIST
       VALIDITY     FACTOR-VENDOR
       WINFLDS      APPAYVENMAST.VENDOR-VNAME  :"          Name:"

                    APPAYVENMAST.VENDOR-CONTCT :"       Contact:"
                    APPAYVENMAST.PHONE-PREFIX  :"     Telephone:",APPAYVENMAST.PHONE-NUM:" ",APPAYVENMAST.PHONE-EXT:" "
                    APPAYVENMAST.FAX-PREFIX    :"           Fax:",APPAYVENMAST.FAX-NUM:"   ",APPAYVENMAST.FAX-EXT:"   "

DEFINE OBJWIN      "Out of Balance"
       ID           AP-API-W-0006
       FILENAME     APINVOICE
       VALIDITY     OUT-OF-BALANCE
       WINFLDS      TRAN-INV-AMT :"                        Invoice Amount:"
                    TRAN-TAX-AMT :"                            Tax Amount:"

                    TRAN-TOT-DIST:"           Distribution Amount Applied:"
                    DIST-OUT-BAL :"    Distribution Out of Balance Amount:"

                    TRAN-TOT-PMT :"                Payment Amount Applied:"
                    PMT-OUT-BAL  :"         Payment Out of Balance Amount:"

                    TRAN-TOT-TAX :"                    Tax Amount Applied:"
                    TAX-OUT-BAL  :"             Tax Out of Balance Amount:"

DEFINE OBJWIN      "Out of Balance"
       ID           AP-API-W-0007
       FILENAME     APINVOICE
       VALIDITY     OUT-BAL-PO-INV
       WINFLDS      TRAN-INV-AMT :"                        Invoice Amount:"
                    TRAN-TAX-AMT :"                            Tax Amount:"

                    TRAN-TOT-DIST:"           Distribution Amount Applied:"
                    DIST-OUT-BAL :"    Distribution Out of Balance Amount:"

                    TRAN-TOT-PMT :"                Payment Amount Applied:"
                    PMT-OUT-BAL  :"         Payment Out of Balance Amount:"

                    TRAN-TOT-TAX :"                    Tax Amount Applied:"
                    TAX-OUT-BAL  :"             Tax Out of Balance Amount:"

DEFINE OBJWIN      "Taxable Out of Balance"
       ID           AP-API-W-0008
       FILENAME     APINVOICE
       VALIDITY     OUT-BAL-TXBL
       WINFLDS      TRAN-INV-AMT :"                        Invoice Amount:"
                    TRAN-TXBL-AMT:"                        Taxable Amount:"
                    TRAN-TAX-AMT :"                            Tax Amount:"

                    TRAN-TOT-TAX :"                    Tax Amount Applied:"
                    TAX-OUT-BAL  :"             Tax Out of Balance Amount:"

                    TRAN-TOT-TXBL:"                Taxable Amount Applied:"
                    TXB-OUT-BAL  :"         Taxable Out of Balance Amount:"

DEFINE OBJWIN      "User Analysis"
       ID           AP-API-W-0009
       FILENAME     APINVOICE
       VALIDITY     APUAVAL-EXISTS
       WINFLDS      APUAVAL.SEGMENT-BLOCK=63:"  Usr Anlys:"
       
DEFINE OBJWIN      "Base Currency Amounts"
       ID           AP-API-W-00010
       FILENAME     APINVOICE
       VALIDITY     NON-BASE-INV
       WINFLDS      ORIG-CNV-RATE:"     Original Exchange Rate:"
                    BASE-INV-AMT :"    Original Invoice Amount:"
                    BASE-ACT-AMT :"    Revalued Invoice Amount:"
                    BASE-DISC-AMT:"            Discount Amount:"

                    CURR-RECALC  :"             Revalue Option:"

DEFINE OBJWIN      "Archived Base Currency Amounts"
       ID           AP-AH1-W-0004
       FILENAME     APAPIHIST 
       VALIDITY     NON-BASE-INV
       WINFLDS      ORIG-CNV-RATE:"     Original Exchange Rate:"
                    BASE-INV-AMT :"    Original Invoice Amount:"
                    BASE-ACT-AMT :"    Revalued Invoice Amount:"
                    BASE-DISC-AMT:"            Discount Amount:"

                    CURR-RECALC  :"             Revalue Option:"

DEFINE OBJWIN      "Invoice Origin"
       ID           AP-API-W-00011
       FILENAME     APINVOICE
       WINFLDS      BATCH-NUM    :"                  Batch:",BATCH-DATE:""
                    AUTH-CODE    :"         Authority Code:"

                    OPERATOR:    :"               Operator:"
                    CREATE-DATE  :"          Creation Date:"
                    CREATION-TIME:"          Creation Time:"

                    VOUCHER-NBR  :"                Voucher:"
                   INVOICE-SOURCE:"         Invoice Source:"

DEFINE OBJWIN      "Archived Invoice Origin"
       ID           AP-AH1-W-0005
       FILENAME     APAPIHIST
       WINFLDS      BATCH-NUM    :"                  Batch:",BATCH-DATE:""
                    AUTH-CODE    :"         Authority Code:"

                    OPERATOR:    :"               Operator:"
                    CREATE-DATE  :"          Creation Date:"
                    CREATION-TIME:"          Creation Time:"

                    VOUCHER-NBR  :"                Voucher:"
                   INVOICE-SOURCE:"         Invoice Source:"

DEFINE OBJWIN       "Invoice User Fields"
         ID           AP-API-W-0012
         FILENAME     APINVOICE
         WINFLDS      INVUSRFLD.INV-USR-FLD-01:"Inv User Field 1:"
                      INVUSRFLD.INV-USR-FLD-02:"Inv User Field 2:"
                      INVUSRFLD.INV-USR-FLD-03:"Inv User Field 3:"
                      INVUSRFLD.INV-USR-FLD-04:"Inv User Field 4:"
                      INVUSRFLD.INV-USR-FLD-05:"Inv User Field 5:"

DEFINE OBJWIN       "Archived Invoice User Fields"
         ID           AP-AH1-W-0006
         FILENAME     APAPIHIST
         WINFLDS      INVUSRFLD.INV-USR-FLD-01:"Inv User Field 1:"
                      INVUSRFLD.INV-USR-FLD-02:"Inv User Field 2:"
                      INVUSRFLD.INV-USR-FLD-03:"Inv User Field 3:"
                      INVUSRFLD.INV-USR-FLD-04:"Inv User Field 4:"
                      INVUSRFLD.INV-USR-FLD-05:"Inv User Field 5:"

DEFINE OBJWIN      "Match Information"
       ID           AP-API-W-0013
       FILENAME     APINVOICE
       VALIDITY     LAWSON-PO     
       WINFLDS      PO-NUMBER                 :"         Purchase Order:",PO-RELEASE:"",PO-CODE:""
                    LOCATION                  :"       Ship To Location:"

                    MATCH-STATUS              :"           Match Status:"

                    MATCH-AMT                 :"           Goods Amount:"
                    RETAIL-AMT                :"          Retail Amount:"

                    RCPT-INV-DATE             :"    Receipt of Inv Date:"
                    MATCH-DATE                :"             Match Date:"

                    MATCH-REF-NBR             :"        Match Reference:"
                    MATCH-TABLE               :"            Match Table:",RULE-GROUP:"Rule Group:"
                    MTCH-PROC-TYPE            :"     Match Invoice Type:"
                    POD-PRINTED               :"   Proof of Del Printed:"

DEFINE OBJWIN      "Archived Match Information"
       ID           AP-AH1-W-0007
       FILENAME     APAPIHIST
       VALIDITY     LAWSON-PO     
       WINFLDS      PO-NUMBER                 :"         Purchase Order:",PO-RELEASE:"",PO-CODE:""
                    LOCATION                  :"       Ship To Location:"

                    MATCH-STATUS              :"           Match Status:"

                    MATCH-AMT                 :"           Goods Amount:"
                    RETAIL-AMT                :"          Retail Amount:"

                    RCPT-INV-DATE             :"    Receipt of Inv Date:"
                    MATCH-DATE                :"             Match Date:"

                    MATCH-REF-NBR             :"        Match Reference:"
                    MATCH-TABLE               :"            Match Table:",RULE-GROUP:"Rule Group:"
                    POD-PRINTED               :"   Proof of Del Printed:"

DEFINE OBJWIN      "AP Detail Invoice Totals"
       ID           AP-API-W-0014
       FILENAME     APINVOICE
       VALIDITY     BYPASS-INV
       WINFLDS      TRAN-INV-AMT              :"         Invoice Amount:"

                    TRAN-TOT-DIST             :" Tot Misc Distributions:"
                    TRAN-TAX-AMT              :"             Tax Amount:"

                    MATCH-AMT                 :"           Goods Amount:"
                    AOC-ALLOW-AMT             :"     Add On Cost Amount:" 

DEFINE OBJWIN      "Match Invoice Totals"
       ID           AP-API-W-0015
       FILENAME     APINVOICE
       VALIDITY     LAWSON-PO
       WINFLDS      TRAN-INV-AMT              :"         Invoice Amount:"

                    TRAN-TOT-DIST             :"    Total Distributions:"
                    TRAN-TAX-AMT              :"             Tax Amount:"

                    MATCH-AMT                 :"           Goods Amount:"
                    AOC-ALLOW-AMT             :"     Add On Cost Amount:"
                    SERVICE-AMT               :"         Service Amount:"

                    PO-INV-TAX                :"              Tax on PO:"

DEFINE OBJWIN      "Archived Match Invoice Totals"
       ID           AP-AH1-W-0008
       FILENAME     APAPIHIST
       VALIDITY     LAWSON-PO
       WINFLDS      TRAN-INV-AMT              :"         Invoice Amount:"

                    TRAN-TOT-DIST             :"    Total Distributions:"
                    TRAN-TAX-AMT              :"             Tax Amount:"

                    MATCH-AMT                 :"           Goods Amount:"
                    AOC-ALLOW-AMT             :"     Add On Cost Amount:"
                    SERVICE-AMT               :"         Service Amount:"

                    PO-INV-TAX                :"              Tax on PO:"

DEFINE OBJVIEW     "Vendor"
       ID           AP-API-V-0001
       FILENAME     APINVOICE
       RELATION     APVENMAST
       DSPFLDS      VENDOR-VNAME:"Name", ADDRESS1.CITY-ADDR5:"City", 
                    ADDRESS1.POSTAL-CODE:"Postal Code"

DEFINE OBJVIEW     "Payment Schedule"
        ID          AP-API-V-0002
        FILENAME    APINVOICE
        RELATION    APPAYMENT
        DSPFLDS     DUE-DATE:"Due Date",DISC-DATE:"Disc Date",TRAN-NET-PMT:"Payment Amount",
                    APHLDINV.HLD-CODE:"Hold",PAY-GROUP:"Pay Group",TRANS-NBR:"Payment Nbr",BANK-STATUS:"Status"

DEFINE OBJVIEW     "Archived Payment Schedule"
        ID          AP-AH1-V-0001
        FILENAME    APAPIHIST
        RELATION    APAPPHIST
        DSPFLDS     DUE-DATE:"Due Date",DISC-DATE:"Disc Date",TRAN-NET-PMT:"Payment Amount",
                    APHLDINV.HLD-CODE:"Hold",PAY-GROUP:"Pay Group",TRANS-NBR:"Payment Nbr",BANK-STATUS:"Status"

DEFINE OBJVIEW     "Expense Distributions"
       ID           AP-API-V-0003
       FILENAME     APINVOICE
       RELATION     APDISTRIB
       DSPLFLDS     DISTRIB-DATE:"Post Date",DIST-COMPANY:"Co", DIS-ACCT-UNIT:"Account", DIS-ACCOUNT, DIS-SUB-ACCT,
                    ORIG-TRAN-AMT:"Amount", TAX-CODE:"Tax Code", TAX-INDICATOR

DEFINE OBJVIEW    "Linked Tax Distributions"
       ID           AP-API-V-0004
       FILENAME     APDISTRIB
       VALIDITY     GOODS-DIST
       RELATION     APDISTRIB
       DSPLFLDS     DISTRIB-DATE:"Post Date",DIST-COMPANY:"Co", DIS-ACCT-UNIT:"Account", DIS-ACCOUNT, DIS-SUB-ACCT,
                    ORIG-TRAN-AMT:"Amount",TAX-CODE:"Tax Code"

DEFINE OBJVIEW     "Archived Expense Distributions"
       ID           AP-AH1-V-0002
       FILENAME     APAPIHIST
       RELATION     APAPDHIST
       DSPLFLDS     DISTRIB-DATE:"Post Date",DIST-COMPANY:"Co", DIS-ACCT-UNIT:"Account", DIS-ACCOUNT, DIS-SUB-ACCT,
                    ORIG-TRAN-AMT:"Amount",TAX-CODE:"Tax Code"

DEFINE OBJVIEW     "Match Purchase Order Detail"
       ID           AP-API-V-0005
       FILENAME     APINVOICE
       VALIDITY     PO-NUMBER-USED         
       RELATION     MAPOINV  
       DSPFLDS      PO-NUMBER:"------PO------", PO-RELEASE:"----",PO-CODE:"Code",
                    PURCHORDER.PO-DATE:"  Date",PURCHORDER.BUYER-CODE:"Buyer"

DEFINE OBJVIEW     "Purchase Order Detail"
       ID           AP-API-V-0020
       FILENAME     APINVOICE
       VALIDITY     PO-EXISTS         
       RELATION     PURCHORDER  
       DSPFLDS      COMPANY:"Company",PO-NUMBER:"------PO------",
                    PO-RELEASE:"----",PO-CODE:"Code"

DEFINE OBJVIEW     "Archived Purchase Orders"
       ID           AP-AH1-V-0003
       FILENAME     APAPIHIST
       VALIDITY     PO-NUMBER-USED
       RELATION     MAAOIHIST
       DSPFLDS      PO-NUMBER:"------PO------", PO-RELEASE:"----",PO-CODE:"Code",
                    PURCHORDER.PO-DATE:"  Date",PURCHORDER.BUYER-CODE:"Buyer"

DEFINE OBJVIEW     "Unreleased Buyer Messages"
       ID           AP-API-V-0006
       FILENAME     APINVOICE
       VALIDITY     BUYER-MESSAGES
       RELATION     POMESSAGE
       CONDITION    OPEN-MESSAGE
       DSPFLDS      APVENMAST.VENDOR:"Vendor",INVOICE:"Invoice",SUFFIX:"",
                    PO-NUMBER:"------PO------",PO-RELEASE:"----",PO-CODE:"Code"

DEFINE OBJVIEW     "Employee Expense Transaction"
       ID           AP-API-V-0007
       FILENAME     APINVOICE
       VALIDITY     EMP-EXPENSE
       RELATION     EEEXPENSE
       DSPFLDS      EXPENSE-NBR:"Expense Nbr", EXPENSE-DATE:"Date", BASE-EXP-AMT:"Total Expense Amt", BASE-EMP-PAID:"Employee Paid Amt"

DEFINE OBJVIEW     "Archived Employee Expense Transaction"
       ID           AP-AH1-V-0004
       FILENAME     APAPIHIST
       VALIDITY     EMP-EXPENSE
       RELATION     EEEXPENSE
       DSPFLDS      EXPENSE-NBR:"Expense Nbr", EXPENSE-DATE:"Date", BASE-EXP-AMT:"Total Expense Amt", BASE-EXP-AMT:"Employee Paid Amt"

DEFINE OBJVIEW     "Employee Advance Transaction"
       ID           AP-API-V-0008
       FILENAME     APINVOICE
       VALIDITY     EMP-ADVANCE
       RELATION     EEADVANCE-A
       DSPFLDS      ADVANCE-NBR:"Expense Nbr", ADVANCE-DATE:"Date", BASE-ADV-AMT:"Advance Amt"

DEFINE OBJVIEW     "Archived Employee Advance Transaction"
       ID           AP-AH1-V-0005
       FILENAME     APAPIHIST
       VALIDITY     EMP-ADVANCE
       RELATION     EEADVANCE-A
       DSPFLDS      ADVANCE-NBR:"Expense Nbr", ADVANCE-DATE:"Date", BASE-ADV-AMT:"Advance Amt"

DEFINE OBJVIEW     "Employee Advance Transaction"
       ID           AP-API-V-0009
       FILENAME     APINVOICE
       VALIDITY     EMP-CREDIT
       RELATION     EEADVANCE-M
       DSPFLDS      ADVANCE-NBR:"Expense Nbr", ADVANCE-DATE:"Date", BASE-ADV-AMT:"Advance Amt"

DEFINE OBJVIEW     "Archived Employee Advance Transaction"
       ID           AP-AH1-V-0006
       FILENAME     APAPIHIST
       VALIDITY     EMP-CREDIT
       RELATION     EEADVANCE-M
       DSPFLDS      ADVANCE-NBR:"Expense Nbr", ADVANCE-DATE:"Date", BASE-ADV-AMT:"Advance Amt"

DEFINE OBJVIEW     "Applied Invoice(s)"
       ID           AP-API-V-0010
       FILENAME     APINVOICE
       VALIDITY     INV-APPLIED
       RELATION     APCRMEMO2
       DSPFLDS      INVOICE:"Invoice", SUFFIX:"", AMT-APPLIED:"Applied Amount"

DEFINE OBJVIEW     "Applied Credit"
       ID           AP-API-V-0011
       FILENAME     APINVOICE
       VALIDITY     CREDIT-APPLIED
       RELATION     APCRMEMO
       DSPFLDS      CR-MEMO-NBR:"Credit", CR-MEMO-SUFFIX:"", AMT-APPLIED:"Applied Amount"

DEFINE OBJVIEW      "Credit Return Detail"
       ID           AP-API-V-0012
       FILENAME     APINVOICE
       VALIDITY     FROM-RETURN
       RELATION     PORETURNHD
       DSPFLDS      RETURN-NUMBER:"Return Nbr",RETURN-FRM-LOC:"Returned From",RETURN-VALUE:"Amount"

DEFINE OBJVIEW     "AOC Distributions"
       ID           AP-API-V-0013
       FILENAME     APINVOICE
       RELATION     APDISTRIB
	   CONDITION    AOC-DIST
       DSPLFLDS     DISTRIB-DATE:"Post Date",DIST-COMPANY:"Co", DIS-ACCT-UNIT:"Account", DIS-ACCOUNT, DIS-SUB-ACCT,
                    ORIG-TRAN-AMT:"Amount", PO-AOC-CODE:"AOC"

DEFINE OBJVIEW     "Archived AOC Distributions"
       ID           AP-AH1-V-0007
       FILENAME     APAPIHIST
       RELATION     APAPDHIST
	   CONDITION    AOC-DIST
       DSPLFLDS     DISTRIB-DATE:"Post Date",DIST-COMPANY:"Co", DIS-ACCT-UNIT:"Account", DIS-ACCOUNT, DIS-SUB-ACCT,
                    ORIG-TRAN-AMT:"Amount", PO-AOC-CODE:"AOC"

DEFINE OBJVIEW     "Related Invoice Transactions"
       ID           AP-API-V-0014
       FILENAME     APINVOICE
       VALIDITY     MATCHED
       RELATION     MATCH-TRANS
       DSPFLDS      INVOICE:"Invoice", SUFFIX,INVOICE-TYPE:"Type",TRAN-INV-AMT:"Amount",INV-STATUS

DEFINE OBJVIEW     "Archived Related Invoice Transactions"
       ID           AP-AH1-V-0008
       FILENAME     APAPIHIST
       VALIDITY     MATCHED
       RELATION     MATCH-TRANS
       DSPFLDS      INVOICE:"Invoice", SUFFIX,INVOICE-TYPE:"Type",TRAN-INV-AMT:"Amount",INV-STATUS

DEFINE OBJVIEW     "Related Receipts - by Match Reference"
       ID           AP-API-V-0015
       FILENAME     APINVOICE
       VALIDITY     MATCH-REF
       RELATION     POR-MATCH-REF
       DSPFLDS      MATCH-REF-NBR:"Match Reference",REC-NUMBER:"Receipt",MATCH-AMT:"Match Amount

DEFINE OBJVIEW     "Archived Related Receipts - by Match Reference"
       ID           AP-AH1-V-0009
       FILENAME     APAPIHIST
       VALIDITY     MATCH-REF
       RELATION     POR-MATCH-REF
       DSPFLDS      MATCH-REF-NBR:"Match Reference",REC-NUMBER:"Receipt",MATCH-AMT:"Match Amount

DEFINE OBJVIEW     "Open Receipts - by Vendor"
       ID           AP-API-V-0016
       FILENAME     APINVOICE
       VALIDITY     LAWSON-PO
       RELATION     PORSET9-VEN
       DSPFLDS      REC-NUMBER:"Receipt", PO-NUMBER:"PO Number",PO-RELEASE,PO-CODE:"PO Code",MATCH-AMT:"Match Amount"

DEFINE OBJVIEW     "Related Open Receipts - by Vendor, PO Number"
       ID           AP-API-V-0017
       FILENAME     APINVOICE
       VALIDITY     LAWSON-PO
       RELATION     PORSET9
       DSPFLDS      REC-NUMBER:"Receipt", PO-NUMBER:"PO Number",PO-RELEASE,PO-CODE:"PO Code",MATCH-AMT:"Match Amount"	

DEFINE OBJVIEW     "Matched Receipt Lines"
       ID           AP-API-V-0018
       FILENAME     APINVOICE
       VALIDITY     MATCHED
       RELATION     PORECLINE
       CONDITION    MATCHED
       DSPLFDS      REC-NUMBER:"Receipt", LINE-NBR:"Line", MATCH-DTL-KEY=28:"Item Detail", ITEM-TYPE:"Type",MATCHED-QTY:"Matched Qty"

DEFINE OBJVIEW     "Multiple Invoices to Receipt"
       ID           AP-API-V-0019
       FILENAME     APINVOICE
       VALIDITY     MATCHED
       RELATION     POMATCHOBJ
*      CONDITION    MATCHED
       DSPLFDS      REC-NUMBER:"Receipt", LINE-NBR:"Line", MATCH-DTL-KEY=28:"Item Detail", MATCHED-QTY:"Matched Qty"

DEFINE OBJWIN      "Global Information"
       ID           AP-API-W-0016
       FILENAME     APINVOICE
       VALIDITY     INTRAST-EXIST
       WINFLDS      INVOICE                   :"                Invoice:", SUFFIX:"", INVOICE-TYPE:""
                    NOTC                      :"                   NOTC:"
                    STAT-PROC                 :"              Stat Proc:"
                    SHIP-VIA                  :"               Ship Via:"
                    UNLOADING-PORT            :"         Unloading Port:"
                    INTRASTAT-NBR             :"             Global Nbr:"
                    DROPSHIP-FL               :"            Dropship Fl:"
                    GLBL-DOC-TYPE             :"     Document Type Code:"
                    FOB-CODE                  :"             Ship Terms:"
                    LTR-OF-GUARAN             :"    Letter of Guarantee:"

DEFINE OBJVIEW     "Archived Matched Receipt Lines"
       ID           AP-AP1-V-0009
       FILENAME     APAPIHIST
       VALIDITY     MATCHED
       RELATION     PORECLINE
       CONDITION    MATCHED
       DSPLFDS      REC-NUMBER:"Receipt", LINE-NBR:"Line", MATCH-DTL-KEY=28:"Item Detail", ITEM-TYPE:"Type",MATCHED-QTY:"Matched Qty"

DEFINE OBJWIN      "Retainage Detail"
       ID           AP-API-W-0021
       FILENAME     APINVOICE
       VALIDITY     RETAINAGE-INV
       WINFLDS      RET-TRAN-AMT: "     Original Retainage Amount:"
                    RET-BASE-AMT: "Original Retainage Base Amount:"
                    RET-TRAN-PMT: "         Paid Retainage Amount:"
                    RET-BASE-PMT: "    Paid Retainage Base Amount:"
                    RET-ACCR-CODE:"        Retainage Accrual Code:"

DEFINE OBJVIEW     "Retainage Records"
       ID           AP-API-V-0021
       FILENAME     APINVOICE
       VALIDITY     RETAINAGE-INV
       RELATION     APDISTPAY
       DSPLFDS      MAINVDTL.ITEM:"Item", APDISTRIB.DIS-ACCOUNT:"Account",
                    APDISTRIB.DIS-SUB-ACCT:"",PMT-AMOUNT:"Amount",RET-PCT:"Ret Pct"

*** APPAYMENT ***

DEFINE OBJWIN     "Cash Payment Detail"
       ID          AP-APP-W-0001
       FILENAME    APPAYMENT
       VALIDITY    CASH-PAYMENT
       WINFLDS     CASH-CODE            :"             Cash Code:"
                   TRANS-NBR            :"        Payment Number:", BANK-INST-CODE:" "
                   CHECK-DATE           :"          Payment Date:"
                   CBCHECK.BANK-CURR-AMT:"        Payment Amount:", CBCASHCODE.CURRENCY-CODE:""

                   CBCHECK.TRAN-PMT-AMT :"    Transaction Amount:", CBCHECK.PAY-CURRENCY    :""
                   CBCHECK.BASE-PMT-AMT :"           Base Amount:"
                   CBCHECK.CHK-STATUS   :"                Status:"

                   CBCHECK.PAID-VENDOR  :"           Paid Vendor:", CBCHECK.PAID-NAME:" "

DEFINE OBJWIN     "Archived Cash Payment Detail"
       ID          AP-AH3-W-0001
       FILENAME    APAPPHIST
       VALIDITY    CASH-PAYMENT
       WINFLDS     CASH-CODE            :"             Cash Code:"
                   TRANS-NBR            :"        Payment Number:", BANK-INST-CODE:" "
                   CHECK-DATE           :"          Payment Date:"
                   CBCHECK.BANK-CURR-AMT:"        Payment Amount:", CBCASHCODE.CURRENCY-CODE:""

                   CBCHECK.TRAN-PMT-AMT :"    Transaction Amount:", CBCHECK.PAY-CURRENCY    :""
                   CBCHECK.BASE-PMT-AMT :"           Base Amount:"
                   CBCHECK.CHK-STATUS   :"                Status:"

                   CBCHECK.PAID-VENDOR  :"           Paid Vendor:", CBCHECK.PAID-NAME:" "

DEFINE OBJWIN     "BOE Payment Detail"
       ID          AP-APP-W-0002
       FILENAME    APPAYMENT
       VALIDITY    DRAFT-PAYMENT
       WINFLDS     CASH-CODE              :"           Cash Code:"
                   TRANS-NBR              :"      Payment Number:", BANK-INST-CODE:" "

                   APDRAFTS.DRAFT-AMOUNT  :"          BOE Amount:", APDRAFTS.CURRENCY-CODE:""

                   APDRAFTS.DRAFT-DATE    :"            BOE Date:"
                   APDRAFTS.ACCEPT-DATE   :"         Accept Date:"
                   APDRAFTS.CASH-DATE     :"           Cash Date:"

                   APDRAFTS.DERIVED-STATUS:"              Status:", APDRAFTS.DISHONOR-DATE:"", APDRAFTS.CANCEL-DATE:""

                   APDRAFTS.VENDOR        :"         Paid Vendor:"
                   APDRAFTS.REMIT-TO-CODE :"   Remit To Location:"
                   APDRAFTS.VENDOR-VNAME  :"    Paid Vendor Name:"

DEFINE OBJWIN     "Archived BOE Payment Detail"
       ID          AP-AH3-W-0002
       FILENAME    APAPPHIST
       VALIDITY    DRAFT-PAYMENT
       WINFLDS     CASH-CODE              :"           Cash Code:"
                   TRANS-NBR              :"      Payment Number:", BANK-INST-CODE:" "

                   APDRAFTS.DRAFT-AMOUNT  :"          BOE Amount:", APDRAFTS.CURRENCY-CODE:""

                   APDRAFTS.DRAFT-DATE    :"            BOE Date:"
                   APDRAFTS.ACCEPT-DATE   :"         Accept Date:"
                   APDRAFTS.CASH-DATE     :"           Cash Date:"

                   APDRAFTS.DERIVED-STATUS:"              Status:", APDRAFTS.DISHONOR-DATE:"", APDRAFTS.CANCEL-DATE:""

                   APDRAFTS.VENDOR        :"         Paid Vendor:"
                   APDRAFTS.REMIT-TO-CODE :"   Remit To Location:"
                   APDRAFTS.VENDOR-VNAME  :"    Paid Vendor Name:"

DEFINE OBJWIN     "Payment Options"
       ID          AP-APP-W-0003
       FILENAME    APPAYMENT
       WINFLDS     PAY-VENDOR        :"        Payment Vendor:",APPAYVENDOR.VENDOR-VNAME:" "
                   REMIT-TO-CODE     :"         Remit To Code:"

                   DUE-DATE          :"              Due Date:"
                   DISC-DATE         :"         Discount Date:"

                   ENCLOSURE         :"             Enclosure:"
                   SEP-CHK-FLAG      :"      Separate Payment:"
                   PAY-IMM-FLAG      :"       Pay Immediately:"
                   INVOICE-GROUP     :"         Invoice Group:"
                   CASH-CODE         :"             Cash Code:"
                   BANK-INST-CODE    :"          Payment Code:"
                   DISCOUNT-CODE     :"         Discount Code:"
                   APHLDINV.HLD-CODE :"             Hold Code:"
                   INCOME-CODE       :"           Income Code:"

DEFINE OBJWIN     "Archived Payment Options"
       ID          AP-AH3-W-0003
       FILENAME    APAPPHIST
       WINFLDS     PAY-VENDOR        :"        Payment Vendor:",APPAYVENDOR.VENDOR-VNAME:" "
                   REMIT-TO-CODE     :"         Remit To Code:"

                   DUE-DATE          :"              Due Date:"
                   DISC-DATE         :"         Discount Date:"

                   ENCLOSURE         :"             Enclosure:"
                   SEP-CHK-FLAG      :"      Separate Payment:"
                   PAY-IMM-FLAG      :"       Pay Immediately:"
                   INVOICE-GROUP     :"         Invoice Group:"
                   CASH-CODE         :"             Cash Code:"
                   BANK-INST-CODE    :"          Payment Code:"
                   DISCOUNT-CODE     :"         Discount Code:"
                   APHLDINV.HLD-CODE :"             Hold Code:"
                   INCOME-CODE       :"           Income Code:"

DEFINE OBJWIN     "Base Currency"
       ID          AP-APP-W-0004
       FILENAME    APPAYMENT
       VALIDITY    NON-BASE-PMT
       WINFLDS     BASE-PMT-AMT           :"     Original Base Payment Amount:",APCOMPANY.BASE-CURRENCY:""
                   BASE-ACT-AMT           :"     Revalued Base Payment Amount:"
                   BASE-DISC-AMT          :"             Base Discount Amount:"

                   APINVOICE.ORIG-CNV-RATE:"           Original Exchange Rate:"
                   ACT-CNV-RATE           :"           Revalued Exchange Rate:"
                   APINVOICE.CURR-RECALC  :"                   Revalue Option:"

                   CASH-CODE              :"                        Cash Code:"

DEFINE OBJWIN     "Archive Base Currency"
       ID          AP-AH3-W-0004
       FILENAME    APAPPHIST
       VALIDITY    NON-BASE-PMT
       WINFLDS     BASE-PMT-AMT           :"     Original Base Payment Amount:",APCOMPANY.BASE-CURRENCY:""
                   BASE-ACT-AMT           :"     Revalued Base Payment Amount:"
                   BASE-DISC-AMT          :"             Base Discount Amount:"

                   APAPIHIST.ORIG-CNV-RATE:"           Original Exchange Rate:"
                   ACT-CNV-RATE           :"           Revalued Exchange Rate:"
                   APAPIHIST.CURR-RECALC  :"                   Revalue Option:"

                   CASH-CODE              :"                        Cash Code:"

DEFINE OBJWIN      "Income Withholding"
       ID           AP-APP-W-0005
       FILENAME     APPAYMENT
       VALIDITY     INC-WH-PMT
       WINFLDS      TRAN-INC-WH           :"          Withholding Amount:", INV-CURRENCY:""
                    TRAN-NET-PMT          :"          Net Payment Amount:"
                    TRAN-INC-AMT          :"    Reportable Income Amount:"

                    INCOME-CODE           :"                 Income Code:"
                      
                    INC-ACCR-CODE         :"     Income Withholding Code:"

DEFINE OBJWIN      "Secondary Withholding"
       ID           AP-APP-W-0008
       FILENAME     APPAYMENT
       VALIDITY     SEC-WH-PMT
       WINFLDS      TRAN-SEC-WTH1         :"         Withholding Amount1:", INV-CURRENCY:""
                    SEC-WTH-CODE1         :" Secondary Withholding Code1:", SEC-WTH-PCT1:"Percent"

                    TRAN-SEC-WTH2         :"         Withholding Amount2:"
                    SEC-WTH-CODE2         :" Secondary Withholding Code2:", SEC-WTH-PCT2:"Percent"

                    TRAN-SEC-WTH3         :"         Withholding Amount3:"
                    SEC-WTH-CODE3         :" Secondary Withholding Code3:", SEC-WTH-PCT3:"Percent"

                    TRAN-NET-PMT          :"          Net Payment Amount:"

DEFINE OBJWIN      "Archive Income Withholding"
       ID           AP-AH3-W-0005
       FILENAME     APAPPHIST
       VALIDITY     INC-WH-PMT
       WINFLDS      TRAN-INC-WH           :"          Withholding Amount:", INV-CURRENCY:""
                    TRAN-NET-PMT          :"          Net Payment Amount:"
                    TRAN-INC-AMT          :"    Reportable Income Amount:"

                    INCOME-CODE           :"                 Income Code:"
                      
                    INC-ACCR-CODE         :"     Income Withholding Code:"

DEFINE OBJWIN      "Archive Income Withholding"
       ID           AP-AH3-W-0010
       FILENAME     APAPPHIST
       VALIDITY     SEC-WH-PMT
       WINFLDS      TRAN-SEC-WTH1         :"         Withholding Amount1:", INV-CURRENCY:""
                    SEC-WTH-CODE1         :" Secondary Withholding Code1:", SEC-WTH-PCT1:"Percent"

                    TRAN-SEC-WTH2         :"         Withholding Amount2:"
                    SEC-WTH-CODE2         :" Secondary Withholding Code2:", SEC-WTH-PCT2:"Percent"

                    TRAN-SEC-WTH3         :"         Withholding Amount3:"
                    SEC-WTH-CODE3         :" Secondary Withholding Code3:", SEC-WTH-PCT3:"Percent"

                    TRAN-NET-PMT          :"          Net Payment Amount:"

DEFINE OBJVIEW     "Bank Reconciliation Statement Detail"
       ID           AP-APP-W-0006
       FILENAME     APPAYMENT
*      VALIDITY     CSDSET2
       RELATION     CBSTMTDTL
       DSPFLDS      RECON-STMT-NBR:"Recon Stmt Nbr",
                    PAYMENT-NBR:   "Payment Nbr",
                    CASH-CODE:     "Cash Code",
                    LINE-NBR:      "Line Nbr",
                    TRANS-DATE:    "Transaction Date",
                    PROC-LEVEL:    "Proc Level",
                    VENDOR:        "Vendor",
                    ORIG-AMT:      "Original Amt"
                    
                     
DEFINE OBJVIEW     "Bank Reconciliation Statement Header"
       ID           AP-APP-W-0007
       FILENAME     APPAYMENT
       RELATION     CBSTATEMENT
       DSPFLDS      RECON-STMT-NBR:"Recon Stmt Nbr",
                    CASH-CODE:     "Cash Code",
                    STMT-STATUS:   "Status",
                    RECON-STMT-DT: "Stmt Date",
                    END-BAL:       "Ending Bal",
                    BATCH-NBR:     "Batch",
                    NBR-LINES:     "LastLine"

DEFINE OBJVIEW     "Invoice Transaction"
       ID           AP-APP-V-0001
       FILENAME     APPAYMENT
       RELATION     APINVOICE
       DSPFLDS      INVOICE:"Invoice",SUFFIX:"",VOUCHER-NBR:"Voucher",INVOICE-DTE:"Date",TRAN-INV-AMT:"Amount",INV-CURRENCY:""

DEFINE OBJVIEW     "Archived Invoice Transaction"
       ID           AP-AH3-V-0001
       FILENAME     APAPPHIST
       RELATION     APAPIHIST
       DSPFLDS      INVOICE:"Invoice",SUFFIX:"",VOUCHER-NBR:"Voucher",INVOICE-DTE:"Date",TRAN-INV-AMT:"Amount",INV-CURRENCY:""

DEFINE OBJVIEW     "Related Records for Payment"
       ID           AP-APP-V-0002
       FILENAME     APPAYMENT
       VALIDITY     APPSET6
       RELATION     APPSET6
       DSPFLDS      VENDOR:"Vendor",INVOICE:"Invoice",SUFFIX,TRAN-PMT-AMT,TRAN-DISC-AMT

DEFINE OBJVIEW     "Archived Related Records for Payment"
       ID           AP-AH3-V-0002
       FILENAME     APAPPHIST
       VALIDITY     AH3SET6
       RELATION     AH3SET6
       DSPFLDS      VENDOR:"Vendor",INVOICE:"Invoice",SUFFIX,TRAN-PMT-AMT,TRAN-DISC-AMT

DEFINE OBJVIEW     "Retainage Records"
       ID           AP-APP-V-0003
       FILENAME     APPAYMENT
       VALIDITY     RETAINAGE-PMT
       RELATION     APDISTPAY
       DSPFLDS      MAINVDTL.ITEM:"Item",APDISTRIB.DIS-ACCOUNT:"Account",
                    APDISTRIB.DIS-SUB-ACCT:"",PMT-AMOUNT:"Amount",RET-PCT:"Ret Pct"

*** APDISTRIB ***

DEFINE OBJVIEW    "Activity Transaction Detail"
       ID          AP-APD-V-0001
       FILENAME    APDISTRIB
       VALIDITY    COMMITTED-COST
       RELATION    ACTRANS
       DSPFLDS     POSTING-DATE:"Post Date",ACTIVITY:"Activity",ACCT-CATEGORY:"Acct Cat",
                   SYSTEM:"Sys",ACT-AMOUNT:"Amount",UNITS-AMOUNT:"Units"

DEFINE OBJVIEW    "Archived Activity Transaction Detail"
       ID          AP-AH2-V-0001
       FILENAME    APAPDHIST
       VALIDITY    COMMITTED-COST
       RELATION    ACTRANS
       DSPFLDS     POSTING-DATE:"Post Date",ACTIVITY:"Activity",ACCT-CATEGORY:"Acct Cat",
                   SYSTEM:"Sys",ACT-AMOUNT:"Amount",UNITS-AMOUNT:"Units"

DEFINE OBJWIN     "Distribution Detail"
       ID          AP-APD-W-0001
       FILENAME    APDISTRIB
       WINFLDS     COMPANY                 :"       Originating Company:"
                   DIST-COMPANY            :"           Post To Company:"
                   GLMASTER.ACCT-DESC=40   :"                   Account:"

                   ORIG-TRAN-AMT           :"       Distribution Amount:",INV-CURRENCY               :"" 
                   ORIG-BASE-AMT           :"Originating Company Amount:",GLSYSTEM-ORIG.CURRENCY-CODE:""
                   TO-BASE-AMT             :"         To Company Amount:",GLSYSTEM-TO.CURRENCY-CODE  :""

                   UNT-AMOUNT             :"                Unit Amount:"
                   DISTRIB-DATE           :"               Posting Date:"
                   DIST-STATUS            :"             Posting Status:"
                   ACCR-CODE              :"               Accrual Code:"
                   DST-REFERENCE          :"                  Reference:"
                   DESCRIPTION            :"                Description:"
                   ACTIVITY               :"                   Activity:",ACCT-CATEGORY:""
                   BILL-CATEGORY          :"           Billing Category:"

DEFINE OBJWIN     "Archived Distribution Detail"
       ID          AP-AH2-W-0001
       FILENAME    APAPDHIST
       WINFLDS     COMPANY                 :"       Originating Company:"
                   DIST-COMPANY            :"           Post To Company:"
                   GLMASTER.ACCT-DESC=40   :"                   Account:"

                   ORIG-TRAN-AMT           :"       Distribution Amount:",INV-CURRENCY               :"" 
                   ORIG-BASE-AMT           :"Originating Company Amount:",GLSYSTEM-ORIG.CURRENCY-CODE:""
                   TO-BASE-AMT             :"         To Company Amount:",GLSYSTEM-TO.CURRENCY-CODE  :""

                   UNT-AMOUNT             :"                Unit Amount:"
                   DISTRIB-DATE           :"               Posting Date:"
                   DIST-STATUS            :"             Posting Status:"
                   ACCR-CODE              :"               Accrual Code:"
                   DST-REFERENCE          :"                  Reference:"
                   DESCRIPTION            :"                Description:"
                   ACTIVITY               :"                   Activity:",ACCT-CATEGORY:""
                   BILL-CATEGORY          :"           Billing Category:"

DEFINE OBJWIN     "User Analysis"
       ID          AP-APD-W-0002
       FILENAME    APDISTRIB
       VALIDITY    APUAVAL-EXISTS
       WINFLDS     APUAVAL.SEGMENT-BLOCK=63:"Usr Anlys:"

DEFINE OBJWIN     "Archived User Analysis"
       ID          AP-AH2-W-0002
       FILENAME    APAPDHIST
       VALIDITY    APUAVHIST-EXIS
       WINFLDS     APUAVHIST.SEGMENT-BLOCK=63:"Usr Anlys:"

DEFINE OBJWIN     "Tax Distribution"
       ID          AP-APD-W-0003
       FILENAME    APDISTRIB
       VALIDITY    TAX-DIST
       WINFLDS     TAX-CODE      :"          Tax Code:"
                   TAX-USAGE-CD  :"    Tax Usage Code:"

                   TAXABLE-AMT   :"    Taxable Amount:"
                   ORIG-TRAN-AMT :"        Tax Amount:"

                   TAX-RATE      :"          Tax Rate:"

DEFINE OBJWIN     "Archived Tax Distribution"
       ID          AP-AH2-W-0003
       FILENAME    APAPDHIST
       VALIDITY    TAX-DIST
       WINFLDS     TAX-CODE      :"          Tax Code:"

                   TAXABLE-AMT   :"    Taxable Amount:"
                   ORIG-TRAN-AMT :"        Tax Amount:"

                   TAX-RATE      :"          Tax Rate:"

DEFINE OBJWIN     "Asset Code"
       ID          AP-APD-W-0004
       FILENAME    APDISTRIB
       VALIDITY    ASSET-DIST
       WINFLDS     ASSET.ASSET         :"       Asset Code:"
                   ASSET.ASSET-TEMPLATE:"   Asset Template:"
                   ASSET.ASSET-DESC    :"      Description:"

DEFINE OBJWIN     "Archived Asset Code"
       ID          AP-AH2-W-0004
       FILENAME    APAPDHIST
       VALIDITY    ASSET-DIST
       WINFLDS     ASSET.ASSET         :"       Asset Code:"
                   ASSET.ASSET-TEMPLATE:"   Asset Template:"
                   ASSET.ASSET-DESC    :"      Description:"

 DEFINE OBJWIN     "Activity"
        ID          AP-APD-W-0005
        FILENAME    APDISTRIB
        VALIDITY    SUMMARY-DIST
        WINFLDS     ACTIVITY     :"         Activity:"
                    ACCT-CATEGORY:" Account Category:"
                    BILL-CATEGORY:" Billing Category:"

 DEFINE OBJWIN     "Archived Activity"
        ID          AP-AH2-W-0005
        FILENAME    APAPDHIST
        VALIDITY    SUMMARY-DIST
        WINFLDS     ACTIVITY     :"         Activity:"
                    ACCT-CATEGORY:" Account Category:"
                    BILL-CATEGORY:" Billing Category:"

DEFINE OBJWIN     "AOC Detail"
       ID          AP-APD-W-0006
       FILENAME    APDISTRIB
       VALIDITY    AOC-DIST
       WINFLDS     PO-AOC-CODE   :"          AOC Code:"

                   ORIG-TRAN-AMT :"        AOC Amount:"

DEFINE OBJWIN     "Archived AOC Detail"
       ID          AP-AH2-W-0006
       FILENAME    APAPDHIST
       VALIDITY    AOC-DIST
       WINFLDS     PO-AOC-CODE   :"          AOC Code:"

                   ORIG-TRAN-AMT :"        AOC Amount:"

DEFINE OBJWIN      "Distribution User Fields"
       ID           AP-APD-W-0007
       FILENAME     APDISTRIB
       WINFLDS      DSTUSRFLD.DST-USR-FLD-01:"Dist User Field 1:"
                    DSTUSRFLD.DST-USR-FLD-02:"Dist User Field 2:"	
                    DSTUSRFLD.DST-USR-FLD-03:"Dist User Field 3:"	
                    DSTUSRFLD.DST-USR-FLD-04:"Dist User Field 4:"	
                    DSTUSRFLD.DST-USR-FLD-05:"Dist User Field 5:"	

DEFINE OBJWIN      "Archived Distribution User Fields"
       ID           AP-AH2-W-0007
       FILENAME     APAPDHIST
       WINFLDS      DSTUSRFLD.DST-USR-FLD-01:"Dist User Field 1:"
                    DSTUSRFLD.DST-USR-FLD-02:"Dist User Field 2:"	
                    DSTUSRFLD.DST-USR-FLD-03:"Dist User Field 3:"	
                    DSTUSRFLD.DST-USR-FLD-04:"Dist User Field 4:"	
                    DSTUSRFLD.DST-USR-FLD-05:"Dist User Field 5:"	

DEFINE OBJVIEW     "Invoice Transaction"
       ID           AP-APD-V-0002
       FILENAME     APDISTRIB
       RELATION     APINVOICE
       DSPFLDS      VENDOR:"Vendor",INVOICE:"Invoice",SUFFIX,
                    BASE-INV-AMT:"Amount", APCOMPANY.BASE-CURRENCY:""
                    
DEFINE OBJVIEW     "Related PCard Transactions"
       ID           AP-APD-V-0003
       FILENAME     APDISTRIB
       RELATION     PDCHRGDTL
       DSPFLDS      PCARD-PROGRAM:"Program", STATEMENT:"Statement", PCARD-NBR:"Card Number",
                    TRAN-DATE:"Transaction Date", MERCHANT:"Merchant", ITEM-CHARGE:"Amount"
                    
DEFINE OBJVIEW     "Related PCard Distributions"
       ID           AP-APD-V-0004
       FILENAME     APDISTRIB
       RELATION     PDCHRGDIST
       DSPFLDS      PCARD-PROGRAM:"Program", STATEMENT:"Statement",
                    DIST-TYPE:"Distribution Type", DIST-AMOUNT:"Amount"
                    
DEFINE OBJVIEW     "Archived Invoice Transaction"
       ID           AP-AH2-V-0002
       FILENAME     APAPDHIST
       RELATION     APAPIHIST
       DSPFLDS      VENDOR:"Vendor",INVOICE:"Invoice",SUFFIX,
                    BASE-INV-AMT:"Amount", APCOMPANY.BASE-CURRENCY:""

*** CBCHECK ***

DEFINE OBJWIN     "Transaction Detail"
       ID          AP-CHK-W-0001
       FILENAME    CBCHECK
       VALIDITY    NO-SERIAL-NBR
       WINFLDS     BANK-CURR-AMT          :"           Issued Amount:",CBCASHCODE.CURRENCY-CODE:""
                   CHK-STATUS             :"                  Status:"
                   CHECK-DATE             :"             Issued Date:"
                   CBTRANS.RECON-DATE     :"  Reconcile or Void Date:"
                   CBTRANS.RECON-BNK-AMT  :"       Reconciled Amount:"

                   PAID-VENDOR            :"                  Vendor:"
                   PAID-NAME              :"    Payee or Description:"

                   COMPANY                :"                 Company:"
                   PROC-LEVEL             :"           Process Level:"
                   PAY-GROUP              :"               Pay Group:"
                   PROC-GRP               :"           Process Group:"

DEFINE OBJWIN     "Transaction Detail"
       ID          AP-CHK-W-0002
       FILENAME    CBCHECK
       VALIDITY    TAPE-SER-NBR
       WINFLDS     BANK-CURR-AMT          :"           Issued Amount:",CBCASHCODE.CURRENCY-CODE:""
                   CHK-STATUS             :"                  Status:"
                   CHECK-DATE             :"             Issued Date:"
                   CBTRANS-TAPE.RECON-DATE:"  Reconcile or Void Date:"
                   REJECT-DATE            :"             Reject Date:"

                   PAID-VENDOR            :"                  Vendor:"
                   PAID-NAME              :"    Payee or Description:"

                   COMPANY                :"                 Company:"
                   PROC-LEVEL             :"           Process Level:"
                   PAY-GROUP              :"               Pay Group:"
                   PROC-GRP               :"           Process Group:"

DEFINE OBJWIN     "Transaction Detail"
       ID          AP-CHK-W-0003
       FILENAME    CBCHECK
       VALIDITY    TAPE-SER-NBR
       WINFLDS     BANK-CURR-AMT          :"           Issued Amount:",CBCASHCODE.CURRENCY-CODE:""
                   CHK-STATUS             :"                  Status:"
                   CHECK-DATE             :"             Issued Date:"
                   CBTRANS-TAPE.RECON-DATE:"  Reconcile or Void Date:"
                   REJECT-DATE            :"             Reject Date:"

                   PAID-VENDOR            :"                  Vendor:"
                   PAID-NAME              :"    Payee or Description:"

                   COMPANY                :"                 Company:"
                   PROC-LEVEL             :"           Process Level:"
                   PAY-GROUP              :"               Pay Group:"
                   PROC-GRP               :"           Process Group:"

DEFINE OBJWIN    "Electronic Transmission Serial Number"
       ID         AP-CHK-W-0004
       FILENAME   CBCHECK
       VALIDITY   TAPE-SER-NBR
       WINFLDS    SERIAL-NUM         :"       Serial Number:"

DEFINE OBJVIEW    "Invoices"
       ID          AP-CHK-V-0001
       FILENAME    CBCHECK
       RELATION    APPAYMENT
       DSPFLDS     APVENMAST.VENDOR-VNAME:"Vendor", INVOICE:"Invoice", SUFFIX:"", TRAN-CHK-AMT:"Net Paid Amt",
*ARG                   INV-CURRENCY:""

DEFINE OBJVIEW    "Archived Invoices"
       ID          AP-CHK-V-0002
       FILENAME    CBCHECK
       RELATION    APAPPHIST
       DSPFLDS     APVENMAST.VENDOR-VNAME:"Vendor", INVOICE:"Invoice", SUFFIX:"", TRAN-CHK-AMT:"Net Paid Amt",
*ARG                   INV-CURRENCY:""

*** APDRAFTS ***
*
DEFINE OBJWIN    "BOE Detail"
      ID         AP-APT-W-0001
      FILENAME   APDRAFTS
      WINFLDS    VENDOR-VNAME          :"                Vendor:"
                 DRAFT-NBR             :"                   BOE:", SUFFIX:""

                 DRAFT-AMOUNT          :"            BOE Amount:", CURRENCY-CODE:""
                 BASE-ORIG-AMT         :"  Original Base Amount:", APCOMPANY.BASE-CURRENCY:""
                 BASE-ACT-AMT          :"    Actual Base Amount:"

                 DRAFT-DATE            :"              BOE Date:"
                 ACCEPT-DATE           :"           Accept Date:"
                 MATURITY-DATE         :"              Due Date:"
                 CASH-DATE             :"             Cash Date:"

                 CASH-CODE             :"             Cash Code:"
                 DRAFT-CODE            :"      BOE Accrual Code:"

                 DERIVED-STATUS        :"                Status:", CANCEL-DATE:"", DISHONOR-DATE:""

DEFINE OBJVIEW   "Invoices"
      ID          AP-APT-V-0001
      FILENAME    APDRAFTS
      RELATION    APPAYMENT
      DSPFLDS     APDRAFTS.VENDOR-VNAME:"Vendor", INVOICE:"Invoice", SUFFIX:"", TRAN-NET-PMT:"Net Paid Amount",
                  INV-CURRENCY:""

DEFINE OBJVIEW   "Archived Invoices"
      ID          AP-APT-V-0002
      FILENAME    APDRAFTS
      RELATION    APAPPHIST
      DSPFLDS     APDRAFTS.VENDOR-VNAME:"Vendor", INVOICE:"Invoice", SUFFIX:"", TRAN-NET-PMT:"Net Paid Amount",
                  INV-CURRENCY:""

DEFINE OBJVIEW  "BOE GL Entries"
      ID         AP-APT-V-0003     
      FILENAME   APDRAFTS
      RELATION   APDRFTDIST
      DSPFLDS    DIST-COMPANY:"Co", DST-ACCT-UNIT:"Account", DST-ACCOUNT:"", DST-SUB-ACCT:"",
                 POST-DATE:"Post Date", TRAN-DIST-AMT:"Amount",  REC-STATUS:"Status"

*** APDRFTDIST

DEFINE OBJWIN  "BOE GL Entry Detail"
      ID        AP-DDT-W-0001
      FILENAME  APDRFTDIST
      WINFLDS   GLMASTER.ACCT-UNIT       :"                    GL Account:", GLMASTER.ACCOUNT:"", GLMASTER.SUB-ACCOUNT:""
                GLMASTER.ACCT-DESC=40    :"           Account Description:"

                TRAN-DIST-AMT            :"           Distribution Amount:", CURRENCY-CODE:""
                BASE-DIST-AMT            :"      Base Distribution Amount:"

DEFINE OBJVIEW "BOE Transaction"
     ID         AP-DDT-V-0001
     FILENAME   APDRFTDIST
     RELATION   APDRAFTS
     DSPLFLDS   DRAFT-NBR:"Number", APVENMAST.SHORT-VNAME:"Vendor Name", DRAFT-DATE:"Date", DRAFT-AMOUNT:"BOE Amount", CURRENCY-CODE:""

DEFINE OBJVIEW "Associated Invoice"
     ID         AP-DDT-V-0002
     FILENAME   APDRFTDIST
     VALIDITY   INVOICE-DIST
     RELATION   APPAYMENT
     DSPLFLDS   VENDOR:"Vendor",INVOICE:"Invoice", APINVOICE.INVOICE-DTE:"Inv Date", TRAN-PMT-AMT:"Amount", INV-CURRENCY:"Curr"

DEFINE OBJVIEW "Archived Associated Invoice"
     ID         AP-DDT-V-0003
     FILENAME   APDRFTDIST
     VALIDITY   INVOICE-DIST
     RELATION   APAPPHIST
     DSPLFLDS   VENDOR:"Vendor",INVOICE:"Invoice", APAPIHIST.INVOICE-DTE:"Inv Date", TRAN-PMT-AMT:"Amount", INV-CURRENCY:"Curr"

*** APPROCLEV

DEFINE OBJWIN     "Address"
       ID          AP-APR-W-0001
       FILENAME    APPROCLEV
       WINFLDS     NAME           :"          Name:"
                   ADDR1          :"       Address:"
                   ADDR2          :"               "
                   ADDR3          :"               "
                   ADDR4          :"               "
                   CITY-ADDR5     :"               "
                   STATE-PROV     :"State,Province:",POSTAL-CODE:"Postal Code:"
                   COUNTY         :"        County:"
                   COUNTRY        :"               "
***
DEFINE OBJWIN     "Defaults"
       ID          AP-APR-W-0002
       FILENAME    APPROCLEV
       WINFLDS     NAME           :"                     Name:"
                   DEF-PROC-LEVEL :"          Company Default:"
                   PAY-GROUP      :"                Pay Group:"

                   ACCR-CODE      :"             Accrual Code:"
                   DISCOUNT-CODE  :"            Discount Code:"
                   INC-ACCR-CODE  :"  Income Withholding Code:"
                   CASH-CODE      :"                Cash Code:"
                   HANDLING-CODE  :"            Handling Code:"
                   DIST-CODE      :"Transit Distribution Code:"

*** APPAYGROUP
DEFINE OBJWIN     "Pay Group"
    ID             AP-PAY-W-0001
    FILENAME       APPAYGROUP
    WINFLDS        NAME                   :"         Pay Group Name:"

                   POST-COMPANY           :"        Posting Company:"
                   VENDOR-GROUP           :"           Vendor Group:"
                   ACTIVE-STATUS          :"                 Status:"

                   BASE-CURRENCY          :"          Base Currency:"

                   LAST-DRAFT-NBR         :"        Last BOE Number:"
                   ACCEPT-ACCTING         :"  Acceptance Accounting:"
                   DRAFT-NBR-OPT          :"      BOE Number Option:"

DEFINE OBJWIN     "Defaults"
    ID             AP-PAY-W-0002
    FILENAME       APPAYGROUP
    WINFLDS        CASH-CODE              :"              Cash Code:"
                   BANK-INST-CODE         :"           Payment Code:"
                   DRAFT-CODE             :"       BOE Accrual Code:"

DEFINE OBJVIEW    "Companies"
    ID            AP-PAY-V-0001
    FILENAME      APPAYGROUP
    RELATION      APPAYCOREL
    DSPFLDS       COMPANY:"Company", PROC-LEVEL:"Process Level"

DEFINE OBJVIEW    "Monitor"
    ID            AP-PAY-V-0002
    FILENAME      APPAYGROUP
    RELATION      APMONITOR
    DSPFLDS       RUN-PROG:"Program", PROC-GRP:"Proc Group", PROC-LEVEL:"PL", BATCH-NUM:"Batch",
                  AUTH-CODE:"Auth"

*** APTAXGROUP

DEFINE OBJWIN    "Reportable Income Group"
    ID            AP-TXP-W-0001
    FILENAME      APTAXGROUP
    WINFLDS       NAME            :"                 Name:"
                  TAX-ID          :"               Tax ID:"

                  TAPE-CONTROL    :"  Transmitter Control:"
                  PAYER-NUMBER    :"   Payer State Number:"
                  PAYER-NAME      :"           Payer Name:"

DEFINE OBJWIN    "Address"
    ID            AP-TXP-W-0002
    FILENAME      APTAXGROUP
    WINFLDS       ADDR1             :"         Address:"
                  ADDR2             :"                 "
                  ADDR3             :"                 "
                  ADDR4             :"                 "
                  CITY-ADDR5        :"            City:"
                  STATE-PROV        :"   State or Prov:"
                  POSTAL-CODE       :"     Postal Code:"
                  COUNTY            :"          County:"
                  COUNTRY           :"         Country:"

DEFINE OBJVIEW   "Companies"
    ID            AP-TXP-V-0001
    FILENAME      APTAXGROUP
    RELATION      APTAXENT
    DSPFLDS       COMPANY:"Company", PROC-LEVEL:"Proc Level"
*********************************************************************
*          Attachments for Vendor Comment AP12
*********************************************************************
DEFINE OBJWIN     "Vendor Comments"
       ID         AP-APC-W-0001
       FILENAME   APCOMMENTS
       COMMENT    TYPE=A
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  LOCATION-CODE: "Location Code:"

DEFINE OBJWIN     "Invoice Note/Report/Check Comments"
       ID         AP-API-W-0017
       FILENAME   APINVOICE
       COMMENT    TYPE=A
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Invoice Notes"
       ID         AP-API-W-0018
       FILENAME   APINVOICE
       COMMENT    TYPE=N
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Invoice Report Comments"
       ID         AP-API-W-0019
       FILENAME   APINVOICE
       COMMENT    TYPE=D
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Invoice Check Comments"
       ID         AP-API-W-0020
       FILENAME   APINVOICE
       COMMENT    TYPE=C
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Invoice Discount"
       ID         AP-API-W-0023
       FILENAME   APINVOICE
       WINFLDS    INVOICE:       "                Invoice:"
                  RCPT-INV-DATE: "   Invoice Receipt Date:"
                  TERMS-CD:      "             Terms Code:"

                  DISCOUNT-CODE: "          Discount Code:"
                  FLEX-FLAG:     "         Flexible Terms:"
                  ANTICIPATION:  "             Anticipate:"
                  TRAN-ALOW-AMT: "              Allowable:"

                  DISC-DATE1:"Dates:", DISCOUNT-RT1: "  Rates:", TRAN-DISC-AMT1: "  Trans Amt1:"
                  DISC-DATE2:"      ", DISCOUNT-RT2: "        ", TRAN-DISC-AMT2: "  Trans Amt2:"
                  DISC-DATE3:"      ", DISCOUNT-RT3: "        ", TRAN-DISC-AMT3: "  Trans Amt3:"

DEFINE OBJWIN     "Invoice Payment Addendum"
       ID         AP-API-W-0022
       FILENAME   APINVOICE
       COMMENT    TYPE=H
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Conversion Invoice Note/Report/Check Comments"
       ID         AP-CVI-W-0001
       FILENAME   APCINVOICE
       COMMENT    TYPE=A
       WINFLDS    COMPANY:       "      Company:"
                  EDI-NBR:       "   EDI Number:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Conversion Invoice Notes"
       ID         AP-CVI-W-0002
       FILENAME   APCINVOICE
       COMMENT    TYPE=N
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  EDI-NBR:       "   EDI Number:"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Conversion Invoice Reports Comments"
       ID         AP-CVI-W-0003
       FILENAME   APCINVOICE
       COMMENT    TYPE=D
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  EDI-NBR:       "   EDI Number:"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Conversion Invoice Check Comments"
       ID         AP-CVI-W-0004
       FILENAME   APCINVOICE
       COMMENT    TYPE=C
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  EDI-NBR:       "   EDI Number:"
                  INVOICE:       "      Invoice:"
DEFINE OBJWIN    "URL Attachment"
       ID         AP-CVI-W-0005
       FILENAME   APCINVOICE
       URL        TYPE=I
       URLSCHEME "http"
       WINFLDS    VENDOR:     "Vendor :"
                  INVOICE:    "Invoice:"
                  SUFFIX:     "Suffix :"
                  
DEFINE OBJWIN     "Conversion Invoice Addendum"
       ID         AP-CVI-W-0006
       FILENAME   APCINVOICE
       COMMENT    TYPE=H
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"                  

DEFINE OBJWIN     "Archived Invoice Note/Report/Check Comments"
       ID         AP-AH1-W-0010
       FILENAME   APAPIHIST
       COMMENT    TYPE=A
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Notes"
       ID         AP-AH1-W-0011
       FILENAME   APAPIHIST
       COMMENT    TYPE=N
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Report Comments"
       ID         AP-AH1-W-0012
       FILENAME   APAPIHIST
       COMMENT    TYPE=D
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Check Comments"
       ID         AP-AH1-W-0013
       FILENAME   APAPIHIST
       COMMENT    TYPE=C
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Addendum"
       ID         AP-AH1-W-0014
       FILENAME   APAPIHIST
       COMMENT    TYPE=H
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Note/Report/Check Comments"
       ID         AP-AH3-W-0006
       FILENAME   APAPPHIST
       COMMENT    TYPE=A
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Notes"
       ID         AP-AH3-W-0007
       FILENAME   APAPPHIST
       COMMENT    TYPE=N
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Report Comments"
       ID         AP-AH3-W-0008
       FILENAME   APAPPHIST
       COMMENT    TYPE=D
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "Archived Invoice Check Comments"
       ID         AP-AH3-W-0009
       FILENAME   APAPPHIST
       COMMENT    TYPE=C
       WINFLDS    COMPANY:       "      Company:"
                  VENDOR :       "      Vendor :"
                  INVOICE:       "      Invoice:"

DEFINE OBJWIN     "AP Distribution Comments"
       ID		  AP-APD-W-0008	
       FILENAME   APDISTRIB 
       COMMENT    TYPE=A
       WINFLDS    API-OBJ-ID  :  "   API Obj ID:"
                  DIST-SEQ-NBR:  " Sequence Nbr:"

*** APUSRCLASS

DEFINE OBJVIEW    "Users"
       ID         AP-APK-V-0001
       FILENAME   APUSRCLASS
       RELATION   APUSER
       DSPFLDS    USER-ID: "User ID", INQUIRE-ACCESS:"Inq", UPDATE-ACCESS:"Upd"

*** APHOLDCODE

DEFINE OBJWIN    "AP Hold Code"
    ID            AP-HLD-W-0001
    FILENAME      APHOLDCODE
    WINFLDS       VENDOR-GROUP    :"      Vendor Group:"
                  HLD-CODE        :"         Hold Code:"
                  DESCRIPTION     :"       Description:"
                  HOLD-VEN-FLAG   :"       Vendor Flag:"
                  HOLD-INV-FLAG   :"      Invoice Flag:"

*** AOC DETAILS

DEFINE OBJWIN    "Archived AOC Details"
    ID            AP-MAA-W-0001
    FILENAME      MAAOCDTL
    WINFLDS       INVOICE         :"           Invoice:"
                  PO-NUMBER       :"         PO Number:",PO-RELEASE,PO-CODE
                  LINE-NBR        :"       Line Number:"
                  AOC-CODE        :"          AOC Code:"
                  AOC-RATE        :"          AOC Rate:"
                  TOTAL-AOC       :"         Total AOC:"
                  AOC-ON-PO       :"         AOC On PO:"
                  AOC-AMT         :"        AOC Amount:"

*** APDISTPAY

DEFINE OBJWIN    "Retainage Detail"
    ID            AP-DTY-W-0001
    FILENAME      APDISTPAY
    WINFLDS       COMPANY:                "                     Company:"
                  PO-NUMBER:              "              Purchase Order:",PO-RELEASE:"",PO-CODE:""
                  LINE-NBR:               "                 Line Number:"
                  MAINVDTL.ITEM:          "                        Item:"
                  INVOICE:                "                     Invoice:"
                  APDISTRIB.DIS-ACCT-UNIT:"                     Account:",APDISTRIB.DIS-ACCOUNT:"",APDISTRIB.DIS-SUB-ACCT:""
                  PMT-AMOUNT:             "         Distribution Amount:"
                  RET-PCT:                "           Retainage Percent:"
                  RET-CALC-AMT:           "Retainage Calculation Amount:"

DEFINE OBJVIEW    "Vendor Agreement"
       ID         AP-DTY-V-0001
       FILENAME   APDISTPAY
       VALIDITY   AGRMT-EXISTS
       RELATION   POVAGRMTH
       DSPFLDS    VEN-AGRMT-REF:"Agreement Reference",DESCRIPTION:"Description",
                  EFFECTIVE-DT:"Effective Date",EXPIRE-DT:"Expire Date:",
                  RET-PCT-COMP:"Percent Complete"

DEFINE OBJVIEW    "Vendor Agreement Line"
       ID         AP-DTY-V-0002
       FILENAME   APDISTPAY
       RELATION   POVAGRMTLN
       DSPFLDS    VEN-AGRMT-REF:"Agreement Reference",ITEM:"Item",
                  DESCRIPTION:"Description",RET-PCT-COMP:"Percent Complete"

DEFINE OBJVIEW    "Purchase Order"
       ID         AP-DTY-V-0003
       FILENAME   APDISTPAY
       VALIDITY   NON-SERVICE
       RELATION   PURCHORDER
       DSPFLDS    COMPANY:"Company",PO-NUMBER:"Purchase Order",PO-RELEASE,
                  PO-CODE,TOT-PRD-AMT:"PO Amount"

DEFINE OBJVIEW    "Purchase Order Line"
       ID         AP-DTY-V-0004
       FILENAME   APDISTPAY
       VALIDITY   NON-SERVICE
       RELATION   POLINE
       DSPFLDS    COMPANY:"Company",PO-NUMBER:"Purchase Order",PO-RELEASE,
                  PO-CODE,EXTENDED-AMT:"Extended Cost"

DEFINE OBJVIEW    "Invoice"
       ID         AP-DTY-V-0005
       FILENAME   APDISTPAY
       RELATION   APINVOICE
       DSPFLDS    INVOICE:"Invoice",SUFFIX,TRAN-INV-AMT:"Amount",
                  RET-TRAN-AMT:"Retainage Amount"

DEFINE OBJVIEW    "Payment"
       ID         AP-DTY-V-0006
       FILENAME   APDISTPAY
       RELATION   APPAYMENT
       DSPFLDS    INVOICE:"Invoice",SUFFIX,DUE-DATE:"Due Date",
                  TRAN-NET-PMT:"Payment Amount",APHLDINV.HLD-CODE:"Hold",
                  TRANS-NBR:"Payment Nbr",BANK-STATUS:"Status"

DEFINE OBJVIEW    "Invoice Line Detail"
       ID         AP-DTY-V-0007
       FILENAME   APDISTPAY
       VALIDITY   NON-AOC-DIST
       RELATION   MAINVDTL
       DSPFLDS    MATCH-DTL-KEY=41:"Item Detail",ITEM-TYPE:"Type",
                  TOT-DIST-AMT:"Total Amt",RET-PCT:"Original Ret Percent"

DEFINE OBJVIEW    "Invoice Line Detail"
       ID         AP-DTY-V-0008
       FILENAME   APDISTPAY
       VALIDITY   PO-AOC-DIST 
       RELATION   MAAOCDTL
       DSPFLDS    MATCH-DTL-KEY=41:"Item Detail",ITEM-TYPE:"Type",
                  AOC-CODE:"Code",TOT-DIST-AMT:"Total Amt",
                  RET-PCT:"Original Ret Percent"

DEFINE OBJVIEW    "Distributions"
       ID         AP-DTY-V-0009
       FILENAME   APDISTPAY
       RELATION   APDISTRIB
       DSPFLDS    DISTRIB-DATE:"Post Date",DIST-COMPANY:"Co",
                  DIS-ACCT-UNIT:"Account",DIS-ACCOUNT,DIS-SUB-ACCT,
                  ORIG-TRAN-AMT:"Amount",TAX-CODE:"Tax Code",TAX-INDICATOR

DEFINE OBJWIN    "Non-AP Transaction Comments"
   ID             AP-RIC-W-0001
   FILENAME       APRPTBLINC
   COMMENT        TYPE=A
   WINFLDS        SOURCE      :"         Source:"
                  PAYEE-NAME  :"     Payee Name:"
                  TRANS-NBR   :" Payment Number:"

