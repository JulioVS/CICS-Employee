      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - SIGN-ON RULES.
      *      - RECORD LAYOUT FOR 'ESONRUL' VSAM <RRDS> FILE.
      *      - VSAM CLUSTER -> 'Z45864.PSVS.ESONRUL'.
      *      - SEED DATASET -> 'Z45864.DATA.ESONRUL'.
      ******************************************************************
       01 SIGN-ON-RULES-RECORD.
          05 SIG-MAXIMUM-ATTEMPTS     PIC 9(2).
          05 SIG-LOCKOUT-INTERVAL     PIC 9(4).
          05 SIG-INACTIVITY-INTERVAL  PIC 9(4).