      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - REGISTERED USERS.
      *      - RECORD LAYOUT FOR 'EREGUSR' VSAM <KSDS> FILE.
      *      - VSAM CLUSTER -> 'Z45864.PSVS.EREGUSR'.
      *      - SEED DATASET -> 'Z45864.DATA.EREGUSR'.
      ******************************************************************
       01 REGISTERED-USER-RECORD.
          05 REG-KEY.
             10 REG-USER-ID             PIC X(8).
          05 REG-DETAILS.        
             10 REG-USER-PASSWORD       PIC X(8).
             10 REG-USER-CATEGORY       PIC X(3).
                88 REG-CT-STANDARD                VALUE 'STD'.
                88 REG-CT-MANAGER                 VALUE 'MGR'.
                88 REG-CT-ADMINISTRATOR           VALUE 'ADM'.
                88 REG-CT-NOT-SET                 VALUE SPACES.
             10 REG-STATUS              PIC X(1).
                88 REG-ST-ACTIVE                  VALUE 'A'.
                88 REG-ST-INACTIVE                VALUE 'I'.
                88 REG-ST-NOT-SET                 VALUE SPACES.
             10 REG-LAST-EFFECTIVE-DATE PIC X(14).
             10 REG-LED REDEFINES REG-LAST-EFFECTIVE-DATE.
                15 REG-LED-DATE         PIC X(8).
                15 REG-LED-TIME         PIC X(6).
             10 FILLER                  PIC X(66).