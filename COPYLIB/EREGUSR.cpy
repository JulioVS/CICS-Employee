      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - REGISTERED USERS.
      *      - RECORD LAYOUT FOR 'EREGUSR' VSAM [KSDS] FILE.
      *      - VSAM CLUSTER -> 'Z45864.PSVS.EREGUSR'.
      *      - SEED DATASET -> 'Z45864.DATA.EREGUSR'.
      ******************************************************************
       01 REG-USER-RECORD.
          05 RU-USER-ID              PIC X(8).
          05 RU-USER-PASSWORD        PIC X(8).
          05 RU-USER-TYPE            PIC X(3).
             88 RU-UT-ADMINISTRATOR            VALUE 'ADM'.
             88 RU-UT-MANAGER                  VALUE 'MGR'.
             88 RU-UT-STANDARD                 VALUE 'STD'.
          05 RU-STATUS               PIC X(1).
             88 RU-ST-ACTIVE                   VALUE 'A'.
             88 RU-ST-INACTIVE                 VALUE 'I'.
          05 RU-LAST-EFFECTIVE-DATE  PIC X(14).
          05 RU-LED REDEFINES RU-LAST-EFFECTIVE-DATE.
             10 RU-LED-DATE          PIC X(8).
             10 RU-LED-TIME          PIC X(6).
          05 FILLER                  PIC X(66).