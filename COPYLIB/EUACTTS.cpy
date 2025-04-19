      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - USER ACTIVITY QUEUE.
      *      - RECORD LAYOUT FOR 'EUACTTS' TEMPORARY STORAGE QUEUE.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 USER-ACTIVITY-RECORD.
          05 UA-USER-ID                 PIC X(8).
          05 UA-USER-CATEGORY           PIC X(3).
             88 UA-CT-STANDARD                   VALUE 'STD'.
             88 UA-CT-MANAGER                    VALUE 'MGR'.
             88 UA-CT-ADMINISTRATOR              VALUE 'ADM'.
             88 UA-CT-NOT-SET                    VALUE SPACES.
          05 UA-USER-SIGN-ON-STATUS     PIC X(1).
             88 UA-ST-IN-PROCESS                 VALUE 'I'.
             88 UA-ST-LOCKED-OUT                 VALUE 'L'.
             88 UA-ST-SIGNED-ON                  VALUE 'S'.
             88 UA-ST-NOT-SET                    VALUE SPACES.
          05 UA-RETRY-NUMBER            PIC 9(2).
          05 UA-LAST-ACTIVITY-TIMESTAMP.
             10 UA-LAST-ACTIVITY-DATE   PIC X(8).
             10 UA-LAST-ACTIVITY-TIME   PIC X(6).