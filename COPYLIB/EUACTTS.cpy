      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - USER ACTIVITY QUEUE.
      *      - RECORD LAYOUT FOR 'EUACTTS' TEMPORARY STORAGE QUEUE.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 USER-ACTIVITY-RECORD.
          05 ACT-USER-ID                PIC X(8).
          05 ACT-USER-CATEGORY          PIC X(3).
             88 ACT-CT-STANDARD                  VALUE 'STD'.
             88 ACT-CT-MANAGER                   VALUE 'MGR'.
             88 ACT-CT-ADMINISTRATOR             VALUE 'ADM'.
             88 ACT-CT-NOT-SET                   VALUE SPACES.
          05 ACT-USER-SIGN-ON-STATUS    PIC X(1).
             88 ACT-ST-IN-PROCESS                VALUE 'I'.
             88 ACT-ST-LOCKED-OUT                VALUE 'L'.
             88 ACT-ST-SIGNED-ON                 VALUE 'S'.
             88 ACT-ST-NOT-SET                   VALUE SPACES.
          05 ACT-ATTEMPT-NUMBER         PIC 9(2).
          05 ACT-LAST-ACTIVITY-TIMESTAMP.
             10 ACT-LAST-ACTIVITY-DATE  PIC X(8).
             10 ACT-LAST-ACTIVITY-TIME  PIC X(6).