      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - ACTIVITY MONITOR.
      *      - RECORD LAYOUT FOR 'EMONCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 ACTIVITY-MONITOR-CONTAINER.
          05 MON-REQUEST.
             10 MON-LINKING-PROGRAM     PIC X(8).
             10 MON-USER-ID             PIC X(8).
             10 MON-USER-ACTION         PIC X(1).
                88 MON-AC-NOTIFY                  VALUE 'N'.
                88 MON-AC-SIGN-ON                 VALUE 'S'.
                88 MON-AC-SIGN-OFF                VALUE 'F'.                
                88 MON-AC-APP-FUNCTION            VALUE 'A'.
                88 MON-AC-NOT-SET                 VALUE SPACES.
          05 MON-RESPONSE.
             10 MON-RESPONSE-CODE       PIC X(1).
                88 MON-PROCESSING-ERROR           VALUE 'E'.
                88 MON-SUCCESFUL-RUN              VALUE SPACES.
             10 MON-SIGN-ON-STATUS      PIC X(1).
                88 MON-ST-IN-PROCESS              VALUE 'I'.
                88 MON-ST-LOCKED-OUT              VALUE 'L'.
                88 MON-ST-SIGNED-ON               VALUE 'S'.
                88 MON-ST-NOT-SET                 VALUE SPACES.
             10 MON-USER-CATEGORY       PIC X(3).
                88 MON-CT-STANDARD                VALUE 'STD'.
                88 MON-CT-MANAGER                 VALUE 'MGR'.
                88 MON-CT-ADMINISTRATOR           VALUE 'ADM'.
                88 MON-CT-NOT-SET                 VALUE SPACES.
             10 MON-MESSAGE             PIC X(79).