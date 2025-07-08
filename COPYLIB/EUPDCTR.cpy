      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - UPDATE EMPLOYEE.
      *      - LAYOUT FOR 'EUPDCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 UPDATE-EMPLOYEE-CONTAINER.
          05 UPD-USER-ID                PIC X(8).
      *
          05 UPD-USER-CATEGORY          PIC X(3).
             88 UPD-CT-STANDARD                    VALUE 'STD'.
             88 UPD-CT-MANAGER                     VALUE 'MGR'.
             88 UPD-CT-ADMINISTRATOR               VALUE 'ADM'.
             88 UPD-CT-NOT-SET                     VALUE SPACES.
      *
          05 UPD-USER-EMP-ID            PIC 9(8).
          05 UPD-USER-DEPT-ID           PIC 9(8).
          05 UPD-USER-ALT-KEY           PIC X(38).
      *
          05 UPD-SELECT-KEY.
             10 UPD-SELECT-KEY-TYPE     PIC X(1)   VALUE SPACES.
                88 UPD-SEL-BY-EMPLOYEE-ID          VALUE '1'.
                88 UPD-SEL-BY-EMPLOYEE-NAME        VALUE '2'.
             10 UPD-SELECT-KEY-VALUE    PIC X(30)  VALUE SPACES.
      *
          05 UPD-EMPLOYEE-RECORD        PIC X(251).
          05 UPD-ORIGINAL-RECORD        PIC X(251).
          05 UPD-FILTERS                PIC X(112).
      *
          05 UPD-FILE-FLAG              PIC X(1)   VALUE SPACES.
             88 UPD-TOP-OF-FILE                    VALUE 'T'.
             88 UPD-END-OF-FILE                    VALUE 'E'.
             88 UPD-NOT-SET                        VALUE SPACES.
      *
          05 UPD-CALLING-PROGRAM        PIC X(8).