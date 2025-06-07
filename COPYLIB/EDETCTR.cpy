      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP ' - EMPLOYEE DETAILS.
      *      - LAYOUT FOR 'EDETCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 EMPLOYEE-DETAILS-CONTAINER.
          05 DET-USER-ID                PIC X(8).
      *
          05 DET-USER-CATEGORY          PIC X(3).
             88 DET-CT-STANDARD                    VALUE 'STD'.
             88 DET-CT-MANAGER                     VALUE 'MGR'.
             88 DET-CT-ADMINISTRATOR               VALUE 'ADM'.
             88 DET-CT-NOT-SET                     VALUE SPACES.
      *
          05 DET-SELECT-KEY.
             10 DET-SELECT-KEY-TYPE     PIC X(1)   VALUE SPACES.
                88 DET-SEL-BY-EMPLOYEE-ID          VALUE '1'.
                88 DET-SEL-BY-EMPLOYEE-NAME        VALUE '2'.
             10 DET-SELECT-KEY-VALUE    PIC X(30)  VALUE SPACES.
      *
          05 DET-EMPLOYEE-RECORD        PIC X(251).
          05 DET-FILTERS                PIC X(112).
      *
          05 DET-FILE-FLAG              PIC X(1)   VALUE SPACES.
             88 DET-TOP-OF-FILE                    VALUE 'T'.
             88 DET-END-OF-FILE                    VALUE 'E'.
             88 DET-NOT-SET                        VALUE SPACES.