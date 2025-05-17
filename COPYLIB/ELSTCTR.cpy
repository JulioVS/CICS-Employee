      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP ' - EMPLOYEE LIST.
      *      - RECORD LAYOUT FOR 'ELSTCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 LIST-EMPLOYEE-CONTAINER.
          05 LST-USER-CATEGORY          PIC X(3).
             88 LST-CT-ADMINISTRATOR               VALUE 'ADM'.
             88 LST-CT-MANAGER                     VALUE 'MGR'.
             88 LST-CT-STANDARD                    VALUE 'STD'.
             88 LST-CT-NOT-SET                     VALUE SPACES.
          05 LST-PROGRAM-NAME           PIC X(8).
          05 LST-CURRENT-PAGE-NUMBER    PIC 9(6).
      *
          05 LST-FILE-FLAG              PIC X(1)   VALUE SPACES.
             88 LST-TOP-OF-FILE                    VALUE 'T'.
             88 LST-END-OF-FILE                    VALUE 'E'.
             88 LST-NOT-SET                        VALUE SPACES.
      *
      *   LIST EMPLOYEES MAP DETAIL SECTION.
      *
          05 LST-CURRENT-RECORD-AREA.
             10 LST-CURRENT-RECORD
                   OCCURS 16 TIMES
                   INDEXED BY LST-RECORD-INDEX
                                        PIC X(251).
      *
      *   FILTERS MAP CRITERIA SECTION.
      *
          05 LST-FILTERS.
             10 LST-FILTERS-FLAG        PIC X(1)   VALUE SPACES.
                88 LST-FILTERS-SET                 VALUE 'Y'.
                88 LST-NO-FILTERS-SET              VALUE SPACES.
             10 LST-SELECT-KEY-TYPE     PIC X(1)   VALUE SPACES.
                88 LST-SEL-BY-EMPLOYEE-ID          VALUE '1'.
                88 LST-SEL-BY-EMPLOYEE-NAME        VALUE '2'.
             10 LST-SELECT-KEY-VALUE    PIC X(30)  VALUE SPACES.
             10 LST-INCLUDE-DEPT-FILTERS.
                15 LST-INCL-DEPT-ID
                      OCCURS 4 TIMES
                      INDEXED BY LST-IN-DEPT-INDEX
                                        PIC X(8).
             10 LST-EXCLUDE-DEPT-FILTERS.
                15 LST-EXCL-DEPT-ID
                      OCCURS 4 TIMES
                      INDEXED BY LST-EX-DEPT-INDEX
                                        PIC X(8).
             10 LST-EMPLOYMENT-DATE-FILTERS.
                15 LST-EMPL-DATE-AFTER  PIC X(8).
                15 LST-EMPL-DATE-BEFORE PIC X(8).