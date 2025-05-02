      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP ' - EMPLOYEE LIST.
      *      - RECORD LAYOUT FOR 'ELSTCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 LIST-EMPLOYEE-CONTAINER.
          05 LST-USER-CATEGORY          PIC X(3).
          05 LST-PROGRAM-NAME           PIC X(8).
          05 LST-CURRENT-PAGE-NUMBER    PIC 9(6).
          05 LST-FILE-FLAG              PIC X(1).
             88 LST-START-OF-FILE                  VALUE 'S'.
             88 LST-END-OF-FILE                    VALUE 'E'.
             88 LST-NOT-SET                        VALUE SPACE.
          05 LST-FILTERS.
             10 LST-FILTER-PRIMARY-NAME PIC X(38). 
             10 LST-FILTER-JOB-TITLE    PIC X(38).
             10 LST-FILTER-DEPARTMENT-ID
                                        PIC 9(8).
          05 LST-FILTERS-FLAG REDEFINES LST-FILTERS
                                        PIC X(84).
             88 LST-NO-FILTERS-SET                 VALUE SPACES.
          05 LST-CURRENT-RECORD-AREA.
             10 LST-CURRENT-RECORD
                   OCCURS 16 TIMES
                   INDEXED BY LST-RECORD-INDEX
                                        PIC X(251).