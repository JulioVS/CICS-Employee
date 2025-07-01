      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - ADD EMPLOYEE.
      *      - LAYOUT FOR 'EADDCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      ******************************************************************
       01 ADD-EMPLOYEE-CONTAINER.
          05 ADD-USER-ID          PIC X(8).
          05 ADD-DEPARTMENT-ID    PIC 9(8).
          05 ADD-EMPLOYEE-RECORD  PIC X(251).