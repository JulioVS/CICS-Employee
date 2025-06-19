      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP ' - ADD EMPLOYEE.
      *      - LAYOUT FOR 'EADDCTR' CONTAINER.
      *      - NON-PERSISTENT (NO ASSOCIATED FILE).
      *      - NEEDED FOR PSEUDO-CONVERSATIONAL DESIGN.      
      ******************************************************************
       01 ADD-EMPLOYEE-CONTAINER.
          05 ADD-USER-ID          PIC X(8).
          05 ADD-EMPLOYEE-RECORD  PIC X(251).