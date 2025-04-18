      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'
      *      - APPLICATION CONSTANTS FOR 'EMPLOYEE' CICS APP.
      ******************************************************************
       01 APPLICATION-CONSTANTS.
      *      SIGN-ON PROCESS CONSTANTS
          05 AC-SIGNON-TRANSACTION-ID   PIC X(4)  VALUE 'ESON'.
          05 AC-SIGNON-PROGRAM-NAME     PIC X(7)  VALUE 'ESONP'.
          05 AC-SIGNON-MAP-NAME         PIC X(7)  VALUE 'ESONM'.
          05 AC-SIGNON-MAPSET-NAME      PIC X(7)  VALUE 'ESONMAP'.
      *      REGISTERED USERS
          05 AC-REG-USER-FILE-NAME      PIC X(8)  VALUE 'EREGUSR'.
      *      SIGN-ON RULES
          05 AC-SIGNON-RULES-FILE-NAME  PIC X(8)  VALUE 'ESONRUL'.
          05 AC-SIGNON-RULES-QUEUE-NAME PIC X(16) VALUE 'ESONRUL'.
          05 AC-SIGNON-RULES-ITEM-NUM   PIC S9(4)
                USAGE IS COMPUTATIONAL            VALUE 1.
          05 AC-SIGNON-RULES-RRN        PIC S9(8)
                USAGE IS COMPUTATIONAL            VALUE 1.
      *      ACTIVITY MONITOR
          05 AC-ACTMON-PROGRAM-NAME     PIC X(8)  VALUE 'EACTMON'.
          05 AC-ACTMON-CHANNEL-NAME     PIC X(16) VALUE 'DHFTRANSACTION'
           .
          05 AC-ACTMON-CONTAINER-NAME   PIC X(16) VALUE 'ACTMContainer'.
          05 AC-ACTMON-QUEUE-PREFIX     PIC X(8)  VALUE 'EUSERACT'.
          05 AC-ACTMON-ITEM-NUM         PIC S9(4)
                USAGE IS COMPUTATIONAL            VALUE 1.
      *      LIST EMPLOYEES
          05 AC-LANDING-PROGRAM-NAME    PIC X(8)  VALUE 'ELISTP'.