      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - APPLICATION CONSTANTS FOR 'EMPLOYEE' CICS APP.
      ******************************************************************
       01 APPLICATION-CONSTANTS.
      *      SIGN-ON PROCESS 
          05 APP-SIGNON-TRANSACTION-ID  PIC X(4)  VALUE 'ESON'.
          05 APP-SIGNON-PROGRAM-NAME    PIC X(7)  VALUE 'ESONP'.
          05 APP-SIGNON-MAPSET-NAME     PIC X(7)  VALUE 'ESONMAP'.
          05 APP-SIGNON-MAP-NAME        PIC X(7)  VALUE 'ESONM'.
      *      REGISTERED USERS
          05 APP-REG-USER-FILE-NAME     PIC X(8)  VALUE 'EREGUSR'.
      *      SIGN-ON RULES
          05 APP-SIGNON-RULES-FILE-NAME PIC X(8)  VALUE 'ESONRUL'.
          05 APP-SIGNON-RULES-QUEUE-NAME
                                        PIC X(16) VALUE 'ESONRUL'.
          05 APP-SIGNON-RULES-ITEM-NUM  PIC S9(4)
                USAGE IS BINARY                   VALUE 1.
          05 APP-SIGNON-RULES-RRN       PIC S9(8)
                USAGE IS BINARY                   VALUE 1.
      *      ACTIVITY MONITOR
          05 APP-ACTMON-PROGRAM-NAME    PIC X(8)  VALUE 'EACTMON'.
          05 APP-ACTMON-CONTAINER-NAME  PIC X(16) VALUE 'MONContainer'.
          05 APP-ACTMON-CHANNEL-NAME    PIC X(16) VALUE 'DHFTRANSACTION'
           .
      *      USER ACTIVITY QUEUE
          05 APP-ACTMON-QUEUE-PREFIX    PIC X(8)  VALUE 'EUSERACT'.
          05 APP-ACTMON-ITEM-NUM        PIC S9(4)
                USAGE IS BINARY                   VALUE 1.
      *      LANDING PAGE (ENTRY POINT)
          05 APP-LANDING-PROGRAM-NAME   PIC X(8)  VALUE 'ELISTP'.
      *      LIST EMPLOYEES
          05 APP-LIST-TRANSACTION-ID    PIC X(4)  VALUE 'ELST'.
          05 APP-LIST-PROGRAM-NAME      PIC X(8)  VALUE 'ELISTP'.
          05 APP-LIST-MAPSET-NAME       PIC X(7)  VALUE 'ELSTMAP'.
          05 APP-LIST-MAP-NAME          PIC X(7)  VALUE 'ELSTM'.
          05 APP-LIST-NUM-LINES         PIC 9(2)  VALUE 16.
          05 APP-LIST-CONTAINER-NAME    PIC X(16) VALUE 'LSTContainer'.
          05 APP-LIST-CHANNEL-NAME      PIC X(16) VALUE 'DHFTRANSACTION'
           .
      *     EMPLOYEE MASTER FILE
          05 APP-EMP-MASTER-FILE-NAME   PIC X(8)  VALUE 'EMPMAST'.
      *     FILTERS 
          05 APP-FILTERS-MAP-NAME       PIC X(7)  VALUE 'EFILM'.       