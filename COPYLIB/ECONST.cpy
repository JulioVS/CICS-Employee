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
                USAGE IS BINARY                   VALUE +1.
          05 APP-SIGNON-RULES-RRN       PIC S9(8)
                USAGE IS BINARY                   VALUE +1.
      *      ACTIVITY MONITOR
          05 APP-ACTMON-PROGRAM-NAME    PIC X(8)  VALUE 'EACTMON'.
          05 APP-ACTMON-CONTAINER-NAME  PIC X(16) VALUE 'MONContainer'.
          05 APP-ACTMON-CHANNEL-NAME    PIC X(16) VALUE 'DHFTRANSACTION'
           .
      *      USER ACTIVITY QUEUE
          05 APP-ACTMON-QUEUE-PREFIX    PIC X(8)  VALUE 'EUSERACT'.
          05 APP-ACTMON-ITEM-NUM        PIC S9(4)
                USAGE IS BINARY                   VALUE +1.
      *
      *      LANDING PAGE (ENTRY POINT)
      * ---------------------------------------------------------------
          05 APP-LANDING-PROGRAM-NAME   PIC X(8)  VALUE 'EMENUP'.
      * ---------------------------------------------------------------
      *
      *      LIST EMPLOYEES
          05 APP-LIST-TRANSACTION-ID    PIC X(4)  VALUE 'ELST'.
          05 APP-LIST-PROGRAM-NAME      PIC X(8)  VALUE 'ELISTP'.
          05 APP-LIST-MAPSET-NAME       PIC X(7)  VALUE 'ELSTMAP'.
          05 APP-LIST-MAP-NAME          PIC X(7)  VALUE 'ELSTM'.
          05 APP-LIST-CONTAINER-NAME    PIC X(16) VALUE 'LSTContainer'.
          05 APP-LIST-CHANNEL-NAME      PIC X(16) VALUE 'DHFTRANSACTION'
           .
      *      EMPLOYEE MASTER FILE
          05 APP-EMP-MASTER-FILE-NAME   PIC X(8)  VALUE 'EMPMAST'.
          05 APP-EMP-MASTER-PATH-NAME   PIC X(8)  VALUE 'EMPNAME'.
      *      FILTERS
          05 APP-FILTERS-MAP-NAME       PIC X(7)  VALUE 'EFILM'.
      *      VIEW EMPLOYEE DETAILS
          05 APP-VIEW-TRANSACTION-ID    PIC X(4)  VALUE 'EDET'.
          05 APP-VIEW-PROGRAM-NAME      PIC X(8)  VALUE 'EVIEWP'.
          05 APP-VIEW-MAPSET-NAME       PIC X(7)  VALUE 'EDETMAP'.
          05 APP-VIEW-MAP-NAME          PIC X(7)  VALUE 'EDETM'.
          05 APP-VIEW-CONTAINER-NAME    PIC X(16) VALUE 'DETContainer'.
          05 APP-VIEW-CHANNEL-NAME      PIC X(16) VALUE 'DHFTRANSACTION'
           .
      *      MAIN MENU 
          05 APP-MENU-TRANSACTION-ID    PIC X(4)  VALUE 'EMNU'.
          05 APP-MENU-PROGRAM-NAME      PIC X(8)  VALUE 'EMENUP'.
          05 APP-MENU-MAPSET-NAME       PIC X(7)  VALUE 'EMNUMAP'.
          05 APP-MENU-MAP-NAME          PIC X(7)  VALUE 'EMNUM'.
          05 APP-MENU-CONTAINER-NAME    PIC X(16) VALUE 'MNUContainer'.
          05 APP-MENU-CHANNEL-NAME      PIC X(16) VALUE 'DHFTRANSACTION'
           .
      *      ADD EMPLOYEE 
          05 APP-ADD-TRANSACTION-ID     PIC X(4)  VALUE 'EADD'.
          05 APP-ADD-PROGRAM-NAME       PIC X(8)  VALUE 'EADDP'.
          05 APP-ADD-MAPSET-NAME        PIC X(7)  VALUE 'EADDMAP'.
          05 APP-ADD-MAP-NAME           PIC X(7)  VALUE 'EADDM'.
          05 APP-ADD-CONTAINER-NAME     PIC X(16) VALUE 'ADDContainer'.
          05 APP-ADD-CHANNEL-NAME       PIC X(16) VALUE 'DHFTRANSACTION'
           .      
      *      AUDIT TRAIL 
          05 APP-AUDIT-TRANSACTION-ID   PIC X(4)  VALUE 'EAUD'.
          05 APP-AUDIT-PROGRAM-NAME     PIC X(8)  VALUE 'EAUDITP'.
          05 APP-AUDIT-TRAIL-FILE-NAME  PIC X(8)  VALUE 'EAUDIT'.
          05 APP-AUDIT-REQUEST-ID       PIC X(8)  VALUE 'EAUDITRQ'.
          05 APP-AUDIT-TESTING-PROGRAM  PIC X(8)  VALUE 'EAUDITT'.
          05 APP-AUDIT-TESTING-TRN-ID   PIC X(4)  VALUE 'EAUT'.