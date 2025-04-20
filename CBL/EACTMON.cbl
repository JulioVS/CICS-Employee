       IDENTIFICATION DIVISION.
       PROGRAM-ID. EACTMON.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYE APP'
      *      - 'ACTIVITY MONITOR' PROGRAM
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - USER ACTIVITY QUEUE.
      *      - SIGN-ON RULES.
      ******************************************************************
       COPY ECONST.
       COPY EMONCTR.
       COPY EUACTTS.
       COPY ESONRUL.
      ******************************************************************
      *   DEFINE MY USER ACTIVITY QUEUE NAME.
      ******************************************************************
       01 WS-USER-ACTIVITY-QUEUE-NAME.
          05 WS-UA-QNAME-PREFIX        PIC X(8).
          05 WS-UA-QNAME-USERID        PIC X(8).
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          03 WS-ITEM-NUMBER            PIC S9(4) USAGE IS BINARY.
          03 WS-CICS-RESPONSE          PIC S9(8) USAGE IS BINARY.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-REQUEST.
           PERFORM 9000-RETURN-TO-CALLER.

      *-----------------------------------------------------------------
       SUB-ROUTINE SECTION.
      *-----------------------------------------------------------------

       1000-INITIALIZE.
           CONTINUE.

       2000-PROCESS-REQUEST.
           CONTINUE.

       9000-RETURN-TO-CALLER.
           EXEC CICS RETURN
                END-EXEC.