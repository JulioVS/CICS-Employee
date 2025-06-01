       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVIEWP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'VIEW EMPLOYEE DETAILS' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - VIEW EMPLOYEE DETAILS MAPSET.
      *      - EMPLOYEE MASTER RECORD.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      ******************************************************************
       COPY ECONST.
       COPY EDETMAP.
       COPY EMPMAST.
       COPY EMONCTR.
       COPY DFHAID.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE          PIC S9(8) USAGE IS BINARY.
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-MESSAGE                PIC X(79) VALUE SPACES.
          05 WS-PF7-LABEL              PIC X(9)  VALUE 'PF7 Prev '.
          05 WS-PF8-LABEL              PIC X(9)  VALUE 'PF8 Next '.
      *
       01 WS-DEBUG-AID                 PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER                    PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT             PIC X(45) VALUE SPACES.
          05 FILLER                    PIC X(1)  VALUE '>'.
          05 FILLER                    PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP          PIC 9(8)  VALUE ZEROES.
          05 FILLER                    PIC X(1)  VALUE '>'.
          05 FILLER                    PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2         PIC 9(8)  VALUE ZEROES.
          05 FILLER                    PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE                PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                      VALUE 'Y'.
          88 NOT-DEBUGGING                       VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-FIRST-INTERACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *-----------------------------------------------------------------
       VIEWING SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *-----------------------------------------------------------------
       ACTIVITY-MONITOR SECTION.
      *-----------------------------------------------------------------

       4000-CHECK-USER-STATUS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4000-CHECK-USER-STATUS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-SEND-MAP-AND-RETURN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

       9300-DEBUG-AID.
      *    >>> DEBUGGING ONLY <<<
           IF I-AM-DEBUGGING THEN
              INITIALIZE WS-DEBUG-MESSAGE

              MOVE WS-DEBUG-AID TO WS-DEBUG-TEXT
              MOVE EIBRESP TO WS-DEBUG-EIBRESP
              MOVE EIBRESP2 TO WS-DEBUG-EIBRESP2

              EXEC CICS SEND TEXT
                   FROM (WS-DEBUG-MESSAGE)
                   END-EXEC
              EXEC CICS RECEIVE
                   LENGTH(LENGTH OF EIBAID)
                   END-EXEC

              INITIALIZE EIBRESP EIBRESP2
           END-IF.
      *    >>> -------------- <<<