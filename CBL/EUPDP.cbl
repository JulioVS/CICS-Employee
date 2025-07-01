       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUPDP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'UPDATE EMPLOYEE DETAILS' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - UPDATE DETAILS MAPSET.
      *      - UPDATE DETAILS CONTAINER.
      *      - EMPLOYEE MASTER RECORD.
      *      - VIEW DETAILS CONTAINER.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - REGISTERED USERS.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EUPDMAP.
       COPY EUPDCTR.
       COPY EMPMAST.
       COPY EDETCTR.
       COPY EMONCTR.
       COPY EREGUSR.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE       PIC S9(8) USAGE IS BINARY.
          05 WS-EMPLOYEE-ID         PIC X(8) JUSTIFIED RIGHT.
          05 WS-INSP-COUNTER        PIC S9(2) USAGE IS BINARY.
          05 WS-DEPT-KEY            PIC X(8).
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-MESSAGE             PIC X(79) VALUE SPACES.
          05 WS-PF7-LABEL           PIC X(9)  VALUE 'PF7 Prev '.
          05 WS-PF8-LABEL           PIC X(9)  VALUE 'PF8 Next '.
      *
       01 WS-DATE-FORMATTING.
          05 WS-INPUT-DATE.
             10 WS-YYYY             PIC X(4)  VALUE SPACES.
             10 WS-MM               PIC X(2)  VALUE SPACES.
             10 WS-DD               PIC X(2)  VALUE SPACES.
          05 WS-OUTPUT-DATE.
             10 WS-DD               PIC X(2)  VALUE SPACES.
             10 FILLER              PIC X(1)  VALUE '-'.
             10 WS-MM               PIC X(2)  VALUE SPACES.
             10 FILLER              PIC X(1)  VALUE '-'.
             10 WS-YYYY             PIC X(4)  VALUE SPACES.
      *
       01 WS-FILTER-FLAGS.
          03 WS-FILTERS-CHECK       PIC X(1)  VALUE SPACES.
             88 FILTERS-PASSED                VALUE 'Y'.
             88 FILTERS-FAILED                VALUE 'N'.
          03 WS-KEY-FILTER-CHECK    PIC X(1)  VALUE SPACES.
             88 KEY-FILTER-PASSED             VALUE 'Y'.
          03 WS-DEPT-FILTER-CHECK   PIC X(1)  VALUE SPACES.
             88 DEPT-FILTER-PASSED            VALUE 'Y'.
             88 DEPT-FILTER-FAILED            VALUE 'N'.
          03 WS-DATE-FILTER-CHECK   PIC X(1)  VALUE SPACES.
             88 DATE-FILTER-PASSED            VALUE 'Y'.
      *
       01 WS-DEBUG-AID              PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER                 PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT          PIC X(45) VALUE SPACES.
          05 FILLER                 PIC X(1)  VALUE '>'.
          05 FILLER                 PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP       PIC 9(8)  VALUE ZEROES.
          05 FILLER                 PIC X(1)  VALUE '>'.
          05 FILLER                 PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2      PIC 9(8)  VALUE ZEROES.
          05 FILLER                 PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE             PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                   VALUE 'Y'.
          88 NOT-DEBUGGING                    VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PSEUDO-CONVERSATIONAL PROGRAM DESIGN.

      *    START BY GETTING THE 'UPDATE' CONTAINER:
      *
      *    - IF IT DOES NOT YET EXIST -> 1ST STEP IN CONVERSATION
      *    - IF IT DOES ALREADY EXIST -> CONVERSATION IN PROGRESS

           EXEC CICS GET
                CONTAINER(APP-UPDATE-CONTAINER-NAME)
                CHANNEL(APP-UPDATE-CHANNEL-NAME)
                INTO (UPDATE-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
      *         1ST INTERACTION -> NO CONTAINER YET (CREATE IT)
      *         PERFORM 1000-FIRST-INTERACTION
                CONTINUE 
           WHEN DFHRESP(NORMAL)
      *         NEXT INTERACTIONS -> CONTAINER FOUND (CONTINUE)
      *         PERFORM 2000-PROCESS-USER-INPUT
                CONTINUE 
           WHEN OTHER
                MOVE 'Error Retrieving Update Container!' TO WS-MESSAGE
           END-EVALUATE.

           PERFORM 9000-SEND-MAP-AND-RETURN.

      *-----------------------------------------------------------------
      *START-UP SECTION.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-SEND-MAP-AND-RETURN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PSEUDO-CONVERSATIONAL RETURN:
      *      - PUT THE CONTAINER BACK TO CICS.
      *      - POPULATE AND SEND MAP TO CICS.
      *      - RETURN TO CICS.

      *    PERFORM 9100-POPULATE-MAP.
      *    PERFORM 9150-PUT-VIEW-CONTAINER.

           EXEC CICS SEND
                MAP(APP-UPDATE-MAP-NAME)
                MAPSET(APP-UPDATE-MAPSET-NAME)
                FROM (EUPDMO)
                ERASE
                FREEKB
                END-EXEC.

           EXEC CICS RETURN
                CHANNEL(APP-UPDATE-CHANNEL-NAME)
                TRANSID(APP-UPDATE-TRANSACTION-ID)
                END-EXEC.

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