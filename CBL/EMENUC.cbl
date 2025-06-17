       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMENUC.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'MENU C' (CURSOR POSITION VERSION) PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - MENU CONTAINER.
      *      - MENU MAPSET (CURSOR POSITION VERSION).
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EMNUCTR.
       COPY EMNCMAP.
       COPY EMONCTR.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE     PIC S9(8) USAGE IS BINARY.
          05 WS-MESSAGE           PIC X(79).
          05 WS-SELECT-FLAG       PIC X(1).
             88 WS-LIST-SELECTED            VALUE 'L'.
             88 WS-VIEW-SELECTED            VALUE 'V'.
      *
       01 WS-DEBUG-AID            PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER               PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT        PIC X(45) VALUE SPACES.
          05 FILLER               PIC X(1)  VALUE '>'.
          05 FILLER               PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP     PIC 9(8)  VALUE ZEROES.
          05 FILLER               PIC X(1)  VALUE '>'.
          05 FILLER               PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2    PIC 9(8)  VALUE ZEROES.
          05 FILLER               PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE           PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                 VALUE 'Y'.
          88 NOT-DEBUGGING                  VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<


           EXEC CICS GET
                CONTAINER(APP-MENU-CONTAINER-NAME)
                CHANNEL(APP-MENU-CHANNEL-NAME)
                INTO (MAIN-MENU-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
      *         1ST INTERACTION -> NO CONTAINER YET (CREATE IT)
                PERFORM 1000-FIRST-INTERACTION
           WHEN DFHRESP(NORMAL)
      *         NEXT INTERACTIONS -> CONTAINER FOUND (CONTINUE)
                PERFORM 2000-PROCESS-USER-INPUT
           WHEN OTHER
                MOVE 'Error Retrieving Menu Container!' TO WS-MESSAGE
           END-EVALUATE.

           PERFORM 9000-SEND-MAP-AND-RETURN.

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-FIRST-INTERACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1100-INITIALIZE.

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

           MOVE MON-USER-ID TO MNU-USER-ID.

       1100-INITIALIZE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE MAIN-MENU-CONTAINER.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE EMNUMO.

           MOVE 'Welcome to the Employee App!' TO WS-MESSAGE.

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS RECEIVE
                MAP(APP-MENU-MAP-NAME)
                MAPSET(APP-MENU-MAPSET-NAME)
                INTO (EMNUMI)
                END-EXEC.

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

           EVALUATE EIBAID
           WHEN DFHENTER
                PERFORM 2050-CHECK-SELECTION

                EVALUATE TRUE
                WHEN WS-LIST-SELECTED
                     PERFORM 2100-TRANSFER-TO-LIST-PAGE
                WHEN WS-VIEW-SELECTED
                     PERFORM 2200-TRANSFER-TO-VIEW-PAGE
                WHEN OTHER
                     MOVE 'Invalid Selection!' TO WS-MESSAGE
                END-EVALUATE
           WHEN DFHPF3
           WHEN DFHPF10
           WHEN DFHPF12
                PERFORM 2500-SIGN-USER-OFF
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
           END-EVALUATE.

       2050-CHECK-SELECTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2050-CHECK-SELECTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE SEL1F TO DFHBMFLG.
           IF DFHCURSR THEN
              SET WS-LIST-SELECTED TO TRUE
              EXIT PARAGRAPH
           END-IF.

           MOVE SEL2F TO DFHBMFLG.
           IF DFHCURSR THEN
              SET WS-VIEW-SELECTED TO TRUE
           END-IF.

       2100-TRANSFER-TO-LIST-PAGE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-TRANSFER-TO-LIST-PAGE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    RESET THIS CONVERSATION BY DELETING CURRENT CONTAINER.
           PERFORM 2300-DELETE-MENU-CONTAINER.

      *    TRANSFER LOGIC TO EMPLOYEES LISTING PAGE.
           EXEC CICS XCTL
                PROGRAM(APP-LIST-PROGRAM-NAME)
                CHANNEL(APP-LIST-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Transferring To Listing Page' TO WS-MESSAGE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request!' TO WS-MESSAGE
           WHEN DFHRESP(PGMIDERR)
                MOVE "Listing Page Program Not Found!" TO WS-MESSAGE
           WHEN OTHER
                MOVE "Error Linking To Listing Page!" TO WS-MESSAGE
           END-EVALUATE.

       2200-TRANSFER-TO-VIEW-PAGE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-TRANSFER-TO-VIEW-PAGE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    RESET THIS CONVERSATION BY DELETING CURRENT CONTAINER.
           PERFORM 2300-DELETE-MENU-CONTAINER.

      *    TRANSFER LOGIC TO VIEW EMPLOYEE DETAILS PAGE.
           EXEC CICS XCTL
                PROGRAM(APP-VIEW-PROGRAM-NAME)
                CHANNEL(APP-VIEW-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Transferring To Details Page' TO WS-MESSAGE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request!' TO WS-MESSAGE
           WHEN DFHRESP(PGMIDERR)
                MOVE 'Details Page Program Not Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Transferring To Details Page!' TO WS-MESSAGE
           END-EVALUATE.

       2300-DELETE-MENU-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2300-DELETE-MENU-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS DELETE
                CONTAINER(APP-MENU-CONTAINER-NAME)
                CHANNEL(APP-MENU-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
                MOVE 'Menu Container Not Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Deleting Menu Container!' TO WS-MESSAGE
           END-EVALUATE.

       2500-SIGN-USER-OFF.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2500-SIGN-USER-OFF' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    >>> CALL ACTIVITY MONITOR <<<
           SET MON-AC-SIGN-OFF TO TRUE.
           PERFORM 4200-CALL-ACTIVITY-MONITOR.
      *    >>> --------------------- <<<

           PERFORM 9200-RETURN-TO-CICS.

      *-----------------------------------------------------------------
       ACTIVITY-MONITOR SECTION.
      *-----------------------------------------------------------------

       4000-CHECK-USER-STATUS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4000-CHECK-USER-STATUS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CHECK IF THE USER IS ALREADY SIGNED ON TO THE ACTIVITY
           PERFORM 4100-GET-MONITOR-CONTAINER.

      *    IF THE USER IS SIGNED ON, CHECK IF SESSION IS STILL ACTIVE.
           SET MON-AC-APP-FUNCTION TO TRUE.
           PERFORM 4200-CALL-ACTIVITY-MONITOR.

       4100-GET-MONITOR-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4100-GET-MONITOR-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS GET
                CONTAINER(APP-ACTMON-CONTAINER-NAME)
                CHANNEL(APP-ACTMON-CHANNEL-NAME)
                INTO (ACTIVITY-MONITOR-CONTAINER)
                FLENGTH(LENGTH OF ACTIVITY-MONITOR-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
                MOVE 'No Activity Monitor Data Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Getting Activity Monitor!' TO WS-MESSAGE
           END-EVALUATE.

       4200-CALL-ACTIVITY-MONITOR.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4200-CALL-ACTIVITY-MONITOR' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PUT CONTAINER AND LINK TO ACTIVITY MONITOR PROGRAM
           MOVE APP-MENU-PROGRAM-NAME TO MON-LINKING-PROGRAM.
           INITIALIZE MON-RESPONSE.

           PERFORM 4300-PUT-MONITOR-CONTAINER.

           EXEC CICS LINK
                PROGRAM(APP-ACTMON-PROGRAM-NAME)
                CHANNEL(APP-ACTMON-CHANNEL-NAME)
                TRANSID(EIBTRNID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(PGMIDERR)
                MOVE 'Activity Monitor Program Not Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Linking to Activity Monitor!' TO WS-MESSAGE
           END-EVALUATE.

       4300-PUT-MONITOR-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4300-PUT-MONITOR-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS PUT
                CONTAINER(APP-ACTMON-CONTAINER-NAME)
                CHANNEL(APP-ACTMON-CHANNEL-NAME)
                FROM (ACTIVITY-MONITOR-CONTAINER)
                FLENGTH(LENGTH OF ACTIVITY-MONITOR-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Error Putting Activity Monitor!' TO WS-MESSAGE
           END-EVALUATE.

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-SEND-MAP-AND-RETURN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 9100-POPULATE-MAP.
           PERFORM 9150-PUT-MENU-CONTAINER.

           EXEC CICS SEND
                MAP(APP-MENU-MAP-NAME)
                MAPSET(APP-MENU-MAPSET-NAME)
                FROM (EMNUMO)
                ERASE
                END-EXEC.

           EXEC CICS RETURN
                CHANNEL(APP-MENU-CHANNEL-NAME)
                TRANSID(APP-MENU-TRANSACTION-ID)
                END-EXEC.

       9100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-POPULATE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE EMNUMO.

           MOVE EIBTRNID TO TRANIDO.

           IF MNU-USER-ID IS NOT EQUAL TO SPACES THEN
              MOVE MNU-USER-ID TO LOGDINO
           ELSE
              MOVE '<Anonym>' TO LOGDINO
           END-IF.

           MOVE WS-MESSAGE TO MESSO.

           EVALUATE TRUE
           WHEN WS-MESSAGE(1:7) IS EQUAL TO 'Welcome'
                MOVE DFHPINK TO MESSC
           WHEN WS-MESSAGE(1:7) IS EQUAL TO 'Invalid'
                MOVE DFHYELLO TO MESSC
           WHEN WS-MESSAGE(1:5) IS EQUAL TO 'Error'
                MOVE DFHRED TO MESSC
           END-EVALUATE.

      *    SET ANY MODIFIED DATA TAG (MDT) 'ON' TO AVOID THE 'AEI9'
      *    ABEND THAT HAPPENS WHEN WE ONLY RECEIVE AN AID-KEY FROM THE
      *    MAP AND NO REAL DATA ALONG IT.
           MOVE DFHBMFSE TO TRANIDA.

       9150-PUT-MENU-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9150-PUT-LIST-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS PUT
                CONTAINER(APP-MENU-CONTAINER-NAME)
                CHANNEL(APP-MENU-CHANNEL-NAME)
                FROM (MAIN-MENU-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Error Putting Menu Container!' TO WS-MESSAGE
           END-EVALUATE.

       9200-RETURN-TO-CICS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9200-RETURN-TO-CICS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS SEND CONTROL
                ERASE
                FREEKB
                END-EXEC.

           EXEC CICS RETURN
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
