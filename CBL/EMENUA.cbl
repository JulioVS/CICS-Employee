       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMENUA.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'MENU A' (AID-KEY VERSION) PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - MENU MAPSET.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EMNUMAP.
       COPY EMONCTR.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE    PIC S9(8) USAGE IS BINARY.
          05 WS-MESSAGE          PIC X(79) VALUE SPACES.
       03 WS-MENU-ACTIONS        PIC X(1)  VALUE SPACES.
          88 WS-ACTION-LIST                VALUE 'L'.
          88 WS-ACTION-VIEW                VALUE 'V'.
          88 WS-ACTION-EXIT                VALUE 'E'.
          88 WS-ACTION-SIGN-OFF            VALUE 'S'.
          88 WS-ACTION-INVALID             VALUE 'I'.
      *
       01 WS-DEBUG-AID           PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER              PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT       PIC X(45) VALUE SPACES.
          05 FILLER              PIC X(1)  VALUE '>'.
          05 FILLER              PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP    PIC 9(8)  VALUE ZEROES.
          05 FILLER              PIC X(1)  VALUE '>'.
          05 FILLER              PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2   PIC 9(8)  VALUE ZEROES.
          05 FILLER              PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE          PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                VALUE 'Y'.
          88 NOT-DEBUGGING                 VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1000-FIRST-INTERACTION.

           PERFORM 2000-DISPLAY-MENU-SCREEN
              UNTIL WS-ACTION-EXIT OR WS-ACTION-SIGN-OFF.

           PERFORM 9200-RETURN-TO-CICS.
           
      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-FIRST-INTERACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1100-INITIALIZE-VARIABLES.
           MOVE 'Hey, Welcome to the Employee App!' TO WS-MESSAGE.

       1100-INITIALIZE-VARIABLES.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE-VARIABLES' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE EMNUMO.

      *-----------------------------------------------------------------
       MENU SECTION.
      *-----------------------------------------------------------------

       2000-DISPLAY-MENU-SCREEN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-DISPLAY-MENU-SCREEN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE EMNUMO.           

           MOVE EIBTRNID TO TRANIDO.
           MOVE WS-MESSAGE TO MESSO.

           EVALUATE TRUE
           WHEN WS-MESSAGE(1:7) IS EQUAL TO 'Invalid'
                MOVE DFHYELLO TO MESSC
           WHEN WS-MESSAGE(1:5) IS EQUAL TO 'Error'
                MOVE DFHRED TO MESSC
           WHEN WS-MESSAGE(1:3) IS EQUAL TO 'Hey'
                MOVE DFHPINK TO MESSC
           END-EVALUATE.

           EXEC CICS SEND
                MAP(APP-MENU-MAP-NAME)
                MAPSET(APP-MENU-MAPSET-NAME)
                FROM (EMNUMO)
                ERASE
                END-EXEC.

           EXEC CICS RECEIVE
                LENGTH(LENGTH OF EIBAID)
                END-EXEC.

           EVALUATE EIBAID
           WHEN DFHPF1
                MOVE 'List Employees Request' TO WS-MESSAGE
                SET WS-ACTION-LIST TO TRUE
                PERFORM 2100-TRANSFER-TO-LIST-PAGE
           WHEN DFHPF2
                MOVE 'View Employee Request' TO WS-MESSAGE
                SET WS-ACTION-VIEW TO TRUE
                PERFORM 2200-TRANSFER-TO-VIEW-PAGE
           WHEN DFHPF3
                MOVE 'Menu Exit Request' TO WS-MESSAGE
                SET WS-ACTION-EXIT TO TRUE
           WHEN DFHPF10
                MOVE 'Sign Off Request' TO WS-MESSAGE
                SET WS-ACTION-SIGN-OFF TO TRUE
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
                SET WS-ACTION-INVALID TO TRUE
           END-EVALUATE.

       2100-TRANSFER-TO-LIST-PAGE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-TRANSFER-TO-LIST-PAGE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS XCTL
                PROGRAM(APP-LIST-PROGRAM-NAME)
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

           EXEC CICS XCTL
                PROGRAM(APP-VIEW-PROGRAM-NAME)
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

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

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