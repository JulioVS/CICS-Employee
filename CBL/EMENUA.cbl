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
          05 WS-CICS-RESPONSE   PIC S9(8) USAGE IS BINARY.
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-MESSAGE         PIC X(79) VALUE SPACES.
      *
       01 WS-DEBUG-AID          PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER             PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT      PIC X(45) VALUE SPACES.
          05 FILLER             PIC X(1)  VALUE '>'.
          05 FILLER             PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP   PIC 9(8)  VALUE ZEROES.
          05 FILLER             PIC X(1)  VALUE '>'.
          05 FILLER             PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2  PIC 9(8)  VALUE ZEROES.
          05 FILLER             PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE         PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING               VALUE 'Y'.
          88 NOT-DEBUGGING                VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1000-FIRST-INTERACTION.
           MOVE 'HEY THERE!' TO WS-MESSAGE.
           PERFORM 9100-POPULATE-MAP.

      *    PRESENT INITIAL MENU SCREEN TO THE USER.
           EXEC CICS SEND
                MAP(APP-MENU-MAP-NAME)
                MAPSET(APP-MENU-MAPSET-NAME)
                FROM (EMNUMO)
                ERASE
                END-EXEC.

           EXEC CICS RECEIVE
                LENGTH(LENGTH OF EIBAID)
                END-EXEC.

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
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-POPULATE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE EMNUMO.           

           MOVE EIBTRNID TO TRANIDO.
           MOVE WS-MESSAGE TO MESSO.

      *    MOVE DFHBMFSE TO TRANIDA.

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