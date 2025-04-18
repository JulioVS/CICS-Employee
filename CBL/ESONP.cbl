       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESONP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYE APP'
      *      - 'SIGN ON' PROGRAM
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR
      *      - APPLICATION CONSTANTS
      *      - MY SYMBOLIC MAP
      *      - IBM'S AID KEYS
      ******************************************************************
       COPY ECONST.
       COPY ESONMAP.
       COPY DFHAID.
      ******************************************************************
      *   DEFINE MY SESSION STATE DATA FOR PASSING INTO COMM-AREA.
      ******************************************************************
       01 WS-SESSION-STATE.
          05 WS-USER-ID        PIC X(8).
          05 WS-USER-PASSWORD  PIC X(8).
      ******************************************************************
      *   EXPLICITLY DEFINE THE COMM-AREA FOR THE TRASACTION.
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA          PIC X(16).

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

           IF EIBCALEN IS EQUAL TO ZERO
              PERFORM 1000-FIRST-INTERACTION
           ELSE
              PERFORM 2000-PROCESS-USER-INPUT
           END-IF.

      *-----------------------------------------------------------------
       SUB-ROUTINE SECTION.
      *-----------------------------------------------------------------

       1000-FIRST-INTERACTION.
      *    THIS IS THE START OF THE (PSEUDO) CONVERSATION,
      *    MEANING THE FIRST INTERACTION OF THE PROCESS,
      *    HENCE THE EMPTY COMM-AREA.-
           INITIALIZE WS-SESSION-STATE.
           INITIALIZE ESONMO.

      *    FOR THE FIRST INTERACTION, IT SENDS THE EMPY MAP WITH
      *    JUST THE TRANSACTION ID ON IT (AN ECHO OF A KNOWN VALUE)
           MOVE EIBTRNID TO TRANIDO.

           EXEC CICS SEND
                MAP(AC-SIGNON-MAP-NAME)
                MAPSET(AC-SIGNON-MAPSET-NAME)
                FROM (ESONMO)
                ERASE
                END-EXEC.

      *    THEN, IT RETURNS JUST SAVING THE STILL-EMPTY STATE
      *    AND ENDING THIS STEP OF THE CONVERSATION.
           EXEC CICS RETURN
                COMMAREA(WS-SESSION-STATE)
                TRANSID(EIBTRNID)
                END-EXEC.

       2000-PROCESS-USER-INPUT.
      *    THIS IS THE CONTINUATION OF THE CONVERSATION,
      *    MEANING THE SECOND INTERACTION OF THE PROCESS,
      *    HENCE THE COMM-AREA IS NOT EMPTY.

      *    RESTORE SESSION DATA INTO WORKING STORAGE
           MOVE DFHCOMMAREA TO WS-SESSION-STATE.

      *    GET NEW INPUT FROM THE USER
           EXEC CICS RECEIVE
                MAP(AC-SIGNON-MAP-NAME)
                MAPSET(AC-SIGNON-MAPSET-NAME)
                INTO (ESONMI)
                END-EXEC.

      *    THEN, SEND THE MAP BACK WITH A GREETING
           STRING "Hello " DELIMITED BY SIZE
                  USERIDI DELIMITED BY SPACE
                  "!" DELIMITED BY SIZE
              INTO MESSO
           END-STRING.

           EXEC CICS SEND
                MAP(AC-SIGNON-MAP-NAME)
                MAPSET(AC-SIGNON-MAPSET-NAME)
                FROM (ESONMO)
                ERASE
                END-EXEC.

           EXEC CICS RETURN
                END-EXEC.