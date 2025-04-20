       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESONP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYE APP'
      *      - 'SIGN ON' PROGRAM
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - SIGN-ON MAP.
      *      - REGISTERED USERS.
      *      - IBM'S AID KEYS.
      ******************************************************************
       COPY ECONST.
       COPY ESONMAP.
       COPY EREGUSR.
       COPY DFHAID.
      ******************************************************************
      *   DEFINE MY SESSION STATE DATA FOR PASSING INTO COMM-AREA.
      ******************************************************************
       01 WS-SESSION-STATE.
          05 WS-USER-ID        PIC X(8).
          05 WS-USER-PASSWORD  PIC X(8).
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE  PIC S9(8) USAGE IS BINARY.
          05 WS-CURRENT-DATE   PIC X(14).
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
           PERFORM 1100-INITIALIZE.
           PERFORM 9200-SEND-MAP-AND-RETURN.
       
       1100-INITIALIZE.
      *    INITIALIZE SESSION STATE AND MAP OUPUT FIELDS
           INITIALIZE WS-SESSION-STATE.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE ESONMO.

      *    FOR THE FIRST INTERACTION, IT SENDS THE EMPY MAP WITH
      *    JUST THE TRANSACTION ID ON IT (AN ECHO OF A KNOWN VALUE)
           MOVE EIBTRNID TO TRANIDO.

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

      *    AND CHECK PRESSED KEY
           EVALUATE EIBAID
           WHEN DFHPF3
           WHEN DFHPF12
                PERFORM 2100-CANCEL-SIGN-ON 
           WHEN DFHENTER
                PERFORM 3000-SIGN-ON-USER
           WHEN OTHER
                MOVE "Invalid key!" TO MESSO 
           END-EVALUATE.

           PERFORM 9200-SEND-MAP-AND-RETURN.

       2100-CANCEL-SIGN-ON.
      *    CLEAR USER SCREEN AND END CONVERSATION
           EXEC CICS SEND CONTROL
                ERASE 
                END-EXEC.

           EXEC CICS RETURN
                END-EXEC.

       3000-SIGN-ON-USER.
           PERFORM 3100-UPDATE-STATE.
           PERFORM 3200-CHECK-USER-STATUS.
           PERFORM 3300-LOOKUP-USER-ID.

       3100-UPDATE-STATE.
      *    IF NEW DATA WAS RECEIVED, UPDATE STATE
           IF USERIDI IS NOT EQUAL TO LOW-VALUES AND
              USERIDI IS NOT EQUAL TO SPACES
              MOVE USERIDI TO WS-USER-ID
           END-IF.
           IF PASSWDI IS NOT EQUAL TO LOW-VALUES AND
              PASSWDI IS NOT EQUAL TO SPACES
              MOVE PASSWDI TO WS-USER-PASSWORD
           END-IF.

       3200-CHECK-USER-STATUS.
           CONTINUE.

       3250-NOTIFY-ACTIVITY-MONITOR.
           CONTINUE.

       3300-LOOKUP-USER-ID.
      *    LOOKUP THE USER ID IN VSAM FILE
           EXEC CICS READ
                FILE(AC-REG-USER-FILE-NAME)
                INTO (REG-USER-RECORD)
                RIDFLD(WS-USER-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                PERFORM 3400-CHECK-USER-CREDENTIALS
           WHEN DFHRESP(NOTFND)
                MOVE "User not found!" TO MESSO
           WHEN OTHER
                MOVE "Error reading Users file!" TO MESSO
           END-EVALUATE.

       3400-CHECK-USER-CREDENTIALS.
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-DATE.

      *    CHECK IF THE USER ID AND PASSWORD MATCH
           IF WS-USER-PASSWORD IS EQUAL TO RU-USER-PASSWORD
      *       CHECK IF THE USER ID IS ACTIVE   
              IF RU-ST-ACTIVE 
      *          CHECK IF THE USER ID VALIDITY PERIOD HAS STARTED
                 IF WS-CURRENT-DATE
                    IS GREATER THAN OR EQUAL TO RU-LAST-EFFECTIVE-DATE
      *             ALL CONDITIONS MET, SUCCESFUL SIGN ON!              
                    PERFORM 3250-NOTIFY-ACTIVITY-MONITOR
                    PERFORM 9100-TRANSFER-TO-LANDING-PAGE
                 ELSE
                    MOVE "User is not yet active!" TO MESSO
                 END-IF 
              ELSE
                 MOVE "User is inactive!" TO MESSO
              END-IF
           ELSE
              MOVE "Invalid password!" TO MESSO
           END-IF.

       9100-TRANSFER-TO-LANDING-PAGE.
      *    TRANSFER TO THE LANDING PAGE
      *    - FOR NOW, WE JUST SEND A MESSAGE BACK
           MOVE "Successful sign on!" TO MESSO.
           PERFORM 9200-SEND-MAP-AND-RETURN.
 
       9200-SEND-MAP-AND-RETURN.
      *    PRESENT INITIAL SIGN-ON SCREEN TO THE USER
           EXEC CICS SEND
                MAP(AC-SIGNON-MAP-NAME)
                MAPSET(AC-SIGNON-MAPSET-NAME)
                FROM (ESONMO)
                ERASE
                END-EXEC.

      *    THEN IT RETURNS SAVING THE INITIAL STATE
      *    AND ENDING THIS STEP OF THE CONVERSATION
           EXEC CICS RETURN
                COMMAREA(WS-SESSION-STATE)
                TRANSID(EIBTRNID)
                END-EXEC.