       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESONP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'SIGN ON' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - SIGN-ON MAPSET.
      *      - REGISTERED USERS.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY ESONMAP.
       COPY EREGUSR.
       COPY EMONCTR.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY SESSION STATE DATA FOR PASSING INTO COMM-AREA.
      ******************************************************************
       01 WS-SESSION-STATE.
          05 WS-USER-ID           PIC X(8).
          05 WS-USER-PASSWORD     PIC X(8).
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE     PIC S9(8) USAGE IS BINARY.
          05 WS-CURRENT-DATE      PIC X(14).
      *
          05 WS-USER-LOOKUP       PIC X(1)  VALUE SPACES.
             88 WS-USER-FOUND               VALUE 'Y'.
          05 WS-LOGIN-OUTCOME     PIC X(1)  VALUE SPACES.
             88 WS-LOGIN-SUCCESS            VALUE 'Y'.
      *
       01 WS-DEBUG-AID            PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER               PIC X(5)  VALUE '<DBG:'.
          05 WS-DEBUG-TEXT        PIC X(30) VALUE SPACES.
          05 FILLER               PIC X(1)  VALUE '>'.
          05 FILLER               PIC X(5)  VALUE '<MSG:'.
          05 WS-MESSAGE           PIC X(9)  VALUE SPACES.
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

      ******************************************************************
      *   EXPLICITLY DEFINE THE COMM-AREA FOR THE TRASACTION.
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA             PIC X(16).

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF EIBCALEN IS EQUAL TO ZERO THEN
              PERFORM 1000-FIRST-INTERACTION
           ELSE
              PERFORM 2000-PROCESS-USER-INPUT
           END-IF.

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-FIRST-INTERACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    THIS IS THE START OF THE (PSEUDO) CONVERSATION,
      *    MEANING THE FIRST INTERACTION OF THE PROCESS,
      *    HENCE THE EMPTY COMM-AREA.-
           PERFORM 1100-INITIALIZE.
           PERFORM 9100-SEND-MAP-AND-RETURN.

       1100-INITIALIZE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    INITIALIZE SESSION STATE, MAP OUPUT, WORK VARS AND CONTAINER
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE REGISTERED-USER-RECORD.
           INITIALIZE WS-SESSION-STATE.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE ESONMO.

      *    FOR THE FIRST INTERACTION, IT SENDS THE EMPY MAP WITH
      *    JUST THE TRANSACTION ID ON IT (AN ECHO OF A KNOWN VALUE)
           MOVE EIBTRNID TO TRANIDO.

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    THIS IS THE CONTINUATION OF THE CONVERSATION,
      *    MEANING THE SECOND INTERACTION OF THE PROCESS,
      *    HENCE THE COMM-AREA IS NOT EMPTY.

      *    RESTORE SESSION DATA INTO WORKING STORAGE
           MOVE DFHCOMMAREA TO WS-SESSION-STATE.

      *    GET NEW INPUT FROM THE USER
           EXEC CICS RECEIVE
                MAP(APP-SIGNON-MAP-NAME)
                MAPSET(APP-SIGNON-MAPSET-NAME)
                INTO (ESONMI)
                END-EXEC.

      *    AND CHECK PRESSED KEY
           EVALUATE EIBAID
           WHEN DFHPF3
           WHEN DFHPF12
                PERFORM 2100-CANCEL-SIGN-ON
           WHEN DFHENTER
                IF USERIDI IS EQUAL TO LOW-VALUES OR
                   USERIDI IS EQUAL TO SPACES OR
                   PASSWDI IS EQUAL TO LOW-VALUES OR
                   PASSWDI IS EQUAL TO SPACES THEN
                   MOVE "Invalid Credentials!" TO MESSO
                ELSE
                   PERFORM 3000-SIGN-ON-USER
                END-IF
           WHEN OTHER
                MOVE "Invalid Key!" TO MESSO
           END-EVALUATE.

           PERFORM 9100-SEND-MAP-AND-RETURN.

       2100-CANCEL-SIGN-ON.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-CANCEL-SIGN-ON' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR USER SCREEN AND END CONVERSATION
           EXEC CICS SEND CONTROL
                ERASE
                END-EXEC.

           EXEC CICS RETURN
                END-EXEC.

      *-----------------------------------------------------------------
       SIGN-ON SECTION.
      *-----------------------------------------------------------------

       3000-SIGN-ON-USER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3000-SIGN-ON-USER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 3100-UPDATE-STATE.
           PERFORM 3200-LOOKUP-USER-ID.

           IF WS-USER-FOUND THEN
              PERFORM 3300-CHECK-USER-STATUS
              PERFORM 3400-CHECK-USER-CREDENTIALS
           ELSE
              EXIT
           END-IF.

           IF WS-LOGIN-SUCCESS THEN
              PERFORM 3500-NOTIFY-ACTIVITY-MONITOR
              PERFORM 9000-TRANSFER-TO-LANDING-PAGE
           END-IF.

       3100-UPDATE-STATE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3100-UPDATE-STATE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF NEW DATA WAS RECEIVED, UPDATE STATE
           IF USERIDI IS NOT EQUAL TO LOW-VALUES AND
              USERIDI IS NOT EQUAL TO SPACES THEN
              MOVE FUNCTION TRIM(USERIDI) TO WS-USER-ID
           END-IF.
           IF PASSWDI IS NOT EQUAL TO LOW-VALUES AND
              PASSWDI IS NOT EQUAL TO SPACES THEN
              MOVE FUNCTION TRIM(PASSWDI) TO WS-USER-PASSWORD
           END-IF.

       3200-LOOKUP-USER-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3200-LOOKUP-USER-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    LOOKUP THE USER ID IN VSAM FILE
           EXEC CICS READ
                FILE(APP-REG-USER-FILE-NAME)
                INTO (REGISTERED-USER-RECORD)
                RIDFLD(WS-USER-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                SET WS-USER-FOUND TO TRUE
                MOVE "User Found!" TO MESSO
           WHEN DFHRESP(NOTFND)
                MOVE "User Not Found!" TO MESSO
           WHEN OTHER
                MOVE "Error Reading Users File!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           END-EVALUATE.

       3300-CHECK-USER-STATUS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3300-CHECK-USER-STATUS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CALL ACTIVITY MONITOR PROGRAM WITH "SIGN-ON" ACTION.
           SET MON-AC-SIGN-ON TO TRUE.
           PERFORM 3310-CALL-ACTIVITY-MONITOR.
           PERFORM 3320-EVALUATE-RESPONSE.

       3310-CALL-ACTIVITY-MONITOR.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3310-CALL-ACTIVITY-MONITOR' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PUT CONTAINER AND LINK TO ACTIVITY MONITOR PROGRAM
           MOVE APP-SIGNON-PROGRAM-NAME TO MON-LINKING-PROGRAM.
           MOVE WS-USER-ID TO MON-USER-ID.
           MOVE REG-USER-CATEGORY TO MON-USER-CATEGORY.
           INITIALIZE MON-RESPONSE.

           PERFORM 3315-PUT-CONTAINER.

      *    'LINK' CALLS THE PROGRAM AND *RETURNS* AFTER ITS EXECUTION.
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
                MOVE "Activity Monitor Program Not Found!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE "Error Linking To Activity Monitor!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           END-EVALUATE.

       3315-PUT-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3315-PUT-CONTAINER' TO WS-DEBUG-AID.
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
                MOVE "Error Writing Activity Monitor!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           END-EVALUATE.

       3320-EVALUATE-RESPONSE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3320-EVALUATE-RESPONSE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    GET THE RESPONSE FROM THE ACTIVITY MONITOR PROGRAM
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
                MOVE "No Activity Monitor Data Found!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE "Error Reading Activity Monitor!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *    RELAY ACTIVITY MONITOR RESPONSE MESSAGE TO USER TERMINAL
           MOVE MON-MESSAGE TO MESSO.

      *    SEE IF IT RESULTED IN SUCCESS, FAIL, NEUTRAL OR ERROR.
           EVALUATE TRUE
           WHEN MON-PROCESSING-ERROR
           WHEN MON-ST-LOCKED-OUT
      *         ON LOCKOUT OR ERROR, SEND BACK TO THE START
                PERFORM 9100-SEND-MAP-AND-RETURN
           WHEN MON-ST-SIGNED-ON
      *         ON SUCCESSFUL SIGN-ON, SEND TO INITIAL APP SCREEN
                PERFORM 9000-TRANSFER-TO-LANDING-PAGE
           WHEN MON-ST-IN-PROCESS
           WHEN MON-ST-NOT-SET
      *         ON NEUTRAL, CONTINUE TO CHECK USER CREDENTIALS
                CONTINUE
           WHEN OTHER
                MOVE "Unknown Response From Activity Monitor!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           END-EVALUATE.

       3400-CHECK-USER-CREDENTIALS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3400-CHECK-USER-CREDENTIALS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-DATE.

      *    CHECK IF THE USER ID AND PASSWORD MATCH.
           IF WS-USER-PASSWORD IS EQUAL TO REG-USER-PASSWORD THEN
      *       CHECK IF THE USER ID IS ACTIVE.
              IF REG-ST-ACTIVE THEN
      *          CHECK IF THE USER ID VALIDITY PERIOD HAS STARTED.
                 IF WS-CURRENT-DATE >= REG-LAST-EFFECTIVE-DATE THEN
      *             ALL CONDITIONS MET
      *             SUCCESFUL SIGN ON!
                    SET WS-LOGIN-SUCCESS TO TRUE
                    MOVE "User Is Active!" TO MESSO
                 ELSE
                    MOVE "User Is Not Yet Active!" TO MESSO
                 END-IF
              ELSE
                 MOVE "User Is Inactive!" TO MESSO
              END-IF
           ELSE
              MOVE "Invalid Password!" TO MESSO
           END-IF.

       3500-NOTIFY-ACTIVITY-MONITOR.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3500-NOTIFY-ACTIVITY-MONITOR' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    NOTIFY ACTIVITY MONITOR OF A SUCCESSFUL SIGN-ON!
      *    (ONE-WAY OPERATION, NO RESPONSE EXPECTED)
           SET MON-AC-NOTIFY TO TRUE.
           PERFORM 3310-CALL-ACTIVITY-MONITOR.

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-TRANSFER-TO-LANDING-PAGE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-TRANSFER-TO-LANDING-PAGE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PUT CONTAINER AND TRANSFER CONTROL TO INITIAL PROGRAM
      *    OF THE EMPLOYEE APP!
           PERFORM 3315-PUT-CONTAINER.

      *    'XCTL' CALLS THE PROGRAM BUT DOES *NOT* RETURN AFTERWARDS!
           EXEC CICS XCTL
                PROGRAM(APP-LANDING-PROGRAM-NAME)
                CHANNEL(APP-ACTMON-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Transferring To Landing Page' TO MESSO
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request!' TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           WHEN DFHRESP(PGMIDERR)
                MOVE "Landing Page Program Not Found!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE "Error Linking To Landing Page!" TO MESSO
                PERFORM 9100-SEND-MAP-AND-RETURN
           END-EVALUATE.

       9100-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-SEND-MAP-AND-RETURN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CHANGE COLOR OF MESSAGE BASED ON TYPE/CONTENT.
           MOVE DFHTURQ TO MESSC.

           EVALUATE TRUE
           WHEN MESSO(1:5) IS EQUAL TO 'Error'
           WHEN MESSO(1:7) IS EQUAL TO 'Invalid'
           WHEN MESSO(6:3) IS EQUAL TO 'Not'
           WHEN MESSO(9:3) IS EQUAL TO 'Not'
           WHEN MESSO(9:8) IS EQUAL TO 'Inactive'
                MOVE DFHRED TO MESSC
           WHEN MESSO(1:7) IS EQUAL TO 'Unknown'
                MOVE DFHYELLO TO MESSC
           END-EVALUATE

      *    PRESENT INITIAL SIGN-ON SCREEN TO THE USER.
           EXEC CICS SEND
                MAP(APP-SIGNON-MAP-NAME)
                MAPSET(APP-SIGNON-MAPSET-NAME)
                FROM (ESONMO)
                ERASE
                END-EXEC.

      *    THEN IT RETURNS SAVING THE INITIAL STATE
      *    AND ENDING THIS STEP OF THE CONVERSATION.
           EXEC CICS RETURN
                COMMAREA(WS-SESSION-STATE)
                TRANSID(APP-SIGNON-TRANSACTION-ID)
                END-EXEC.

       9300-DEBUG-AID.
      *    >>> DEBUGGING ONLY <<<
           IF I-AM-DEBUGGING THEN
              INITIALIZE WS-DEBUG-MESSAGE

              MOVE WS-DEBUG-AID TO WS-DEBUG-TEXT
              MOVE MESSO TO WS-MESSAGE
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