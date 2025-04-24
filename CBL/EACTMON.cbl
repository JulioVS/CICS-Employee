       IDENTIFICATION DIVISION.
       PROGRAM-ID. EACTMON.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'
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
      *
          03 WS-CURRENT-TIMESTAMP.
             05 WS-CT-DATE.
                07 WS-CT-YEAR          PIC 9(4).
                07 WS-CT-MONTH         PIC 9(2).
                07 WS-CT-DAY           PIC 9(2).
             05 WS-CT-TIME.
                07 WS-CT-HOUR          PIC 9(2).
                07 WS-CT-MINUTE        PIC 9(2).
                07 WS-CT-SECOND        PIC 9(2).
      *
          03 WS-LOCKOUT-TIMESTAMP.
             05 WS-LK-DATE.
                07 WS-LK-YEAR          PIC 9(4).
                07 WS-LK-MONTH         PIC 9(2).
                07 WS-LK-DAY           PIC 9(2).
             05 WS-LK-TIME.
                07 WS-LK-HOUR          PIC 9(2).
                07 WS-LK-MINUTE        PIC 9(2).
                07 WS-LK-SECOND        PIC 9(2).
      *
          03 WS-ACTIVITY-TIMESTAMP.
             05 WS-AC-DATE.
                07 WS-AC-YEAR          PIC 9(4).
                07 WS-AC-MONTH         PIC 9(2).
                07 WS-AC-DAY           PIC 9(2).
             05 WS-AC-TIME.
                07 WS-AC-HOUR          PIC 9(2).
                07 WS-AC-MINUTE        PIC 9(2).
                07 WS-AC-SECOND        PIC 9(2).
      *
          03 WS-ELAPSED-MINUTES        PIC S9(4).

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

           PERFORM 1000-INITIAL-SETUP.
           PERFORM 2000-PROCESS-REQUEST.
           PERFORM 9000-RETURN-TO-CALLER.

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-INITIAL-SETUP.
           INITIALIZE WS-WORKING-VARS.
          
           PERFORM 1100-GET-DATA-FROM-CALLER.
           PERFORM 1200-GET-SIGN-ON-RULES.
           PERFORM 1300-GET-USER-ACTIVITY-QUEUE.

       1100-GET-DATA-FROM-CALLER.
      *    GET CONTAINER SENT FROM THE CALLING PROGRAM.
           EXEC CICS GET
                CONTAINER(AC-ACTMON-CONTAINER-NAME)
                CHANNEL(AC-ACTMON-CHANNEL-NAME)
                INTO (ACTIVITY-MONITOR-CONTAINER)
                FLENGTH(LENGTH OF ACTIVITY-MONITOR-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE 
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
                MOVE 'Activity Mon Container Not Found!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           WHEN OTHER
                MOVE 'Activity Mon Container Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.

           INITIALIZE MON-RESPONSE.

       1200-GET-SIGN-ON-RULES.
      *    GET SIGN-ON RULES FROM TEMPORARY QUEUE, IF AVAILABLE.
      *    IF NOT, GET THEM FROM THE VSAM FILE.
           MOVE AC-SIGNON-RULES-ITEM-NUM TO WS-ITEM-NUMBER.
           
      *    FOR 16-BYTE QUEUE NAMES, USE THE 'QNAME()' INNER OPTION AND
      *    NOT 'QUEUE()' WHICH ONLY TAKES 8-BYTES!
           EXEC CICS READQ TS
                QNAME(AC-SIGNON-RULES-QUEUE-NAME)
                ITEM(WS-ITEM-NUMBER)
                INTO (SIGN-ON-RULES-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(QIDERR)
                PERFORM 1210-LOAD-RULES-FROM-FILE
           WHEN OTHER
                MOVE 'Sign-On Rules READQ Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.

       1210-LOAD-RULES-FROM-FILE.
      *    LOAD SIGN-ON RULES FROM VSAM [RRDS] FILE.
      *      - JUST A SINGLE RECORD IN RELATIVE RECORD NUMBER 1.
           EXEC CICS READ
                FILE(AC-SIGNON-RULES-FILE-NAME)
                INTO (SIGN-ON-RULES-RECORD)
                RIDFLD(AC-SIGNON-RULES-RRN)
                RRN
                RESP(WS-CICS-RESPONSE)
                END-EXEC.
                
           EVALUATE WS-CICS-RESPONSE 
           WHEN DFHRESP(NORMAL)
                PERFORM 1220-WRITE-RULES-TO-QUEUE
           WHEN OTHER
                MOVE 'Sign-On Rules File Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.
         
       1220-WRITE-RULES-TO-QUEUE.
      *    SET UP SIGN-ON RULES QUEUE TO PROVIDE IN-MEMORY ACCESS.
           MOVE AC-SIGNON-RULES-ITEM-NUM TO WS-ITEM-NUMBER.

           EXEC CICS WRITEQ TS
                QNAME(AC-SIGNON-RULES-QUEUE-NAME)
                ITEM(WS-ITEM-NUMBER)
                FROM (SIGN-ON-RULES-RECORD)
                MAIN
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Sign-On Rules WRITEQ Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.                     
          
       1300-GET-USER-ACTIVITY-QUEUE.
      *    ACTIVITY QUEUE NAME HAS A FIXED PREFIX AND A VARIABLE
      *    'USER ID' SUFFIX.
           MOVE AC-ACTMON-QUEUE-PREFIX TO WS-UA-QNAME-PREFIX.
           MOVE MON-USER-ID TO WS-UA-QNAME-USERID.

      *    LIKE THE RULES QUEUE, IT FEATURES JUST A SINGLE ITEM.
           MOVE AC-ACTMON-ITEM-NUM TO WS-ITEM-NUMBER.

           EXEC CICS READQ TS
                QNAME(WS-USER-ACTIVITY-QUEUE-NAME)
                ITEM(WS-ITEM-NUMBER)
                INTO (USER-ACTIVITY-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(QIDERR)
                PERFORM 1310-NO-USER-ACTIVITY-QUEUE
           WHEN OTHER
                MOVE 'User Activity READQ Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.   
           
       1310-NO-USER-ACTIVITY-QUEUE.
           IF MON-LINKING-PROGRAM IS EQUAL TO AC-SIGNON-PROGRAM-NAME
      *       VALID SCENARIO - FIRST INTERACTION SINCE APP STARTUP
      *                        OR PREVIOUS SIGN-OFF.
              PERFORM 1320-INIT-USER-ACTIVITY-QUEUE

      *       AS A FIRST INTERACTION, JUST SET STATUS TO IN-PROCESS,
      *       UPDATE CONTAINER AND RETURN TO CALLER.
              SET MON-ST-IN-PROCESS TO TRUE
              MOVE 'Sign-On In Process' TO MON-MESSAGE
              PERFORM 9000-RETURN-TO-CALLER
           ELSE
      *       INVALID SCENARIO - REPORT AND LEAVE.
              MOVE 'No User Activity Queue Exception!' TO MON-MESSAGE 
              SET MON-PROCESSING-ERROR TO TRUE
              PERFORM 9000-RETURN-TO-CALLER
           END-IF.

       1320-INIT-USER-ACTIVITY-QUEUE.
           INITIALIZE USER-ACTIVITY-RECORD.

      *    SET THE USER ACTIVITY QUEUE TO INITIAL VALUES.
           MOVE MON-USER-ID TO ACT-USER-ID.
           SET ACT-CT-NOT-SET TO TRUE.
           SET ACT-ST-IN-PROCESS TO TRUE.
           MOVE 1 TO ACT-ATTEMPT-NUMBER.
           MOVE FUNCTION CURRENT-DATE(1:14) TO
              ACT-LAST-ACTIVITY-TIMESTAMP. 

           MOVE AC-ACTMON-ITEM-NUM TO WS-ITEM-NUMBER.
           
      *    NO ACTUAL 'CREATE QUEUE' COMMAND - CICS CREATES IT 
      *    AUTOMATICALLY ON FIRST WRITE!   
           EXEC CICS WRITEQ TS
                QNAME(WS-USER-ACTIVITY-QUEUE-NAME)
                ITEM(WS-ITEM-NUMBER)
                FROM (USER-ACTIVITY-RECORD)
                MAIN
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE 
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'User Activity WRITEQ Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-REQUEST.
           EVALUATE TRUE
           WHEN MON-AC-SIGN-OFF
      *         NOTIFICATION OF USER SIGN-OFF - DELETE QUEUE.
                PERFORM 2100-SIGN-USER-OFF
           WHEN MON-AC-NOTIFY 
      *         NOTIFICATION OF SUCCESSFUL SIGN-ON - UPDATE STATUS.
                PERFORM 2200-SET-SIGNED-ON-STATUS
           WHEN MON-AC-SIGN-ON
      *         NOTIFICATION OF SIGN-ON ATTEMPT - CHECKS HIS STATUS.
                PERFORM 2300-SIGN-ON-USER
           WHEN MON-AC-APP-FUNCTION
      *         NOT YET IMPLEMENTED - STALL.
                CONTINUE 
           WHEN MON-AC-NOT-SET 
      *         NO SPECIFIED ACTION, NOTHING TO DO.
                MOVE 'No Action Was Requested!' TO MON-MESSAGE
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.

       2100-SIGN-USER-OFF.
      *    DELETE USER ACTIVITY QUEUE.
           EXEC CICS DELETEQ TS
                QNAME(WS-USER-ACTIVITY-QUEUE-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(QIDERR)
                MOVE 'User Activity Queue Missing!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           WHEN OTHER
                MOVE 'User Activity DELETEQ Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.

           PERFORM 9100-RETURN-TO-CICS.
          
       2200-SET-SIGNED-ON-STATUS.
      *    UPDATE USER ACTIVITY QUEUE WITH SIGN-ON STATUS.
           SET ACT-ST-SIGNED-ON TO TRUE.
           SET MON-ST-SIGNED-ON TO TRUE.

           INITIALIZE ACT-ATTEMPT-NUMBER.

           PERFORM 2250-UPDATE-USER-ACT-QUEUE.
           PERFORM 9000-RETURN-TO-CALLER.

       2250-UPDATE-USER-ACT-QUEUE.
      *    UPDATE USER ACTIVITY QUEUE.
           MOVE FUNCTION CURRENT-DATE(1:14) TO
              ACT-LAST-ACTIVITY-TIMESTAMP.

           MOVE AC-ACTMON-ITEM-NUM TO WS-ITEM-NUMBER.

      *    WE NEED TO INCLUDE THE 'REWRITE' OPTION TO UPDATE THE QUEUE.
           EXEC CICS WRITEQ TS
                QNAME(WS-USER-ACTIVITY-QUEUE-NAME)
                ITEM(WS-ITEM-NUMBER)
                FROM (USER-ACTIVITY-RECORD)
                REWRITE
                MAIN
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE 
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'User Activity WRITEQ Exception!' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.
           
       2300-SIGN-ON-USER.
           EVALUATE TRUE
           WHEN ACT-ST-LOCKED-OUT
                PERFORM 3000-LOCKED-OUT-CASE
           WHEN ACT-ST-SIGNED-ON
                PERFORM 4000-SIGNED-ON-CASE
           WHEN ACT-ST-IN-PROCESS
                PERFORM 5000-IN-PROCESS-CASE
           WHEN ACT-ST-NOT-SET
                IF MON-LINKING-PROGRAM EQUAL AC-SIGNON-PROGRAM-NAME
                   SET ACT-ST-IN-PROCESS TO TRUE
                   PERFORM 5000-IN-PROCESS-CASE
                END-IF
           WHEN OTHER
                MOVE 'Invalid User Sign-On Status!' TO MON-MESSAGE
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.
           
       3000-LOCKED-OUT-CASE.
      *    CHECK IF LOCKOUT TIME HAS EXPIRED.
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-TIMESTAMP.
           MOVE ACT-LAST-ACTIVITY-TIMESTAMP TO WS-LOCKOUT-TIMESTAMP.

      *    CALCULATE ELAPSED TIME IN MINUTES SINCE LOCK-OUT.
      *    (COULD BE NEGATIVE IF THE DATE HAS CHANGED)
           COMPUTE WS-ELAPSED-MINUTES =
              (WS-CT-HOUR * 60 + WS-CT-MINUTE) -
              (WS-LK-HOUR * 60 + WS-LK-MINUTE)
           END-COMPUTE.
           
      *    IF THE DATE HAS CHANGED ADD A DAY'S WORTH OF MINUTES
      *    (1440 MINUTES) TO FIX A CASE LIKE LOCKOUT TIME OF 23:59
      *    AND CURRENT TIME OF 00:01.
           IF WS-CT-DATE > WS-LK-DATE
              ADD 1440 TO WS-ELAPSED-MINUTES
           END-IF.

      *    IF ENOUGH TIME HAS PASSED, UNLOCK THE USER AND UPDATE QUEUE.
           IF WS-ELAPSED-MINUTES > SR-LOCKOUT-INTERVAL THEN
              SET ACT-ST-SIGNED-ON TO TRUE
              SET MON-ST-SIGNED-ON TO TRUE
              INITIALIZE ACT-ATTEMPT-NUMBER

              MOVE 'Sign-On Lockout Expired' TO MON-MESSAGE
              PERFORM 2250-UPDATE-USER-ACT-QUEUE
              PERFORM 9200-REDIRECT-TO-SIGNON
           ELSE
              SET MON-ST-LOCKED-OUT TO TRUE
              MOVE 'Sign-On Lockout Still In Effect!' TO MON-MESSAGE
              PERFORM 9000-RETURN-TO-CALLER
           END-IF.

       4000-SIGNED-ON-CASE.
      *    CHECK IF THE USER SESSION HAS TIMED OUT.
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-TIMESTAMP.
           MOVE ACT-LAST-ACTIVITY-TIMESTAMP TO WS-ACTIVITY-TIMESTAMP.     

      *    CALCULATE ELAPSED TIME IN MINUTES SINCE LAST ACTIVITY.
      *    (COULD BE NEGATIVE IF THE DATE HAS CHANGED)
           COMPUTE WS-ELAPSED-MINUTES =
              (WS-CT-HOUR * 60 + WS-CT-MINUTE) -
              (WS-AC-HOUR * 60 + WS-AC-MINUTE)
           END-COMPUTE.

      *    IF THE DATE HAS CHANGED ADD A DAY'S WORTH OF MINUTES
      *    (1440 MINUTES) TO FIX A CASE LIKE LAST ACTIVITY TIME
      *    OF 23:59 AND CURRENT TIME OF 00:01.
           IF WS-CT-DATE > WS-AC-DATE
              ADD 1440 TO WS-ELAPSED-MINUTES
           END-IF.

           INITIALIZE ACT-ATTEMPT-NUMBER.
  
      *    IF TIMEOUT HAS OCCURRED, REVOKE SIGN-ON AND UPDATE QUEUE.
           IF WS-ELAPSED-MINUTES > SR-INACTIVITY-INTERVAL THEN
              SET ACT-ST-IN-PROCESS TO TRUE
              SET MON-ST-IN-PROCESS TO TRUE
              MOVE 'Sign-On Has Timed Out!' TO MON-MESSAGE

              PERFORM 2250-UPDATE-USER-ACT-QUEUE
              PERFORM 9200-REDIRECT-TO-SIGNON
           ELSE 
              SET MON-ST-SIGNED-ON TO TRUE
              MOVE 'Sign-On Still Active' TO MON-MESSAGE

              PERFORM 2250-UPDATE-USER-ACT-QUEUE
              PERFORM 9000-RETURN-TO-CALLER
           END-IF.

       5000-IN-PROCESS-CASE.
      *    SET DEFAULT RESPONSE - USER STILL IN 'IN-PROCESS'.
      *    (IT MAY CHANGE IF CONDITIONS BELOWS DICTATE IT)
           SET MON-ST-IN-PROCESS TO TRUE

      *    CHECK IF THE USER SESSION HAS TIMED OUT.
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-TIMESTAMP.
           MOVE ACT-LAST-ACTIVITY-TIMESTAMP TO WS-ACTIVITY-TIMESTAMP.     

      *    CALCULATE ELAPSED TIME IN MINUTES SINCE LAST ACTIVITY.
      *    (COULD BE NEGATIVE IF THE DATE HAS CHANGED)
           COMPUTE WS-ELAPSED-MINUTES =
              (WS-CT-HOUR * 60 + WS-CT-MINUTE) -
              (WS-AC-HOUR * 60 + WS-AC-MINUTE)
           END-COMPUTE.

      *    IF THE DATE HAS CHANGED ADD A DAY'S WORTH OF MINUTES
      *    (1440 MINUTES) TO FIX A CASE LIKE LAST ACTIVITY TIME
      *    OF 23:59 AND CURRENT TIME OF 00:01.
           IF WS-CT-DATE > WS-AC-DATE
              ADD 1440 TO WS-ELAPSED-MINUTES
           END-IF.

      *    CHECK IF TIMEOUT HAS OCCURRED.     
           IF WS-ELAPSED-MINUTES > SR-INACTIVITY-INTERVAL THEN
      *       IF SO, REDIRECT TO SIGN-ON (BACK TO THE START)
              INITIALIZE ACT-ATTEMPT-NUMBER
              MOVE 'Sign-On Attempt Has Timed Out!' TO MON-MESSAGE
              
              PERFORM 2250-UPDATE-USER-ACT-QUEUE
              PERFORM 9200-REDIRECT-TO-SIGNON
           ELSE
      *       IF NOT, CHECK IF THE USER HAS EXCEEDED MAXIMUM ATTEMPTS.
              IF ACT-ATTEMPT-NUMBER > SR-MAXIMUM-ATTEMPTS THEN
      *          IF SO, LOCK THE USER OUT.
                 SET ACT-ST-LOCKED-OUT TO TRUE
                 SET MON-ST-LOCKED-OUT TO TRUE
                 INITIALIZE ACT-ATTEMPT-NUMBER
                 MOVE 'User Is Now Locked Out!' TO MON-MESSAGE
              ELSE
      *          IF NOT, JUST INCREMENT ATTEMPT NUMBER.
                 ADD 1 TO ACT-ATTEMPT-NUMBER
                 MOVE 'Sign-On Still Active' TO MON-MESSAGE
              END-IF

      *       UPDATE QUEUE AND RETURN TO CALLER.
              PERFORM 2250-UPDATE-USER-ACT-QUEUE
              PERFORM 9000-RETURN-TO-CALLER
           END-IF.
           
      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-RETURN-TO-CALLER.
      *    UPDATE CONTAINER WITH ACTIVITY MONITORING DATA.
           EXEC CICS PUT
                CONTAINER(AC-ACTMON-CONTAINER-NAME)
                CHANNEL(AC-ACTMON-CHANNEL-NAME)
                FROM (ACTIVITY-MONITOR-CONTAINER)
                FLENGTH(LENGTH OF ACTIVITY-MONITOR-CONTAINER)
                END-EXEC.

      *    RETURN TO CALLER - END OF PROCESSING.
           EXEC CICS RETURN
                END-EXEC.

       9100-RETURN-TO-CICS.
      *    STRANGELY, WE WIPE THE USER'S SCREEN FROM HERE!
      *    (VIA AN INHERITED TERMINAL CONNECTION)
           EXEC CICS SEND CONTROL
                ERASE
                FREEKB
                TERMINAL
                END-EXEC.

      *    RETURN TO CICS - END OF PROCESSING.
           EXEC CICS RETURN
                END-EXEC.

       9200-REDIRECT-TO-SIGNON.
      *    UPDATE CONTAINER WITH ACTIVITY MONITORING DATA.
           EXEC CICS PUT
                CONTAINER(AC-ACTMON-CONTAINER-NAME)
                CHANNEL(AC-ACTMON-CHANNEL-NAME)
                FROM (ACTIVITY-MONITOR-CONTAINER)
                FLENGTH(LENGTH OF ACTIVITY-MONITOR-CONTAINER)
                END-EXEC.

      *    INITIATE A NEW SIGN-ON TRANSACTION!
           EXEC CICS START
                TRANSID(AC-SIGNON-TRANSACTION-ID)
                CHANNEL(AC-ACTMON-CHANNEL-NAME)
                TERMID(EIBTRMID)
                END-EXEC.

      *    THEN EXIT FROM THIS PROGRAM!
           PERFORM 9100-RETURN-TO-CICS.