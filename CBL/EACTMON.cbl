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

           PERFORM 1000-INITIAL-SETUP.
           PERFORM 2000-PROCESS-REQUEST.
           PERFORM 9000-RETURN-TO-CALLER.

      *-----------------------------------------------------------------
       SUB-ROUTINE SECTION.
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
                END-EXEC.

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
                MOVE 'SIGN-ON RULES READQ EXCEPTION' TO MON-MESSAGE 
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
                MOVE 'SIGN-ON RULES FILE EXCEPTION' TO MON-MESSAGE 
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
                MOVE 'SIGN-ON RULES WRITEQ EXCEPTION' TO MON-MESSAGE 
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
                MOVE 'USER ACTIVITY READQ EXCEPTION' TO MON-MESSAGE 
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
              MOVE 'SIGN-ON IN PROCESS' TO MON-MESSAGE
              PERFORM 9000-RETURN-TO-CALLER
           ELSE
      *       INVALID SCENARIO - REPORT AND LEAVE.
              MOVE 'USER ACTIVITY NO-QUEUE EXCEPTION' TO MON-MESSAGE 
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
                MOVE 'USER ACTIVITY WRITEQ EXCEPTION' TO MON-MESSAGE 
                SET MON-PROCESSING-ERROR TO TRUE
                PERFORM 9000-RETURN-TO-CALLER
           END-EVALUATE.
                  
       2000-PROCESS-REQUEST.
           CONTINUE.

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