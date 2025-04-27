       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELISTP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'
      *      - 'LIST EMPLOYEES' PROGRAM
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - LIST EMPLOYESS MAPSET.
      *      - LIST EMPLOYEES CONTAINER.
      *      - EMPLOYEE MASTER RECORD.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS SUPPORT.
      ******************************************************************
       COPY ECONST.
       COPY ELSTMAP.
       COPY ELSTCTR.
       COPY EMPMAST.
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
          05 WS-NO-FILTERS-SET  PIC X(6)  VALUE '(None)'.
          05 WS-MESSAGE         PIC X(79) VALUE SPACES.
      *    
       01 WS-DEBUG-MODE         PIC X(1)  VALUE 'Y'.
          88 I-AM-DEBUGGING               VALUE 'Y'.
          88 NOT-DEBUGGING                VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    PSEUDO-CONVERSATIONAL PROGRAM DESIGN.
      *
      *    START BY GETTING THE LIST CONTAINER:
      *
      *    - IF IT DOES NOT YET EXIST -> 1ST STEP IN CONVERSATION
      *    - IF IT DOES ALREADY EXIST -> CONVERSATION IN PROGRESS

           EXEC CICS GET
                CONTAINER(APP-LIST-CONTAINER-NAME)
                CHANNEL(APP-LIST-CHANNEL-NAME)
                INTO (LIST-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.
           
           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                PERFORM 2000-PROCESS-USER-INPUT
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
                PERFORM 1000-FIRST-INTERACTION
           WHEN OTHER
                MOVE 'Error Retrieving Container!' TO WS-MESSAGE 
           END-EVALUATE.

           PERFORM 9000-SEND-MAP-AND-RETURN.


      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------
      
       1000-FIRST-INTERACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1100-INITIALIZE.

      *    READ EMPLOYEE MASTER FILE RECORDS INTO CONTAINER.
           PERFORM 1200-START-BROWSING.
           PERFORM 1300-READ-NEXT-RECORD
              VARYING LST-RECORD-INDEX
              FROM 1 BY 1
              UNTIL LST-RECORD-INDEX IS GREATER THAN 3.
           PERFORM 1400-END-BROWSING.

       1100-INITIALIZE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE LIST-EMPLOYEE-CONTAINER.
           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE ELSTMO.

      *    SET INITIAL VALUES FOR LIST CONTAINER.
           MOVE APP-LIST-PROGRAM-NAME TO LST-PROGRAM-NAME.
           MOVE 1 TO LST-CURRENT-PAGE-NUMBER.

       1200-START-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1200-START-BROWSING' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS STARTBR
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Browsing Employee Master File' TO WS-MESSAGE
      *         >>> DEBUGGING ONLY <<<
                PERFORM 9300-DEBUG-AID
      *         >>> -------------- <<<
           WHEN OTHER
                MOVE 'Error Starting Browse!' TO WS-MESSAGE
      *         >>> DEBUGGING ONLY <<<
                PERFORM 9300-DEBUG-AID
      *         >>> -------------- <<<
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1300-READ-NEXT-RECORD.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1300-READ-NEXT-RECORD' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS READNEXT
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                INTO (EMPLOYEE-MASTER-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE 
           WHEN DFHRESP(NORMAL)
                MOVE 'Reading Employee Master File' TO WS-MESSAGE
                MOVE EMPLOYEE-MASTER-RECORD TO
                   LST-CURRENT-RECORD(LST-RECORD-INDEX)
           WHEN DFHRESP(NOTFND)
                MOVE 'No More Records Found!' TO WS-MESSAGE
           WHEN DFHRESP(ENDFILE)
                MOVE 'End of Employee Master File' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Reading Next Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1400-END-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1400-END-BROWSING' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS ENDBR
                FILE(APP-EMP-MASTER-FILE-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'End of Employee Master File' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Ending Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS RECEIVE
                MAP(APP-LIST-MAP-NAME)
                MAPSET(APP-LIST-MAPSET-NAME)
                INTO (ELSTMI)
                END-EXEC.

           EVALUATE EIBAID 
           WHEN DFHENTER
                PERFORM 2100-SHOW-DETAILS
           WHEN DFHPF3
                PERFORM 2200-GET-FILTERS
           WHEN DFHPF7
                PERFORM 2300-PREV-PAGE
           WHEN DFHPF8
                PERFORM 2400-NEXT-PAGE
           WHEN DFHPF10
                PERFORM 9200-SIGN-USER-OFF
           WHEN DFHPF12
                PERFORM 2500-CANCEL-PROCESS
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
           END-EVALUATE.

      *    >>> DEBUGGING ONLY <<<
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<
               
       2100-SHOW-DETAILS.
           MOVE '2100: Cannot Detect Cursor!' TO WS-MESSAGE.

           MOVE SELCT01F TO DFHBMFLG.
           IF DFHCURSR THEN
              MOVE '2100: Cursor Detected In Line 1' TO WS-MESSAGE
           END-IF.

           MOVE SELCT02F TO DFHBMFLG.
           IF DFHCURSR THEN
              MOVE '2100: Cursor Detected In Line 2' TO WS-MESSAGE
           END-IF.

           MOVE SELCT03F TO DFHBMFLG.
           IF DFHCURSR THEN
              MOVE '2100: Cursor Detected In Line 3' TO WS-MESSAGE
           END-IF.
             
       2200-GET-FILTERS.
           MOVE '2200: Get Filter (Not Coded Yet)' TO WS-MESSAGE.

       2300-PREV-PAGE.
           MOVE '2300: Previous Page (Not Coded Yet)' TO WS-MESSAGE.

       2400-NEXT-PAGE.
           MOVE '2400: Next Page (Not Coded Yet)' TO WS-MESSAGE.

       2500-CANCEL-PROCESS.
           MOVE '2500: Cancel Process (Not Coded Yet)' TO WS-MESSAGE.

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-SEND-MAP-AND-RETURN' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PSEUDO-CONVERSATIONAL RETURN:
      *      - PUT THE CONTAINER BACK TO CICS.
      *      - POPULATE AND SEND MAP TO CICS.
      *      - RETURN TO CICS.

           PERFORM 9100-POPULATE-MAP.

           EXEC CICS PUT
                CONTAINER(APP-LIST-CONTAINER-NAME)
                CHANNEL(APP-LIST-CHANNEL-NAME)
                FROM (LIST-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Error Putting Container!' TO WS-MESSAGE
           END-EVALUATE.
           
           EXEC CICS SEND
                MAP(APP-LIST-MAP-NAME)
                MAPSET(APP-LIST-MAPSET-NAME)
                FROM (ELSTMO)
                ERASE
                FREEKB 
                END-EXEC.

           EXEC CICS RETURN
                CHANNEL(APP-LIST-CHANNEL-NAME)
                TRANSID(EIBTRNID)
                END-EXEC.

       9100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-POPULATE-MAP' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE ELSTMO.

      *    DISPLAY TRANSACTION ID AND PAGE NUMBER.
           MOVE EIBTRNID TO TRANIDO.
           MOVE LST-CURRENT-PAGE-NUMBER TO PAGENO.

           IF LST-NO-FILTERS-SET THEN
              MOVE WS-NO-FILTERS-SET TO FLTRSO
           END-IF.

      *    POPULATE LINES 1-3 WITH EMPLOYEE RECORDS.
           MOVE LST-CURRENT-RECORD(1) TO EMPLOYEE-MASTER-RECORD.
           MOVE EMP-EMPLOYEE-ID TO EMPID01O.
           MOVE EMP-PRIMARY-NAME TO PRMNM01O.
           MOVE EMP-JOB-TITLE TO JOBTL01O.
           MOVE EMP-DEPARTMENT-ID TO DPTID01O.

           MOVE LST-CURRENT-RECORD(2) TO EMPLOYEE-MASTER-RECORD.
           MOVE EMP-EMPLOYEE-ID TO EMPID02O.
           MOVE EMP-PRIMARY-NAME TO PRMNM02O.
           MOVE EMP-JOB-TITLE TO JOBTL02O.
           MOVE EMP-DEPARTMENT-ID TO DPTID02O.

           MOVE LST-CURRENT-RECORD(3) TO EMPLOYEE-MASTER-RECORD.
           MOVE EMP-EMPLOYEE-ID TO EMPID03O.
           MOVE EMP-PRIMARY-NAME TO PRMNM03O.
           MOVE EMP-JOB-TITLE TO JOBTL03O.
           MOVE EMP-DEPARTMENT-ID TO DPTID03O.

           MOVE WS-MESSAGE TO MESSO.

       9200-SIGN-USER-OFF.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9200-SIGN-USER-OFF' TO WS-MESSAGE.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SIGN USER OFF FROM CICS:
      *      - CLEAR TERMINAL SCREEN.
      *      - COLD RETURN TO CICS.

           EXEC CICS SEND CONTROL
                ERASE
                FREEKB 
                END-EXEC.

           EXEC CICS RETURN
                END-EXEC.

       9300-DEBUG-AID.
      *    >>> DEBUGGING ONLY <<<
           IF I-AM-DEBUGGING THEN
              EXEC CICS SEND TEXT
                   FROM (WS-MESSAGE)
                   END-EXEC
              EXEC CICS RECEIVE
                   LENGTH(LENGTH OF EIBAID)
                   END-EXEC
           END-IF.
      *    >>> -------------- <<<