       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVIEWP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'VIEW EMPLOYEE DETAILS' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - EMPLOYEE DETAILS MAPSET.
      *      - EMPLOYEE DETAILS CONTAINER.
      *      - EMPLOYEE MASTER RECORD.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EDETMAP.
       COPY EDETCTR.
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
          05 WS-MESSAGE         PIC X(79) VALUE SPACES.
          05 WS-PF7-LABEL       PIC X(9)  VALUE 'PF7 Prev '.
          05 WS-PF8-LABEL       PIC X(9)  VALUE 'PF8 Next '.
      *
       01 WS-DATE-FORMATTING.
          05 WS-INPUT-DATE.
             10 WS-YYYY         PIC X(4)  VALUE SPACES.
             10 WS-MM           PIC X(2)  VALUE SPACES.
             10 WS-DD           PIC X(2)  VALUE SPACES.
          05 WS-OUTPUT-DATE.
             10 WS-DD           PIC X(2)  VALUE SPACES.
             10 FILLER          PIC X(1)  VALUE '-'.
             10 WS-MM           PIC X(2)  VALUE SPACES.
             10 FILLER          PIC X(1)  VALUE '-'.
             10 WS-YYYY         PIC X(4)  VALUE SPACES.
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

      *    PSEUDO-CONVERSATIONAL PROGRAM DESIGN.

      *    START BY GETTING THE DETAIL CONTAINER:
      *
      *    - IF IT DOES NOT YET EXIST -> 1ST STEP IN CONVERSATION
      *    - IF IT DOES ALREADY EXIST -> CONVERSATION IN PROGRESS

           EXEC CICS GET
                CONTAINER(APP-VIEW-CONTAINER-NAME)
                CHANNEL(APP-VIEW-CHANNEL-NAME)
                INTO (EMPLOYEE-DETAILS-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(CONTAINERERR)
           WHEN DFHRESP(CHANNELERR)
      *         1ST INTERACTION -> NO CONTAINER YET (CREATE IT)
                PERFORM 1000-FIRST-INTERACTION
           WHEN DFHRESP(NORMAL)
      *         NEXT INTERACTIONS -> CONTAINER FOUND (CONTINUE)
                PERFORM 2000-PROCESS-USER-INPUT
           WHEN OTHER
                MOVE 'Error Retrieving View Container!' TO WS-MESSAGE
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

           PERFORM 1100-INITIALIZE-VARIABLES.
           PERFORM 1200-INITIALIZE-CONTAINER.
           PERFORM 1300-READ-EMPLOYEE-BY-KEY.

       1100-INITIALIZE-VARIABLES.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE-VARIABLES' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE EMPLOYEE-DETAILS-CONTAINER.
           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE EDETMO.

       1200-INITIALIZE-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1200-INITIALIZE-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SET INITIAL VALUES FOR LIST CONTAINER.
           MOVE '1' TO DET-SELECT-KEY-TYPE.
      *    MOVE '2' TO DET-SELECT-KEY-TYPE.
           MOVE LOW-VALUE TO DET-SELECT-KEY-VALUE.

       1300-READ-EMPLOYEE-BY-KEY.
      *    >>> DEBUGGING ONLY <<<
           IF DET-SEL-BY-EMPLOYEE-ID THEN
              MOVE '1300-READ-EMPLOYEE-BY-KEY (ID)' TO WS-DEBUG-AID
           ELSE
              MOVE '1300-READ-EMPLOYEE-BY-KEY (NM)' TO WS-DEBUG-AID
           END-IF.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    READ EMPLOYEE MASTER FILE RECORD INTO CONTAINER.
           PERFORM 1310-START-BROWSING.

      *    >>> DEBUGGING ONLY <<<
           IF I-AM-DEBUGGING AND DET-SEL-BY-EMPLOYEE-ID AND
              EMP-EMPLOYEE-ID IS GREATER THAN 3 THEN
              SET DET-END-OF-FILE TO TRUE
           END-IF.
           IF I-AM-DEBUGGING AND DET-SEL-BY-EMPLOYEE-NAME AND
              EMP-PRIMARY-NAME(1:1) IS GREATER THAN 'A' THEN
              SET DET-END-OF-FILE TO TRUE
           END-IF.
      *    >>> -------------- <<<
Í
           IF NOT DET-END-OF-FILE THEN
              PERFORM 1320-READ-NEXT-RECORD
              PERFORM 1330-END-BROWSING
           END-IF.

      *    IF NO RECORD WAS FOUND, WE DISPLAY A MESSAGE.
           IF DET-EMPLOYEE-RECORD IS EQUAL TO SPACES THEN
      *       IF LST-FILTERS-SET THEN
      *          MOVE 'No Matching Records Found!' TO WS-MESSAGE
      *       ELSE
              MOVE 'No Record Found!' TO WS-MESSAGE
      *       END-IF
           END-IF.


       1310-START-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           IF DET-SEL-BY-EMPLOYEE-ID THEN
              MOVE '1310-START-BROWSING (ID)' TO WS-DEBUG-AID
           ELSE
              MOVE '1310-START-BROWSING (NM)' TO WS-DEBUG-AID
           END-IF.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF DET-SEL-BY-EMPLOYEE-ID THEN
              EXEC CICS STARTBR
                   FILE(APP-EMP-MASTER-FILE-NAME)
                   RIDFLD(EMP-EMPLOYEE-ID)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           ELSE
              EXEC CICS STARTBR
                   FILE(APP-EMP-MASTER-PATH-NAME)
                   RIDFLD(EMP-PRIMARY-NAME)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           END-IF.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Browsing Employee Master File' TO WS-MESSAGE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET DET-END-OF-FILE TO TRUE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (Browse)!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Master File Not Open!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE 'Error Starting Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1320-READ-NEXT-RECORD.
      *    >>> DEBUGGING ONLY <<<
           IF DET-SEL-BY-EMPLOYEE-ID THEN
              MOVE '1320-READ-NEXT-RECORD (ID)' TO WS-DEBUG-AID
           ELSE
              MOVE '1320-READ-NEXT-RECORD (NM)' TO WS-DEBUG-AID
           END-IF.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF DET-SEL-BY-EMPLOYEE-ID THEN
              EXEC CICS READNEXT
                   FILE(APP-EMP-MASTER-FILE-NAME)
                   RIDFLD(EMP-EMPLOYEE-ID)
                   INTO (EMPLOYEE-MASTER-RECORD)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           ELSE
              EXEC CICS READNEXT
                   FILE(APP-EMP-MASTER-PATH-NAME)
                   RIDFLD(EMP-PRIMARY-NAME)
                   INTO (EMPLOYEE-MASTER-RECORD)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           END-IF.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Reading Employee Master File' TO WS-MESSAGE
                MOVE EMPLOYEE-MASTER-RECORD TO DET-EMPLOYEE-RECORD
      *         PERFORM 3200-APPLY-FILTERS
      *         IF WS-FILTERS-PASSED THEN
      *            MOVE EMPLOYEE-MASTER-RECORD TO DET-EMPLOYEE-RECORD
      *         END-IF
           WHEN DFHRESP(NOTFND)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET DET-END-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'End of Employee Master File' TO WS-MESSAGE
                SET DET-END-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1330-END-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           IF DET-SEL-BY-EMPLOYEE-ID THEN
              MOVE '1330-END-BROWSING (ID)' TO WS-DEBUG-AID
           ELSE
              MOVE '1330-END-BROWSING (NM)' TO WS-DEBUG-AID
           END-IF
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF DET-SEL-BY-EMPLOYEE-ID THEN
              EXEC CICS ENDBR
                   FILE(APP-EMP-MASTER-FILE-NAME)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           ELSE
              EXEC CICS ENDBR
                   FILE(APP-EMP-MASTER-PATH-NAME)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           END-IF.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'End of Browsing Master File' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Ending Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *-----------------------------------------------------------------
       VIEWING SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS RECEIVE
                MAP(APP-VIEW-MAP-NAME)
                MAPSET(APP-VIEW-MAPSET-NAME)
                INTO (EDETMI)
                END-EXEC.

           EVALUATE EIBAID
           WHEN DFHENTER
      *         PERFORM 2100-SHOW-DETAILS
      *    WHEN DFHPF3
      *         PERFORM 2200-SHOW-FILTERS
      *    WHEN DFHPF7
      *         PERFORM 2300-PREV-BY-EMPLOYEE-KEY
           WHEN DFHPF8
                PERFORM 2400-NEXT-BY-EMPLOYEE-KEY
           WHEN DFHPF10
                PERFORM 2500-SIGN-USER-OFF
           WHEN DFHPF12
                PERFORM 2600-CANCEL-ACTION
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
           END-EVALUATE.

       2400-NEXT-BY-EMPLOYEE-KEY.
      *    >>> DEBUGGING ONLY <<<
           IF DET-SEL-BY-EMPLOYEE-ID THEN
              MOVE '2400-NEXT-BY-EMPLOYEE-KEY (ID)' TO WS-DEBUG-AID
           ELSE
              MOVE '2400-NEXT-BY-EMPLOYEE-KEY (NM)' TO WS-DEBUG-AID
           END-IF.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE DET-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

           IF NOT DET-END-OF-FILE THEN
              IF DET-SEL-BY-EMPLOYEE-ID THEN
                 ADD 1 TO EMP-EMPLOYEE-ID
              ELSE
                 MOVE HIGH-VALUES TO EMP-PRIMARY-NAME(38:)
              END-IF
              PERFORM 1300-READ-EMPLOYEE-BY-KEY
           ELSE
              MOVE 'No More Records To Display!' TO WS-MESSAGE
              MOVE DFHPROTN TO HLPPF8A
           END-IF.

       2500-SIGN-USER-OFF.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2500-SIGN-USER-OFF' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    >>> CALL ACTIVITY MONITOR <<<
      *    SET MON-AC-SIGN-OFF TO TRUE.
      *    PERFORM 4200-CALL-ACTIVITY-MONITOR.
      **    >>> --------------------- <<<

           PERFORM 9200-RETURN-TO-CICS.

       2600-CANCEL-ACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2600-CANCEL-ACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 9200-RETURN-TO-CICS.

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

           PERFORM 9100-POPULATE-MAP.

           EXEC CICS PUT
                CONTAINER(APP-VIEW-CONTAINER-NAME)
                CHANNEL(APP-VIEW-CHANNEL-NAME)
                FROM (EMPLOYEE-DETAILS-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Error Putting View Container!' TO WS-MESSAGE
           END-EVALUATE.

           EXEC CICS SEND
                MAP(APP-VIEW-MAP-NAME)
                MAPSET(APP-VIEW-MAPSET-NAME)
                FROM (EDETMO)
                ERASE
                FREEKB
                END-EXEC.

           EXEC CICS RETURN
                CHANNEL(APP-VIEW-CHANNEL-NAME)
                TRANSID(APP-VIEW-TRANSACTION-ID)
                END-EXEC.

       9100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-POPULATE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE EDETMO.

           MOVE DET-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

      *    DISPLAY TRANSACTION ID.
           MOVE EIBTRNID TO TRANIDO.

      *    DISPLAY EMPLOYEE INFORMATION.
           MOVE EMP-EMPLOYEE-ID TO EMPLIDO.
           MOVE EMP-PRIMARY-NAME TO PRNAMEO.
           MOVE EMP-HONORIFIC TO HONORO.
           MOVE EMP-SHORT-NAME TO SHNAMEO.
           MOVE EMP-FULL-NAME TO FLNAMEO.
           MOVE EMP-JOB-TITLE TO JBTITLO.
           MOVE EMP-DEPARTMENT-ID TO DEPTIDO.
           MOVE '(Undefined)' TO DEPTNMO.

           MOVE EMP-START-DATE TO WS-INPUT-DATE.
           MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE.
           MOVE WS-OUTPUT-DATE TO STDATEO.
               
           MOVE EMP-END-DATE TO WS-INPUT-DATE.
           MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE.
           MOVE WS-OUTPUT-DATE TO ENDATEO.

           MOVE EMP-APPRAISAL-DATE TO WS-INPUT-DATE.
           MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE.
           MOVE WS-OUTPUT-DATE TO APPRDTO.

           MOVE EMP-APPRAISAL-RESULT TO APPRRSO.
           MOVE EMP-DELETE-FLAG TO DELFLGO.

           EVALUATE TRUE
           WHEN EMP-ACTIVE
                MOVE 'Active' TO DELDSCO
           WHEN EMP-DELETED
                MOVE 'Deleted' TO DELDSCO
           WHEN OTHER
                MOVE '(Undefined)' TO DELDSCO
           END-EVALUATE

           MOVE EMP-DELETE-DATE TO WS-INPUT-DATE.
           MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE.
           MOVE WS-OUTPUT-DATE TO DELDTO.

      *    POPULATE THE ALL-IMPORTANT MESSAGE LINE!
           MOVE WS-MESSAGE TO MESSO.
           MOVE DFHTURQ TO MESSC.

      *    CHANGE COLOR OF MESSAGE LINE BASED ON TYPE/CONTENT.
           EVALUATE TRUE
           WHEN WS-MESSAGE(1:5) IS EQUAL TO 'Error'
                MOVE DFHRED TO MESSC
           WHEN WS-MESSAGE(1:3) IS EQUAL TO 'No '
                MOVE DFHYELLO TO MESSC
           WHEN WS-MESSAGE(1:7) IS EQUAL TO 'Invalid'
                MOVE DFHPINK TO MESSC
           END-EVALUATE

      *    HERE, WE SET THE MODIFIED DATA TAG (MDT) OF ONE THE FIELDS
      *    TO 'ON' TO AVOID THE 'AEI9' ABEND THAT HAPPENS DUE TO A
      *    'MAPFAIL' CONDITION WHEN WE LATER RECEIVE THE MAP WITH JUST
      *    AN AID KEY PRESS AND NO MODIFIED DATA ON IT.
           MOVE DFHBMFSE TO EMPLIDA.

      *    POPULATE THE NAVIGATION FUNCTION KEY LABELS.
           IF NOT DET-TOP-OF-FILE THEN
              MOVE WS-PF7-LABEL TO HLPPF7O
           END-IF.
           IF NOT DET-END-OF-FILE THEN
              MOVE WS-PF8-LABEL TO HLPPF8O
           END-IF.

       9200-RETURN-TO-CICS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9200-RETURN-TO-CICS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SIGN-OFF OR CANCEL:
      *      - CLEAR TERMINAL SCREEN.
      *      - COLD RETURN TO CICS.
      *      - END OF CONVERSATION.

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