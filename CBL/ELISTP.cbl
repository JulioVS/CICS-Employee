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
      *      - LIST EMPLOYEES MAPSET (MODIFIED VERSION WITH ARRAYS)
      *      - LIST EMPLOYEES CONTAINER.
      *      - EMPLOYEE MASTER RECORD.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS SUPPORT.
      ******************************************************************
       COPY ECONST.
       COPY ELSTMAPM.
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
          05 WS-READ-COUNTER    PIC S9(2) USAGE IS BINARY.
          05 WS-READ-DISPLAY    PIC  9(2) USAGE IS DISPLAY.
          05 WS-LINE-COUNTER    PIC S9(2) USAGE IS BINARY.
          05 WS-LINE-DISPLAY    PIC  9(2) USAGE IS DISPLAY.
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-NO-FILTERS-SET  PIC X(6)  VALUE '(None)'.
          05 WS-MESSAGE         PIC X(79) VALUE SPACES.
          05 WS-PF7-LABEL       PIC X(9)  VALUE 'PF7 Prev '.
          05 WS-PF8-LABEL       PIC X(9)  VALUE 'PF8 Next '.
      *    
       01 WS-DEBUG-MODE         PIC X(1)  VALUE 'Y'.
          88 I-AM-DEBUGGING               VALUE 'Y'.
          88 NOT-DEBUGGING                VALUE 'N'.
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
       01 WS-MAXIMUM-EMP-ID     PIC 9(8)  VALUE 99999999.
       01 WS-LINES-PER-PAGE     PIC S9(4) USAGE IS BINARY
                                          VALUE +16.
 
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           INITIALIZE EIBRESP EIBRESP2.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

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

      *    FIRST TIME WILL GIVE A '122' RET CODE - NOT AN ERROR!!!
      *
      *    IT IS SIMPLY A 'MISSING CHANNEL' CONDITION, AS THE CONTAINER
      *    WILL ACTUALLY BE CREATED ON THE FIRST 'PUT' COMMAND LOCATED
      *    IN THE '9000-SEND-MAP-AND-RETURN' PARAGRAPH.
      *
      *    FIX => NONE NEEDED!

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
           MOVE '1000-FIRST-INTERACTION (START)' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1100-INITIALIZE-VARIABLES.
           PERFORM 1200-INITIALIZE-CONTAINER.
           PERFORM 1300-READ-EMPLOYEES-BY-ID.

      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION (END)' TO WS-DEBUG-AID.
      *    >>> -------------- <<<

       1100-INITIALIZE-VARIABLES.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE-VARIABLES' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE LIST-EMPLOYEE-CONTAINER.
           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE ELSTMO.

       1200-INITIALIZE-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1200-INITIALIZE-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SET INITIAL VALUES FOR LIST CONTAINER.
           MOVE APP-LIST-PROGRAM-NAME TO LST-PROGRAM-NAME.
           MOVE 1 TO LST-CURRENT-PAGE-NUMBER.

       1300-READ-EMPLOYEES-BY-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1300-READ-EMPLOYEES-BY-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAN EMPLOYEE LIST BUFFER.
           INITIALIZE LST-CURRENT-RECORD-AREA.

      *    READ EMPLOYEE MASTER FILE RECORDS INTO CONTAINER.
           PERFORM 1310-START-BROWSING.

           PERFORM 1320-READ-NEXT-RECORD
              VARYING LST-RECORD-INDEX FROM 1 BY 1
              UNTIL LST-RECORD-INDEX IS GREATER THAN WS-LINES-PER-PAGE
              OR LST-END-OF-FILE.

           IF NOT LST-END-OF-FILE THEN
              PERFORM 1330-END-BROWSING
           END-IF.

       1310-START-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1310-START-BROWSING' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS STARTBR
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

      *    WILL GIVE A '16' (+20) ERROR RETURN CODE IF NOT DEFINED AS
      *    'BROWSABLE' IN THE CICS FILE DEFINITION ENTRY!
      *
      *    FIX => IN 'CEDA DEFINE FILE(EMPMAST)' PAGE DOWN UNTIL THE
      *           'OPERATIONS' SECTION AND SET 'BROWSE' TO 'YES'!
      *           (THEN RE-INSTALL THE FILE IN CICS)

      *    ALSO POSSIBLE IS ABEND '19' (+60) WHICH HAPPENS IF THE FILE 
      *    WAS CLOSED (I.E. BY ME!) WHEN THE PROGRAM RAN THE 'STARTBR' 
      *    COMMAND.
      *
      *    FIX => RE-INSTALL IT IN CICS AND/OR READ IT WITH 'CECI READ
      *           FILE(EMPMAST)' ETC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Browsing Employee Master File' TO WS-MESSAGE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET LST-END-OF-FILE TO TRUE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalist Request (Browse)!' TO WS-MESSAGE
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
           INITIALIZE WS-DEBUG-AID. 
           SET WS-READ-COUNTER TO LST-RECORD-INDEX.
           MOVE WS-READ-COUNTER TO WS-READ-DISPLAY.
           STRING '1320-READ-NEXT-RECORD'
                  '('
                  WS-READ-DISPLAY
                  ')'
              DELIMITED BY SIZE
              INTO WS-DEBUG-AID
           END-STRING.
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
                SET LST-END-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'End of Employee Master File' TO WS-MESSAGE
                SET LST-END-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Next Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1330-END-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1330-END-BROWSING' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS ENDBR
                FILE(APP-EMP-MASTER-FILE-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'End of Browsing Master File' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Ending Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1400-READ-BACKWARDS-BY-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1400-READ-BACKWARDS-BY-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE LST-CURRENT-RECORD-AREA.
           PERFORM 1310-START-BROWSING.

           PERFORM 1410-READ-PREV-RECORD
              VARYING LST-RECORD-INDEX FROM WS-LINES-PER-PAGE BY -1
              UNTIL LST-RECORD-INDEX IS LESS THAN 1
              OR LST-START-OF-FILE.

           IF NOT LST-START-OF-FILE THEN
              PERFORM 1330-END-BROWSING
           END-IF.

       1410-READ-PREV-RECORD.
      *    >>> DEBUGGING ONLY <<<
           INITIALIZE WS-DEBUG-AID. 
           SET WS-READ-COUNTER TO LST-RECORD-INDEX.
           MOVE WS-READ-COUNTER TO WS-READ-DISPLAY.
           STRING '1410-READ-PREV-RECORD'
                  '('
                  WS-READ-DISPLAY
                  ')'
              DELIMITED BY SIZE
              INTO WS-DEBUG-AID
           END-STRING.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS READPREV
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
                MOVE 'No Previous Records Found!' TO WS-MESSAGE
                SET LST-START-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'Start of Employee Master File' TO WS-MESSAGE
                SET LST-START-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Previous Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT (START)' TO WS-DEBUG-AID.
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
                PERFORM 2300-PREV-BY-EMPLOYEE-ID
           WHEN DFHPF8
                PERFORM 2400-NEXT-BY-EMPLOYEE-ID
           WHEN DFHPF10
                PERFORM 9200-SIGN-USER-OFF
           WHEN DFHPF12
                PERFORM 2500-CANCEL-ACTION
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
           END-EVALUATE.

      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT (END)' TO WS-DEBUG-AID.
      *    >>> -------------- <<<
               
       2100-SHOW-DETAILS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-SHOW-DETAILS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE 'Cannot Detect Cursor!' TO WS-MESSAGE.

           PERFORM VARYING LINEO-INDEX
              FROM 1 BY 1
              UNTIL LINEO-INDEX
              IS GREATER THAN WS-LINES-PER-PAGE
      *            CHECK EACH LINE TO SEE IF CURSOR IS POSITIONED THERE.
                   MOVE SELCTF(LINEO-INDEX) TO DFHBMFLG

                   IF DFHCURSR THEN
                      SET WS-LINE-COUNTER TO LINEO-INDEX
                      MOVE WS-LINE-COUNTER TO WS-LINE-DISPLAY
                      STRING 'Cursor Detected In Line '
                             WS-LINE-DISPLAY
                         DELIMITED BY SIZE
                         INTO WS-MESSAGE
                      END-STRING
                   END-IF
           END-PERFORM.
             
       2200-GET-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-GET-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

       2300-PREV-BY-EMPLOYEE-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2300-PREV-BY-EMPLOYEE-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF LST-CURRENT-PAGE-NUMBER IS GREATER THAN 1 THEN
      *       LOCATE THE FIRST EMPLOYEE ID IN THE CURRENTLY DISPLAYED
      *       PAGE) AND SUBTRACT 1 FROM IT TO GET THE STARTING POINT 
      *       FOR OUR UPCOMING 'BACKWARDS BROWSING'.    
              IF LST-CURRENT-RECORD(1) IS NOT EQUAL TO SPACES THEN
      *          >>> DEBUGGING ONLY <<<
                 MOVE '2300-PREV: NORMAL CASE' TO WS-DEBUG-AID
                 PERFORM 9300-DEBUG-AID
      *          >>> -------------- <<<
                 MOVE LST-CURRENT-RECORD(1) TO EMPLOYEE-MASTER-RECORD
                 SUBTRACT 1 FROM EMP-EMPLOYEE-ID
              ELSE
      *          >>> DEBUGGING ONLY <<<
                 MOVE '2300-PREV: EDGE CASE!' TO WS-DEBUG-AID
                 PERFORM 9300-DEBUG-AID
      *          >>> -------------- <<<
      *          UNLESS WE ARE ON AN 'EMPTY DETAIL PAGE' EDGE CASE!
      *          IN ORDER TO GO BACKWARDS, WE JUST SET THE EMPLOYEE ID 
      *          TO A FICTIONAL 'MAXIMUM VALUE'.             
                 MOVE WS-MAXIMUM-EMP-ID TO EMP-EMPLOYEE-ID
              END-IF

      *       RESET THE 'SOF'/'EOF' FILE FLAG.
              INITIALIZE LST-FILE-FLAG

      *       SUBTRACT 1 FROM THE CURRENT PAGE NUMBER. 
              SUBTRACT 1 FROM LST-CURRENT-PAGE-NUMBER

      *       AND NOW READ THE EMPLOYEE MASTER FILE BACKWARDS!!!
              PERFORM 1400-READ-BACKWARDS-BY-ID
           ELSE
              MOVE 'No Previous Records To Display' TO WS-MESSAGE
              MOVE DFHPROTN TO HLPPF7A
           END-IF.

       2400-NEXT-BY-EMPLOYEE-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2400-NEXT-BY-EMPLOYEE-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    WE ADVANCE BOTH THE PAGE NUMBER AND THE EMPLOYEE ID.
      *    THE LATTER IS TO AVOID THE LAST DISPLAYED EMPLOYEE TO BE
      *    CAUGHT AGAIN BY THE NEXT 'STARTBR' COMMAND (WHICH 
      *    CHECKS FOR AN 'EQUAL OR GREATER THAN' VALUE THAN THE
      *    PASSED ID).

           IF NOT LST-END-OF-FILE THEN
              ADD 1 TO LST-CURRENT-PAGE-NUMBER
              ADD 1 TO EMP-EMPLOYEE-ID
              PERFORM 1300-READ-EMPLOYEES-BY-ID
           ELSE
              MOVE 'No More records To Display' TO WS-MESSAGE
              MOVE DFHPROTN TO HLPPF8A
           END-IF.

       2500-CANCEL-ACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2500-CANCEL-ACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
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
           INITIALIZE ELSTMO.

      *    DISPLAY TRANSACTION ID AND PAGE NUMBER.
           MOVE EIBTRNID TO TRANIDO.
           MOVE LST-CURRENT-PAGE-NUMBER TO PAGENO.

           IF LST-NO-FILTERS-SET THEN
              MOVE WS-NO-FILTERS-SET TO FLTRSO
           END-IF.

      *    POPULATE THE ALL-IMPORTANT MESSAGE LINE!
           MOVE WS-MESSAGE TO MESSO.

      *    POPULATE THE NAVIGATION FUNCTION KEY LABELS.
           IF LST-CURRENT-PAGE-NUMBER IS GREATER THAN 1 THEN
              MOVE WS-PF7-LABEL TO HLPPF7O
           END-IF.
           IF NOT LST-END-OF-FILE THEN
              MOVE WS-PF8-LABEL TO HLPPF8O
           END-IF.

      *    POPULATE ALL DISPLAY LINES WITH EMPLOYEE RECORDS.
           PERFORM VARYING LST-RECORD-INDEX
              FROM 1 BY 1
              UNTIL LST-RECORD-INDEX IS GREATER THAN WS-LINES-PER-PAGE
      *            LOAD EACH RECORD INTO THE DISPLAY BUFFER.        
                   MOVE LST-CURRENT-RECORD(LST-RECORD-INDEX)
                      TO EMPLOYEE-MASTER-RECORD

      *            SET THE MAP ARRAY INDEX TO THE CURRENT LIST 
      *            CONTAINER RECORD INDEX VALUE!
                   SET LINEO-INDEX TO LST-RECORD-INDEX 

      *            AND HERE, USE THE MAP INDEX! (IMPORTANT)
                   MOVE EMP-EMPLOYEE-ID TO EMPIDO(LINEO-INDEX)
                   MOVE EMP-PRIMARY-NAME TO PRMNMO(LINEO-INDEX)
                   MOVE EMP-JOB-TITLE TO JOBTLO(LINEO-INDEX)
                   MOVE EMP-DEPARTMENT-ID TO DPTIDO(LINEO-INDEX)
           END-PERFORM.

       9200-SIGN-USER-OFF.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9200-SIGN-USER-OFF' TO WS-DEBUG-AID.
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