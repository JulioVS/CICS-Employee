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
          05 WS-CICS-RESPONSE          PIC S9(8) USAGE IS BINARY.
          05 WS-READ-COUNTER           PIC 9(2).
          05 WS-LINE-COUNTER           PIC S9(2) USAGE IS BINARY.
          05 WS-LINE-DISPLAY           PIC 9(2).
          05 WS-INSP-COUNTER           PIC S9(2) USAGE IS BINARY.
          05 WS-INDEX                  PIC S9(2) USAGE IS BINARY.
          05 WS-DEPT-KEY               PIC X(8).
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-MESSAGE                PIC X(79) VALUE SPACES.
          05 WS-PF7-LABEL              PIC X(9)  VALUE 'PF7 Prev '.
          05 WS-PF8-LABEL              PIC X(9)  VALUE 'PF8 Next '.
          05 WS-FILTERS-MSG-SF         PIC X(79)
             VALUE 'Set Filter Criteria And Press ENTER Or Leave Blank F
      -            'or Full Listing.'.
          05 WS-FILTERS-MSG-EF         PIC X(79)
                                                 VALUE
                'Edit Filter Criteria And Press ENTER To Continue.'.
      *
       01 WS-FILTER-FLAGS.
          03 WS-FILTERS-CHECK          PIC X(1)  VALUE SPACES.
             88 WS-FILTERS-PASSED                VALUE 'Y'.
          03 WS-KEY-FILTER-CHECK       PIC X(1)  VALUE SPACES.
             88 WS-KEY-FILTER-PASSED             VALUE 'Y'.
          03 WS-DEPT-FILTER-CHECK      PIC X(1)  VALUE SPACES.
             88 WS-DEPT-FILTER-PASSED            VALUE 'Y'.
             88 WS-DEPT-FILTER-FAILED            VALUE 'N'.
          03 WS-DATE-FILTER-CHECK      PIC X(1)  VALUE SPACES.
             88 WS-DATE-FILTER-PASSED            VALUE 'Y'.
          03 WS-FILTER-ACTIONS         PIC X(1)  VALUE SPACES.
             88 WS-ACTION-DISPLAY                VALUE 'D'.
             88 WS-ACTION-EXIT                   VALUE 'E'.
             88 WS-ACTION-SIGN-OFF               VALUE 'S'.
             88 WS-ACTION-CLEAR                  VALUE 'C'.
             88 WS-ACTION-INVALID                VALUE 'I'.
             88 WS-ACTION-NOT-SET                VALUE SPACES.
      *
       01 WS-MAXIMUM-EMP-ID            PIC 9(8)  VALUE 99999999.
       01 WS-LINES-PER-PAGE            PIC S9(4) USAGE IS BINARY
                                                 VALUE +16.
      *
       01 WS-DEBUG-AID                 PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER                    PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT             PIC X(45) VALUE SPACES.
          05 FILLER                    PIC X(1)  VALUE '>'.
          05 FILLER                    PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP          PIC 9(8)  VALUE ZEROES.
          05 FILLER                    PIC X(1)  VALUE '>'.
          05 FILLER                    PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2         PIC 9(8)  VALUE ZEROES.
          05 FILLER                    PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE                PIC X(1)  VALUE SPACES.
          88 I-AM-DEBUGGING                      VALUE 'Y'.
          88 NOT-DEBUGGING                       VALUE SPACES.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           INITIALIZE EIBRESP EIBRESP2.
           PERFORM 9300-DEBUG-AID.

      *    UNCOMMENT THE FOLLOWING LINE FOR DEBUGGING MODE!
      *    SET I-AM-DEBUGGING TO TRUE

           IF I-AM-DEBUGGING THEN 
              MOVE 3 TO WS-LINES-PER-PAGE
           END-IF.
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
           PERFORM 1150-INITIALIZE-CONTAINER.
           PERFORM 1200-GET-INITIAL-FILTERS.
           PERFORM 1300-READ-EMPLOYEES-BY-ID.

      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION (END)' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
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

       1150-INITIALIZE-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1150-INITIALIZE-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SET INITIAL VALUES FOR LIST CONTAINER.
           MOVE APP-LIST-PROGRAM-NAME TO LST-PROGRAM-NAME.
           MOVE 1 TO LST-CURRENT-PAGE-NUMBER.

       1200-GET-INITIAL-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1200-GET-INITIAL-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    BY DESIGN, WE START BY SHOWING THE FILTERS SCREEN BEFORE
      *    RENDERING THE FIRST LISTING PAGE.

      *    THIS WILL BE A 'FULLY CONVERSATIONAL' MAP DISPLAY.

      *    AFTER THE USER SETS INITIAL FILTER VALUES (OR LEAVES THEM 
      *    BLANK) LOGIC WILL MOVE FORWARDS INTO THE NEXT STEPS, IE. 
      *    '1300-READ-EMPLOYEES-BY-ID', ET AL.

           INITIALIZE WS-FILTER-ACTIONS.

           PERFORM 3000-DISPLAY-FILTERS-SCREEN
              UNTIL WS-ACTION-DISPLAY
              OR WS-ACTION-EXIT
              OR WS-ACTION-SIGN-OFF.

       1300-READ-EMPLOYEES-BY-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1300-READ-EMPLOYEES-BY-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAN EMPLOYEE LIST BUFFER.
           INITIALIZE LST-CURRENT-RECORD-AREA.
           INITIALIZE WS-READ-COUNTER.

      *    READ EMPLOYEE MASTER FILE RECORDS INTO CONTAINER.
           PERFORM 1310-START-BROWSING.

           SET LST-RECORD-INDEX TO 1
           PERFORM 1320-READ-NEXT-RECORD
              UNTIL LST-RECORD-INDEX IS GREATER THAN WS-LINES-PER-PAGE
              OR LST-END-OF-FILE.

           IF NOT LST-END-OF-FILE THEN
              PERFORM 1330-END-BROWSING
           END-IF.

      *    IF NO RECORDS WERE FOUND ON THIS CYCLE, WE DISPLAY A MESSAGE.
           IF LST-CURRENT-RECORD-AREA IS EQUAL TO SPACES THEN
              IF LST-CURRENT-PAGE-NUMBER IS EQUAL TO 1 THEN
                 IF LST-FILTERS-SET THEN
                    MOVE 'No Matching Records Found!' TO WS-MESSAGE
                 ELSE
                    MOVE 'No Records Found!' TO WS-MESSAGE
                 END-IF
              ELSE
                 MOVE 'No More Records Found!' TO WS-MESSAGE
              END-IF
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
           INITIALIZE WS-DEBUG-AID.
           ADD 1 TO WS-READ-COUNTER.
           STRING '1320-READ-NEXT-RECORD'
                  '('
                  WS-READ-COUNTER
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
                PERFORM 3200-APPLY-FILTERS

                IF WS-FILTERS-PASSED THEN
                   MOVE EMPLOYEE-MASTER-RECORD TO
                      LST-CURRENT-RECORD(LST-RECORD-INDEX)
                   SET LST-RECORD-INDEX UP BY 1
                END-IF
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
           INITIALIZE WS-READ-COUNTER.

           PERFORM 1310-START-BROWSING.

           SET LST-RECORD-INDEX TO WS-LINES-PER-PAGE.
           PERFORM 1410-READ-PREV-RECORD
              UNTIL LST-RECORD-INDEX IS LESS THAN 1
              OR LST-TOP-OF-FILE.

           IF NOT LST-TOP-OF-FILE THEN
              PERFORM 1330-END-BROWSING
           END-IF.

       1410-READ-PREV-RECORD.
      *    >>> DEBUGGING ONLY <<<
           INITIALIZE WS-DEBUG-AID.
           ADD 1 TO WS-READ-COUNTER.
           STRING '1410-READ-PREV-RECORD'
                  '('
                  WS-READ-COUNTER
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
                PERFORM 3200-APPLY-FILTERS

                IF WS-FILTERS-PASSED THEN
                   MOVE EMPLOYEE-MASTER-RECORD TO
                      LST-CURRENT-RECORD(LST-RECORD-INDEX)
                   SET LST-RECORD-INDEX DOWN BY 1
                END-IF
           WHEN DFHRESP(NOTFND)
                MOVE 'No Previous Records Found!' TO WS-MESSAGE
                SET LST-TOP-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'Start of Employee Master File' TO WS-MESSAGE
                SET LST-TOP-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Previous Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *-----------------------------------------------------------------
       LISTING SECTION.
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
                PERFORM 2200-SHOW-FILTERS
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
           PERFORM 9300-DEBUG-AID.
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

       2200-SHOW-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-SHOW-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE WS-FILTER-ACTIONS.

           PERFORM 3000-DISPLAY-FILTERS-SCREEN
              UNTIL WS-ACTION-DISPLAY
              OR WS-ACTION-EXIT
              OR WS-ACTION-SIGN-OFF.

           PERFORM 2210-RESET-BROWSING-VALUES.
           PERFORM 1300-READ-EMPLOYEES-BY-ID.

       2210-RESET-BROWSING-VALUES.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2210-RESET-BROWSING-VALUES' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE LST-CURRENT-RECORD-AREA.
           INITIALIZE LST-FILE-FLAG.

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

      *       RESET THE 'TOF'/'EOF' FILE FLAG.
              INITIALIZE LST-FILE-FLAG

              SUBTRACT 1 FROM LST-CURRENT-PAGE-NUMBER

      *       AND NOW READ THE EMPLOYEE MASTER FILE BACKWARDS!!!
              PERFORM 1400-READ-BACKWARDS-BY-ID
           ELSE
              MOVE 'No Previous Records To Display!' TO WS-MESSAGE
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
              MOVE 'No More Records To Display!' TO WS-MESSAGE
              MOVE DFHPROTN TO HLPPF8A
           END-IF.

       2500-CANCEL-ACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2500-CANCEL-ACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           CONTINUE.

      *-----------------------------------------------------------------
       FILTERS SECTION.
      *-----------------------------------------------------------------

       3000-DISPLAY-FILTERS-SCREEN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3000-DISPLAY-FILTERS-SCREEN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    THIS IS A 'FULLY CONVERSATIONAL' INVOCATION TO THE FILTERS
      *    MAP SCREEN. THE MAP IS DISPLAYED AND THE USER CAN ENTER
      *    FILTER CRITERIA. THE MAP IS THEN SENT BACK TO THIS PROGRAM.
      *
      *    UNLIKE THE PLURALSIGHT EXAMPLE, I CHOSE *NOT* TO DO A
      *    PSEUDO-CONVERSATIONAL 'FILTERS' LOGIC BECAUSE:
      *
      *       - IT MAKES NO SENSE AT ALL - YOU EITHER SET OR NOT SET
      *         THE FILTER FIELDS AND THEN QUICKLY MOVE INTO THE
      *         LISTING PAGE SO NO NEED FOR A 'FILTERS BACK-AND-FORTH
      *         CONVERSATION'.
      *       - IT IS *EXTREMELY* CONFUSING TO HANDLE *TWO* PSEUDO
      *         CONVERSATIONS IN THE *SAME* PROGRAM (A POOR DESIGN
      *         CHOICE IN MY OPINION).
      *       - IF THERE REALLY WAS A NEED FOR A PSEUDO-CONVERSATIONAL
      *         FILTERS MAP, THEN I WOULD HAVE DONE IT AS A SEPARATE
      *         PROGRAM AND CALLED IT FROM HERE VIA 'LINK' & CONTAINER.
      *       - DESPITE BEING FULLY CONVERSATIONL, THE MAP MAY/WILL
      *         RE-DISPLAY ON LOOP UNTIL THE USER EITHER HITS 'ENTER'
      *         TO MOVE FORWARD OR EXITS VIA DESIGNATED <PF> KEYS.

           INITIALIZE EFILMO.

           MOVE EIBTRNID TO TRANFLO.

      *    IF THIS IS THE FIRST INVOCATION OF THE PARAGRAPH, IE.
      *    FIRST STEP IN THE CONVERSATION, WE SET A DEFAULT SELECT 
      *    ORDER AND ALSO DISPLAY A MESSAGE TO THE USER.
           IF LST-NO-FILTERS-SET THEN
              MOVE '1' TO KEYSELO
              MOVE WS-FILTERS-MSG-SF TO MESSFLO
              MOVE DFHTURQ TO MESSFLC
           END-IF.

      *    IF THIS IS A RE-RENDER OF THE FILTERS SCREEN, IE. BY
      *    PRESSING THE PF3 KEY ON THE LISTING PAGE, WE RESTORE THE MAP
      *    FIELDS TO THE LAST ENTERED SET OF VALUES.
           IF LST-FILTERS-SET THEN
              PERFORM 3600-LOAD-FILTER-CRITERIA
           END-IF.

      *    IF AN INVALID KEY WAS PRESEED ON THE PREVOUS MAP DISPLAY,
      *    WE ISSUE A WARNING MESSAGE ON NEXT RENDER.
           IF WS-ACTION-INVALID THEN
              MOVE WS-MESSAGE TO MESSFLO
              MOVE DFHPINK TO MESSFLC
           END-IF.

      *    WE RENDER THE INITIAL FILTER MAP.
           EXEC CICS SEND
                MAP(APP-FILTERS-MAP-NAME)
                MAPSET(APP-LIST-MAPSET-NAME)
                FROM (EFILMO)
                ERASE
                FREEKB
                END-EXEC.

      *    <<<<<     PROGRAM EXECUTION HALTS HERE    >>>>>

      *    AND WAIT FOR THE USER TO ENTER FILTER CRITERIA.
      *    (EXECUTION HALTS HERE UNTIL THE USER HITS AN APPROPIATE
      *    AID KEY LIKE 'ENTER' OR 'PF3' OR 'PF12' ETC.)
           EXEC CICS RECEIVE
                MAP(APP-FILTERS-MAP-NAME)
                MAPSET(APP-LIST-MAPSET-NAME)
                INTO (EFILMI)
                END-EXEC.

      *    <<<<<    PROGRAM EXECUTION RESUMES HERE   >>>>>

           EVALUATE EIBAID
           WHEN DFHENTER
                MOVE 'Filter Criteria Entered' TO WS-MESSAGE
                SET WS-ACTION-DISPLAY TO TRUE
           WHEN DFHPF3
                MOVE 'Filter Criteria Cancelled' TO WS-MESSAGE
                SET WS-ACTION-EXIT TO TRUE
                PERFORM 9200-SIGN-USER-OFF
           WHEN DFHPF10
                MOVE 'Sign Off Requested' TO WS-MESSAGE
                SET WS-ACTION-SIGN-OFF TO TRUE
                PERFORM 9200-SIGN-USER-OFF
           WHEN DFHPF12
                MOVE 'Clear Criteria Requested' TO WS-MESSAGE
                SET WS-ACTION-CLEAR TO TRUE
                INITIALIZE LST-FILTERS
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
                SET WS-ACTION-INVALID TO TRUE
           END-EVALUATE.

      *    WITH FILTER CRITERIA ENTERED AND RECEIVED INTO THE MAP'S
      *    INPUT SECTION, WE PASS THE DATA TO THE CONTAINER AND THEN
      *    PROCEED INTO THE FILE ACCESS LOGIC.
           PERFORM 3100-SAVE-FILTER-CRITERIA.

       3100-SAVE-FILTER-CRITERIA.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3100-SAVE-FILTER-CRITERIA' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF KEYSELI IS NOT EQUAL TO LOW-VALUE THEN
              MOVE KEYSELI TO LST-SELECT-KEY-TYPE
              SET LST-FILTERS-SET TO TRUE
           END-IF.

           IF MATCHI IS NOT EQUAL TO LOW-VALUE THEN
              MOVE MATCHI TO LST-SELECT-KEY-VALUE
              SET LST-FILTERS-SET TO TRUE
           END-IF.

           PERFORM VARYING WS-INDEX
              FROM 1 BY 1
              UNTIL WS-INDEX IS GREATER THAN 4
                   IF DPTINCLI(WS-INDEX) IS NOT EQUAL TO LOW-VALUE
                      MOVE DPTINCLI(WS-INDEX)
                         TO LST-INCL-DEPT-ID(WS-INDEX)
                      SET LST-FILTERS-SET TO TRUE
                   END-IF
           END-PERFORM.

           PERFORM VARYING WS-INDEX
              FROM 1 BY 1
              UNTIL WS-INDEX IS GREATER THAN 4
                   IF DPTEXCLI(WS-INDEX) IS NOT EQUAL TO LOW-VALUE
                      MOVE DPTEXCLI(WS-INDEX)
                         TO LST-EXCL-DEPT-ID(WS-INDEX)
                      SET LST-FILTERS-SET TO TRUE
                   END-IF
           END-PERFORM.

           IF EDATEAI IS NOT EQUAL TO LOW-VALUE THEN
              MOVE EDATEAI TO LST-EMPL-DATE-AFTER
              SET LST-FILTERS-SET TO TRUE
           END-IF.

           IF EDATEBI IS NOT EQUAL TO LOW-VALUE THEN
              MOVE EDATEBI TO LST-EMPL-DATE-BEFORE
              SET LST-FILTERS-SET TO TRUE
           END-IF.

      *    >>> DEBUGGING ONLY <<<
           MOVE LST-FILTERS(01:45) TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    MOVE LST-FILTERS(46:45) TO WS-DEBUG-AID.
      *    PERFORM 9300-DEBUG-AID.
      *    MOVE LST-FILTERS(91:22) TO WS-DEBUG-AID.
      *    PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

       3200-APPLY-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3200-APPLY-FILTERS' TO WS-DEBUG-AID.
      *    PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    FILTER LOGIC.
      *
      *       - UNLIKE THE PLURALSIGHT EXAMPLE, HERE THE FILTERS ARE
      *         'WILDCARDED' BY DEFAULT, MEANING THE FILTER STRINGS
      *         ENTERED BY THE USER WILL BE LOOKED FOR BY THE
      *         'INSPECT' COMMAND ON ANY POSITION OF THE ID OR NAME
      *         FIELDS.
      *
      *       - THIS MEANS A FILTER OF '15' ON ID WILL GET EMPLOYEES
      *         '15', '115', '159', '315', '515300', '1571' AND SO ON.
      *
      *       - SAME WITH NAMES, A FILTER OF 'mar' WILL GET EMPLOYEES
      *         NAMED 'MARIA', 'LAMARR', 'MARSHALL' AND SO ON.
      *
      *       - I MADE THE NAME CHECKING CASE-INSENSITIVE, MEANING
      *         'MAR' AND 'mar' WILL YIELD THE SAME RESULTS.
      *
      *       - NO USE OF ACTUAL WILDCARDS LIKE '*' IS NEEDED, AS
      *         THE 'INSPECT' COMMAND WILL DO THE JOB FOR US!
      *
      *       - I COULD HAVE IMPLEMENTED AN ACTUAL WILDCARD USAGE
      *         AND/OR AN 'EXACT MATCH' OPTION, BUT I WOULD RATHER
      *         FOCUS ON SPECIFIC 'CICS' STUFF AND JUST LEAVE THE
      *         'MOST USEFUL' FILTER SCENARIO BY DEFAULT.

           INITIALIZE WS-FILTER-FLAGS. 

      *    IF NO FILTERS WERE SET, THEN WE JUST 'OK' THE RECORD.
           IF LST-NO-FILTERS-SET THEN
              SET WS-FILTERS-PASSED TO TRUE
              EXIT PARAGRAPH 
           END-IF.

      *    IF FILTERS WERE SET, THEN WE CHECK THEM ALL.
           PERFORM 3300-APPLY-KEY-FILTERS.
           PERFORM 3400-APPLY-DEPT-FILTERS.
           PERFORM 3500-APPLY-DATE-FILTERS.

      *    IF *ALL* FILTERS WERE MET, THEN WE SET THE 'PASSED' FLAG.
           IF WS-KEY-FILTER-PASSED AND
              WS-DEPT-FILTER-PASSED AND
              WS-DATE-FILTER-PASSED THEN
              SET WS-FILTERS-PASSED TO TRUE
           END-IF.

       3300-APPLY-KEY-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3300-APPLY-KEY-FILTERS' TO WS-DEBUG-AID.
      *    PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF 'VALUE' WAS OMITTED, WE IGNORE THE FILTER.
           IF LST-SELECT-KEY-VALUE IS EQUAL TO SPACES THEN
              SET WS-KEY-FILTER-PASSED TO TRUE
              EXIT PARAGRAPH
           END-IF.

      *    OTHERWISE, WE CHECK THE KEY FILTERS.

      *    IF 'KEY' WAS OMITTED BUT WE GOT A 'VALUE', THEN WE GUESS THE 
      *    KEY FROM THE VALUE!
           IF LST-SELECT-KEY-TYPE IS EQUAL TO SPACES AND 
              LST-SELECT-KEY-VALUE IS NOT EQUAL TO SPACES THEN
              IF FUNCTION TRIM(LST-SELECT-KEY-VALUE) IS NUMERIC THEN
                 MOVE '1' TO LST-SELECT-KEY-TYPE
              ELSE
                 MOVE '2' TO LST-SELECT-KEY-TYPE
              END-IF
           END-IF.

      *    SELECT OPTION '1' -> 'EMPLOYEE ID' FILTER.
           IF LST-SEL-BY-EMPLOYEE-ID THEN
              INITIALIZE WS-INSP-COUNTER

              INSPECT EMP-KEY
                 TALLYING WS-INSP-COUNTER
                 FOR ALL FUNCTION TRIM(LST-SELECT-KEY-VALUE)

              IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
                 SET WS-KEY-FILTER-PASSED TO TRUE
              END-IF
           END-IF.

      *    SELECT OPTION '2' -> 'EMPLOYEE NAME' FILTER.
           IF LST-SEL-BY-EMPLOYEE-NAME THEN
              INITIALIZE WS-INSP-COUNTER

              INSPECT FUNCTION UPPER-CASE(EMP-PRIMARY-NAME)
                 TALLYING WS-INSP-COUNTER
                 FOR ALL FUNCTION TRIM(LST-SELECT-KEY-VALUE)

              IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
                 SET WS-KEY-FILTER-PASSED TO TRUE
              END-IF
           END-IF.

       3400-APPLY-DEPT-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3400-APPLY-DEPT-FILTERS' TO WS-DEBUG-AID.
      *    PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF NO DEPARTMENT FILTERS WERE SET, WE JUST 'OK' IT.
           IF LST-INCLUDE-DEPT-FILTERS IS EQUAL TO SPACES AND
              LST-EXCLUDE-DEPT-FILTERS IS EQUAL TO SPACES THEN
              SET WS-DEPT-FILTER-PASSED TO TRUE
              EXIT PARAGRAPH
           END-IF.

      *    OTHERWISE, WE CHECK THE DEPARTMENT FILTERS.
           MOVE EMP-DEPARTMENT-ID TO WS-DEPT-KEY.

      *    FIRST, THE 'POSITIVE' DEPARTMENT INCLUSION FILTERS.
           IF LST-INCLUDE-DEPT-FILTERS IS EQUAL TO SPACES THEN
      *       NO 'INCLUDE' FILTERS, SO *ALL* DEPARTMENTS ARE FINE.
              SET WS-DEPT-FILTER-PASSED TO TRUE
           ELSE
      *       WE NEED TO MATCH A 'WHITE-LISTED' DEPARTMENT TO PASS.
              PERFORM VARYING LST-IN-DEPT-INDEX
                 FROM 1 BY 1
                 UNTIL LST-IN-DEPT-INDEX IS GREATER THAN 4
                 OR WS-DEPT-FILTER-PASSED
                      IF LST-INCL-DEPT-ID(LST-IN-DEPT-INDEX)
                         IS NOT EQUAL TO SPACES THEN
                         
                         INITIALIZE WS-INSP-COUNTER

                         INSPECT WS-DEPT-KEY
                            TALLYING WS-INSP-COUNTER
                            FOR ALL FUNCTION TRIM
                            (LST-INCL-DEPT-ID(LST-IN-DEPT-INDEX))

                         IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
      *                     SUCCESS! IT PASSES THE FILTER.
                            SET WS-DEPT-FILTER-PASSED TO TRUE
                         END-IF
                      END-IF
              END-PERFORM
           END-IF.

      *    SECOND, THE 'NEGATIVE' DEPARTMENT EXCLUSION FILTERS.
           IF LST-EXCLUDE-DEPT-FILTERS IS EQUAL TO SPACES THEN
      *       NO 'EXCLUDE' FILTERS, SO *NO* DEPARTMENTS ARE OFF.
      *       WE MANTAIN THE STATUS QUO (AS IN THE 'INCLUDE' OUTCOME)
              CONTINUE
           ELSE
      *       WE NEED TO AVOID ALL 'BLACK-LISTED' DEPARTMENTS TO PASS.
              PERFORM VARYING LST-EX-DEPT-INDEX
                 FROM 1 BY 1
                 UNTIL LST-EX-DEPT-INDEX IS GREATER THAN 4
                 OR WS-DEPT-FILTER-FAILED
                      IF LST-EXCL-DEPT-ID(LST-EX-DEPT-INDEX)
                         IS NOT EQUAL TO SPACES THEN

                         INITIALIZE WS-INSP-COUNTER

                         INSPECT WS-DEPT-KEY
                            TALLYING WS-INSP-COUNTER
                            FOR ALL FUNCTION TRIM
                            (LST-EXCL-DEPT-ID(LST-EX-DEPT-INDEX))

                         IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
      *                     BLACKLISTED! IT DOESN'T MAKE THE CUT.
                            SET WS-DEPT-FILTER-FAILED TO TRUE
                         END-IF
                      END-IF
              END-PERFORM
           END-IF.

       3500-APPLY-DATE-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3500-APPLY-DATE-FILTERS' TO WS-DEBUG-AID.
      *    PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF NO DATE FILTERS WERE SET, WE JUST 'OK' IT AND RETURN
           IF LST-EMPLOYMENT-DATE-FILTERS IS EQUAL TO SPACES THEN
              SET WS-DATE-FILTER-PASSED TO TRUE
              EXIT PARAGRAPH
           END-IF.

      *    OTHERWISE, WE CHECK THE DATE FILTERS.

      *    IF BOTH FILTERS WERE SET, WE CHECK THE EMPLOYEE START DATE
      *    AGAINST THE FILTERS.
           IF LST-EMPL-DATE-AFTER IS NOT EQUAL TO SPACES AND
              LST-EMPL-DATE-BEFORE IS NOT EQUAL TO SPACES THEN
              IF EMP-START-DATE IS GREATER THAN LST-EMPL-DATE-AFTER AND
                 EMP-START-DATE IS LESS THAN LST-EMPL-DATE-BEFORE THEN
      *          SUCCESS!
                 SET WS-DATE-FILTER-PASSED TO TRUE
                 EXIT PARAGRAPH
              END-IF
           END-IF.

      *    IF ONLY DATE-BEFORE FILTER WAS SET.
           IF LST-EMPL-DATE-AFTER IS EQUAL TO SPACES THEN
              IF EMP-START-DATE IS LESS THAN LST-EMPL-DATE-BEFORE THEN
      *          SUCCESS!
                 SET WS-DATE-FILTER-PASSED TO TRUE
                 EXIT PARAGRAPH
              END-IF
           END-IF.

      *    IF ONLY DATE-AFTER FILTER WAS SET.
           IF LST-EMPL-DATE-BEFORE IS EQUAL TO SPACES THEN
              IF EMP-START-DATE IS GREATER THAN LST-EMPL-DATE-AFTER THEN
      *          SUCCESS!
                 SET WS-DATE-FILTER-PASSED TO TRUE
                 EXIT PARAGRAPH
              END-IF
           END-IF.

       3600-LOAD-FILTER-CRITERIA.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3600-LOAD-FILTER-CRITERIA' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    HERE WE PRE-POPULATE THE FILTER MAP WITH THE LAST VALUES
      *    ENTERED BY THE USER AND SAVED IN THE CONTAINER.
      *
      *    WHEN THE USER SEES THE RE-DISPLAYED FILTERS SCREEN AND
      *    MAKES CHANGES TO THEM, *ONLY* THE NEW OR MODIFIED
      *    VALUES WILL BE PASSED BACK BY THE 'RECEIVE' COMMAND.
      *
      *    VALUES THAT WERE RE-DISPLAYED BUT *NOT* MODIFIED WILL NOT
      *    BE PASSED BACK, BUT THAT WILL NOT BE A PROBLEM SINCE WE
      *    ALREADY HAVE THEM STORED IN THE CONTAINER!
      *
      *    THEREFORE:
      *
      *       - UPON RETURN FROM THE EDIT FILTERS SCREEN, WE JUST
      *         UPDATE THE CONTAINER WITH THE NEW OR MODIFIED VALUES
      *         THAT WERE *INDEED* PASSED BACK TO US.
      *       - THE ONES THAT WERE *NOT* MODIFIED WILL STAY IN THE
      *         CONTAINER 'AS ARE' AND WILL TOO BE USED FOR THE NEXT
      *         RENDERING OF THE LISTING PAGE.
      *
      *    THUS, I COMPLETELY AVOIDED PLURALSIGHT'S PRACTICE OF SETTING
      *    UP 'MODIFIED FIELD FLAGS' AS I SAW REALLY NO NEED TO DO SO
      *    (DUE TO KEEPING STATE IN THE CONTAINER!)

           INITIALIZE EFILMO.

           MOVE LST-SELECT-KEY-TYPE TO KEYSELO.
           MOVE LST-SELECT-KEY-VALUE TO MATCHO.

           PERFORM VARYING WS-INDEX
              FROM 1 BY 1
              UNTIL WS-INDEX IS GREATER THAN 4
                   MOVE LST-INCL-DEPT-ID(WS-INDEX)
                      TO DPTINCLO(WS-INDEX)
                   MOVE LST-EXCL-DEPT-ID(WS-INDEX)
                      TO DPTEXCLO(WS-INDEX)
           END-PERFORM.

           MOVE LST-EMPL-DATE-AFTER TO EDATEAO.
           MOVE LST-EMPL-DATE-BEFORE TO EDATEBO.

           MOVE WS-FILTERS-MSG-EF TO MESSFLO.
           MOVE DFHTURQ TO MESSFLC.


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

      *    DISPLAY FILTERS LINE.
           PERFORM 9110-SET-FILTERS-LINE.

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

       9110-SET-FILTERS-LINE.
      *    IF NO FILTERS WERE SET, WE JUST DISPLAY A DEFAULT MESSAGE.
           IF LST-NO-FILTERS-SET THEN
              MOVE '(No Filters Set)' TO FLTRSO
              EXIT PARAGRAPH
           END-IF.

      *    OTHERWISE, WE DISPLAY FEEBACK ABOUT FILTERS SET BY THE USER.
           IF LST-SEL-BY-EMPLOYEE-ID THEN
              STRING 'Employee ID Contains "'
                     FUNCTION TRIM(LST-SELECT-KEY-VALUE)
                     '"'
                 DELIMITED BY SIZE INTO FLTRSO
              END-STRING
              EXIT PARAGRAPH
           END-IF.

           IF LST-SEL-BY-EMPLOYEE-NAME THEN
              STRING 'Employee Name Contains "'
                     FUNCTION TRIM(LST-SELECT-KEY-VALUE)
                     '"'
                 DELIMITED BY SIZE INTO FLTRSO
              END-STRING
              EXIT
           END-IF.

           IF LST-INCLUDE-DEPT-FILTERS IS NOT EQUAL TO SPACES THEN
              STRING 'Include Departments: '
                     FUNCTION TRIM(LST-INCLUDE-DEPT-FILTERS)
                 DELIMITED BY SIZE INTO FLTRSO
              END-STRING
              EXIT PARAGRAPH
           END-IF.

           IF LST-EXCLUDE-DEPT-FILTERS IS NOT EQUAL TO SPACES THEN
              STRING 'Exclude Departments: '
                     FUNCTION TRIM(LST-EXCLUDE-DEPT-FILTERS)
                 DELIMITED BY SIZE INTO FLTRSO
              END-STRING
              EXIT PARAGRAPH
           END-IF.

           IF LST-EMPL-DATE-AFTER IS NOT EQUAL TO SPACES THEN
              STRING 'Employment Date After: '
                     FUNCTION TRIM(LST-EMPL-DATE-AFTER)
                 DELIMITED BY SIZE INTO FLTRSO
              END-STRING
              EXIT PARAGRAPH
           END-IF.

           IF LST-EMPL-DATE-BEFORE IS NOT EQUAL TO SPACES THEN
              STRING 'Employement Date Before: '
                     FUNCTION TRIM(LST-EMPL-DATE-BEFORE)
                 DELIMITED BY SIZE INTO FLTRSO
              END-STRING
           END-IF.

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