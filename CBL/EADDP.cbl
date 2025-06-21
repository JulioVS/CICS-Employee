       IDENTIFICATION DIVISION.
       PROGRAM-ID. EADDP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'ADD EMPLOYEE' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - ADD CONTAINER.
      *      - ADD MAPSET.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EADDCTR.
       COPY EADDMAP.
       COPY EMPMAST.
       COPY EMONCTR.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE    PIC S9(8) USAGE IS BINARY.
          05 WS-MESSAGE          PIC X(79).
          05 WS-NEW-EMPLOYEE-ID  PIC 9(8).
      *
       01 WS-VALIDATION-FLAG     PIC X(1)  VALUE SPACES.
          88 VALIDATION-PASSED             VALUE 'Y'.
          88 VALIDATION-FAILED             VALUE SPACES.
      *
       01 WS-FILE-FLAG           PIC X(1)  VALUE SPACES.
          88 END-OF-FILE                   VALUE 'E'.
          88 TOP-OF-FILE                   VALUE 'T'.
          88 RECORD-FOUND                  VALUE 'R'.
      *
       01 WS-DATE-FORMATTING.
          05 WS-INPUT-DATE.
             10 WS-YYYY          PIC X(4)  VALUE SPACES.
             10 WS-MM            PIC X(2)  VALUE SPACES.
             10 WS-DD            PIC X(2)  VALUE SPACES.
          05 WS-OUTPUT-DATE.
             10 WS-YYYY          PIC X(4)  VALUE SPACES.
             10 FILLER           PIC X(1)  VALUE '-'.
             10 WS-MM            PIC X(2)  VALUE SPACES.
             10 FILLER           PIC X(1)  VALUE '-'.
             10 WS-DD            PIC X(2)  VALUE SPACES.
      *
       01 WS-DEBUG-AID           PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER              PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT       PIC X(45) VALUE SPACES.
          05 FILLER              PIC X(1)  VALUE '>'.
          05 FILLER              PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP    PIC 9(8)  VALUE ZEROES.
          05 FILLER              PIC X(1)  VALUE '>'.
          05 FILLER              PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2   PIC 9(8)  VALUE ZEROES.
          05 FILLER              PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE          PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                VALUE 'Y'.
          88 NOT-DEBUGGING                 VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS GET
                CONTAINER(APP-ADD-CONTAINER-NAME)
                CHANNEL(APP-ADD-CHANNEL-NAME)
                INTO (ADD-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
      *         1ST INTERACTION -> NO CONTAINER YET (CREATE IT)
                PERFORM 1000-FIRST-INTERACTION
           WHEN DFHRESP(NORMAL)
      *         NEXT INTERACTIONS -> CONTAINER FOUND (CONTINUE)
                PERFORM 2000-PROCESS-USER-INPUT
           WHEN OTHER
                MOVE 'Error Retrieving Add Container!' TO WS-MESSAGE
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

           PERFORM 1100-INITIALIZE.

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

           MOVE MON-USER-ID TO ADD-USER-ID.

       1100-INITIALIZE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE ADD-EMPLOYEE-CONTAINER.
           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE WS-WORKING-VARS.
           INITIALIZE EADDMO.

           MOVE 'Welcome to the Add New Record page!' TO WS-MESSAGE.
           MOVE -1 TO PRNAMEL. 

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE 'So Far, So Good...' TO WS-MESSAGE.
           
           EXEC CICS RECEIVE
                MAP(APP-ADD-MAP-NAME)
                MAPSET(APP-ADD-MAPSET-NAME)
                INTO (EADDMI)
                END-EXEC.

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

           EVALUATE EIBAID
           WHEN DFHENTER
                PERFORM 2100-VALIDATE-USER-INPUT
           WHEN DFHPF3
                PERFORM 2200-ADD-EMPLOYEE-RECORD
                PERFORM 2300-TRANSFER-BACK-TO-MENU
           WHEN DFHPF4
                PERFORM 2200-ADD-EMPLOYEE-RECORD
                PERFORM 2600-CLEAR-SCREEN
           WHEN DFHPF9
                PERFORM 2600-CLEAR-SCREEN
           WHEN DFHPF10
                PERFORM 2500-SIGN-USER-OFF
           WHEN DFHPF12
                PERFORM 2300-TRANSFER-BACK-TO-MENU
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
           END-EVALUATE.

       2100-VALIDATE-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-VALIDATE-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    RESTORE LAST SAVED AND VAILDATED DATA FROM CONTAINER.
           MOVE ADD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

      *    GET NEWLY ENTERED FIELDS AND UPDATE THE RECORD.
           IF PRNAMEL IS GREATER THAN ZERO THEN
              MOVE PRNAMEI TO EMP-PRIMARY-NAME
           END-IF.
           IF HONORL IS GREATER THAN ZERO THEN
              MOVE HONORI TO EMP-HONORIFIC
           END-IF.
           IF SHNAMEL IS GREATER THAN ZERO THEN
              MOVE SHNAMEI TO EMP-SHORT-NAME
           END-IF.
           IF FLNAMEL IS GREATER THAN ZERO THEN
              MOVE FLNAMEI TO EMP-FULL-NAME
           END-IF.
           IF JBTITLL IS GREATER THAN ZERO THEN
              MOVE JBTITLI TO EMP-JOB-TITLE
           END-IF.

           IF DEPTIDL IS GREATER THAN ZERO THEN
              EXEC CICS BIF DEEDIT
                   FIELD(DEPTIDI)
                   LENGTH(LENGTH OF DEPTIDI)
                   END-EXEC
              MOVE DEPTIDI TO EMP-DEPARTMENT-ID
           END-IF.
           
           IF STDATEL IS GREATER THAN ZERO THEN
              EXEC CICS BIF DEEDIT
                   FIELD(STDATEI)
                   LENGTH(LENGTH OF STDATEI)
                   END-EXEC
      *       SINCE WE HAVE A 10-CHAR DATE FIELD ON THE MAP, THE DEEDIT
      *       COMMAND WILL PAD THE FIELD WITH LEFTMOST ZEROES AS IN
      *       '00YYYYMMDD' SO WE NEED TO TRIM IT DOWN TO 'YYYYMMDD'
      *       TO FIT ON THE MASTER FILE'S 8-CHAR DATE FIELD.
              MOVE STDATEI(3:8) TO EMP-START-DATE
           END-IF.

      *    SAVE UPDATED RECORD BACK TO THE CONTAINER.
           MOVE EMPLOYEE-MASTER-RECORD TO ADD-EMPLOYEE-RECORD.

      *    VALIDATE FIELDS.
      *      - TO SET THE CURSOR POSITION ON THE MAP, WE MOVE -1 TO
      *        THE LENGTH OF THE FIELD THAT IS INVALID *AND* WE ADD
      *        THE "CURSOR" OPTION ON THE 'CICS SEND MAP' COMMAND.
      *      - IF ALL IS WELL, WE POSITION IT AT THE FIRST EDITABLE 
      *        FIELD, WHICH IS 'PRIMARY NAME', TO PREVENT THE CURSOR TO 
      *        SHOW UP AT "0,0" POSITION ON THE SCREEN.

           INITIALIZE WS-VALIDATION-FLAG.

           EVALUATE TRUE
           WHEN EMP-PRIMARY-NAME IS EQUAL TO SPACES
                MOVE 'Validation Error: Primary Name is required!'
                   TO WS-MESSAGE
                MOVE -1 TO PRNAMEL
                EXIT
           WHEN EMP-FULL-NAME IS EQUAL TO SPACES
                MOVE 'Validation Error: Full Name is required!'
                   TO WS-MESSAGE
                MOVE -1 TO FLNAMEL
                EXIT
           WHEN EMP-JOB-TITLE IS EQUAL TO SPACES
                MOVE 'Validation Error: Job Title is required!'
                   TO WS-MESSAGE
                MOVE -1 TO JBTITLL
                EXIT
           WHEN EMP-START-DATE IS EQUAL TO SPACES
                MOVE 'Validation Error: Start Date is required!'
                   TO WS-MESSAGE
                MOVE -1 TO STDATEL
                EXIT
           WHEN OTHER
                MOVE 'Employee Record Validated Successfully!'
                   TO WS-MESSAGE 
                MOVE -1 TO PRNAMEL
                SET VALIDATION-PASSED TO TRUE
           END-EVALUATE.

       2200-ADD-EMPLOYEE-RECORD.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-ADD-EMPLOYEE-RECORD' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 2100-VALIDATE-USER-INPUT.

           IF VALIDATION-PASSED THEN
              MOVE 'Adding New Employee Record...' TO WS-MESSAGE
              PERFORM 3000-WRITE-NEW-RECORD
           END-IF.

       2300-TRANSFER-BACK-TO-MENU.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2300-TRANSFER-BACK-TO-MENU' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    RESET THIS CONVERSATION BY DELETING CURRENT CONTAINER.
           PERFORM 2400-DELETE-ADD-CONTAINER.

           EXEC CICS XCTL
                PROGRAM(APP-MENU-PROGRAM-NAME)
                CHANNEL(APP-MENU-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Transferring Back To Menu' TO WS-MESSAGE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN DFHRESP(PGMIDERR)
                MOVE 'Menu Program Not Found!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE 'Error Transferring To Menu!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       2400-DELETE-ADD-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2400-DELETE-ADD-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS DELETE
                CONTAINER(APP-ADD-CONTAINER-NAME)
                CHANNEL(APP-ADD-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
                MOVE 'Add Container Not Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Deleting Add Container!' TO WS-MESSAGE
           END-EVALUATE.

       2500-SIGN-USER-OFF.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2500-SIGN-USER-OFF' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    >>> CALL ACTIVITY MONITOR <<<
           SET MON-AC-SIGN-OFF TO TRUE.
           PERFORM 4200-CALL-ACTIVITY-MONITOR.
      *    >>> --------------------- <<<

           PERFORM 9200-RETURN-TO-CICS.

       2600-CLEAR-SCREEN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2600-CLEAR-SCREEN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE ADD-EMPLOYEE-RECORD.
           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE EADDMO.
           MOVE -1 TO PRNAMEL. 

      *-----------------------------------------------------------------
       WRITING SECTION.
      *-----------------------------------------------------------------

       3000-WRITE-NEW-RECORD.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3000-WRITE-NEW-RECORD' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 3100-GET-NEW-EMPLOYEE-ID.

       3100-GET-NEW-EMPLOYEE-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3100-GET-NEW-EMPLOYEE-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 3110-START-BROWSING.

           IF END-OF-FILE THEN
              MOVE 1 TO WS-NEW-EMPLOYEE-ID
           END-IF.

           IF NOT END-OF-FILE THEN 
              PERFORM 3120-READ-PREV-RECORD
              PERFORM 3130-END-BROWSING
           END-IF.

           IF RECORD-FOUND THEN
              ADD 1 TO EMP-EMPLOYEE-ID 
              MOVE EMP-EMPLOYEE-ID TO WS-NEW-EMPLOYEE-ID
           ELSE
              MOVE 1 TO WS-NEW-EMPLOYEE-ID
           END-IF.

      *    >>> DEBUGGING ONLY <<<
           SET I-AM-DEBUGGING TO TRUE.
           MOVE WS-NEW-EMPLOYEE-ID TO WS-DEBUG-AID. 
           PERFORM 9300-DEBUG-AID.
           SET NOT-DEBUGGING TO TRUE.
      *    >>> -------------- <<<
               
       3110-START-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3110-START-BROWSING' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE HIGH-VALUES TO EMP-KEY.
           INITIALIZE WS-FILE-FLAG.

           EXEC CICS STARTBR
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET END-OF-FILE TO TRUE
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

       3120-READ-PREV-RECORD.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3120-READ-PREV-RECORD' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE WS-FILE-FLAG.

           EXEC CICS READPREV
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                INTO (EMPLOYEE-MASTER-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                SET RECORD-FOUND TO TRUE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Previous Records Found!' TO WS-MESSAGE
                SET TOP-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'Start of Employee Master File' TO WS-MESSAGE
                SET TOP-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Previous Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       3130-END-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3130-END-BROWSING' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS ENDBR
                FILE(APP-EMP-MASTER-FILE-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.
\
           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (End Browse)!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Master File Not Open!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE 'Error Ending Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.
             
      *-----------------------------------------------------------------
       ACTIVITY-MONITOR SECTION.
      *-----------------------------------------------------------------

       4000-CHECK-USER-STATUS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4000-CHECK-USER-STATUS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CHECK IF THE USER IS ALREADY SIGNED ON TO THE ACTIVITY
           PERFORM 4100-GET-MONITOR-CONTAINER.

      *    IF THE USER IS SIGNED ON, CHECK IF SESSION IS STILL ACTIVE.
           SET MON-AC-APP-FUNCTION TO TRUE.
           PERFORM 4200-CALL-ACTIVITY-MONITOR.

       4100-GET-MONITOR-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4100-GET-MONITOR-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

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
                MOVE 'No Activity Monitor Data Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Getting Activity Monitor!' TO WS-MESSAGE
           END-EVALUATE.

       4200-CALL-ACTIVITY-MONITOR.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4200-CALL-ACTIVITY-MONITOR' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PUT CONTAINER AND LINK TO ACTIVITY MONITOR PROGRAM
           MOVE APP-ADD-PROGRAM-NAME TO MON-LINKING-PROGRAM.
           INITIALIZE MON-RESPONSE.

           PERFORM 4300-PUT-MONITOR-CONTAINER.

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
                MOVE 'Activity Monitor Program Not Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Linking to Activity Monitor!' TO WS-MESSAGE
           END-EVALUATE.

       4300-PUT-MONITOR-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '4300-PUT-MONITOR-CONTAINER' TO WS-DEBUG-AID.
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
                MOVE 'Error Putting Activity Monitor!' TO WS-MESSAGE
           END-EVALUATE.

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-SEND-MAP-AND-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-SEND-MAP-AND-RETURN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 9100-POPULATE-MAP.
           PERFORM 9150-PUT-ADD-CONTAINER.

           EXEC CICS SEND
                MAP(APP-ADD-MAP-NAME)
                MAPSET(APP-ADD-MAPSET-NAME)
                FROM (EADDMO)
                ERASE
                CURSOR 
                END-EXEC.

           EXEC CICS RETURN
                CHANNEL(APP-ADD-CHANNEL-NAME)
                TRANSID(APP-ADD-TRANSACTION-ID)
                END-EXEC.

       9100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-POPULATE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    INITIALIZE EADDMO.

           MOVE EIBTRNID TO TRANIDO.

           IF ADD-USER-ID IS NOT EQUAL TO SPACES THEN
              MOVE ADD-USER-ID TO LOGDINO
           ELSE
              MOVE '<Anonym>' TO LOGDINO
           END-IF.

           IF ADD-EMPLOYEE-RECORD IS NOT EQUAL TO SPACES THEN
              MOVE ADD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD 

              MOVE '00090125' TO EMPLIDO

              MOVE EMP-PRIMARY-NAME TO PRNAMEO
              MOVE EMP-HONORIFIC TO HONORO
              MOVE EMP-SHORT-NAME TO SHNAMEO
              MOVE EMP-FULL-NAME TO FLNAMEO

              MOVE EMP-JOB-TITLE TO JBTITLO
              MOVE '00005150' TO DEPTIDO
              MOVE 'World Domination HQ' TO DEPTNMO

              MOVE EMP-START-DATE TO WS-INPUT-DATE 
              MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE
              MOVE WS-OUTPUT-DATE TO STDATEO   
           END-IF.

           MOVE WS-MESSAGE TO MESSO.

           EVALUATE TRUE
           WHEN MESSO(1:7) IS EQUAL TO 'Welcome'
                MOVE DFHPINK TO MESSC
           WHEN MESSO(1:7) IS EQUAL TO 'Invalid'
                MOVE DFHYELLO TO MESSC
           WHEN MESSO(01:5) IS EQUAL TO 'Error'
           WHEN MESSO(12:5) IS EQUAL TO 'Error'
                MOVE DFHRED TO MESSC
           END-EVALUATE.

      *    SET ANY MODIFIED DATA TAG (MDT) 'ON' TO AVOID THE 'AEI9'
      *    ABEND THAT HAPPENS WHEN WE ONLY RECEIVE AN AID-KEY FROM THE
      *    MAP AND NO REAL DATA ALONG IT.
           MOVE DFHBMFSE TO TRANIDA.

       9150-PUT-ADD-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9150-PUT-LIST-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS PUT
                CONTAINER(APP-ADD-CONTAINER-NAME)
                CHANNEL(APP-ADD-CHANNEL-NAME)
                FROM (ADD-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Error Putting Add Container!' TO WS-MESSAGE
           END-EVALUATE.

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