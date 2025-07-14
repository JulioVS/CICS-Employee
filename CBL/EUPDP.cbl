       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUPDP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'UPDATE EMPLOYEE DETAILS' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - UPDATE EMPLOYEE MAPSET.
      *      - UPDATE EMPLOYEE CONTAINER.
      *      - EMPLOYEE DETAILS CONTAINER.
      *      - LIST CONTAINER.
      *      - ACTIVITY MONITOR CONTAINER.
      *      - EMPLOYEE MASTER RECORD.
      *      - REGISTERED USERS RECORD.
      *      - AUDIT TRAIL RECORD.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EUPDMAP.
       COPY EUPDCTR.
       COPY EDETCTR.
       COPY ELSTCTR.
       COPY EMONCTR.
       COPY EMPMAST.
       COPY EREGUSR.
       COPY EAUDIT.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE       PIC S9(8) USAGE IS BINARY.
          05 WS-EMPLOYEE-ID         PIC X(8) JUSTIFIED RIGHT.
          05 WS-INSP-COUNTER        PIC S9(2) USAGE IS BINARY.
          05 WS-DEPT-KEY            PIC X(8).
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-MESSAGE             PIC X(79) VALUE SPACES.
      *
       01 WS-DATE-FORMATTING.
          05 WS-INPUT-DATE.
             10 WS-YYYY             PIC X(4)  VALUE SPACES.
             10 WS-MM               PIC X(2)  VALUE SPACES.
             10 WS-DD               PIC X(2)  VALUE SPACES.
          05 WS-OUTPUT-DATE.
             10 WS-DD               PIC X(2)  VALUE SPACES.
             10 FILLER              PIC X(1)  VALUE '-'.
             10 WS-MM               PIC X(2)  VALUE SPACES.
             10 FILLER              PIC X(1)  VALUE '-'.
             10 WS-YYYY             PIC X(4)  VALUE SPACES.
      *
       01 WS-FILTER-FLAGS.
          03 WS-FILTERS-CHECK       PIC X(1)  VALUE SPACES.
             88 FILTERS-PASSED                VALUE 'Y'.
             88 FILTERS-FAILED                VALUE 'N'.
          03 WS-KEY-FILTER-CHECK    PIC X(1)  VALUE SPACES.
             88 KEY-FILTER-PASSED             VALUE 'Y'.
          03 WS-DEPT-FILTER-CHECK   PIC X(1)  VALUE SPACES.
             88 DEPT-FILTER-PASSED            VALUE 'Y'.
             88 DEPT-FILTER-FAILED            VALUE 'N'.
          03 WS-DATE-FILTER-CHECK   PIC X(1)  VALUE SPACES.
             88 DATE-FILTER-PASSED            VALUE 'Y'.
      *
       01 WS-USER-ID-FLAG           PIC X(1)  VALUE SPACES.
          88 USER-ID-INPUT                    VALUE 'Y'.
      *
       01 WS-VALIDATION-FLAG        PIC X(1)  VALUE SPACES.
          88 VALIDATION-PASSED                VALUE SPACES.
          88 VALIDATION-FAILED                VALUE 'F'.
          88 NO-CHANGES-MADE                  VALUE 'N'.
      *
       01 WS-PRIMARY-NAME-FLAG      PIC X(1)  VALUE SPACES.
          88 PRIMARY-NAME-VALID               VALUE 'Y'.
          88 PRIMARY-NAME-EXISTS              VALUE SPACES.
      *
       01 WS-FILE-FLAG              PIC X(1)  VALUE SPACES.
          88 END-OF-FILE                      VALUE 'E'.
          88 TOP-OF-FILE                      VALUE 'T'.
          88 RECORD-FOUND                     VALUE 'R'.
      *
       01 WS-UPDATE-FLAG            PIC X(1)  VALUE SPACES.
          88 AVAILABLE-FOR-UPDATE             VALUE 'Y'.
      *
       01 WS-DELETION-FLAG          PIC X(1)  VALUE SPACES.
          88 CONFIRM-DELETION                 VALUE 'Y'.
          88 CANCEL-DELETION                  VALUE 'N'.
      *
       01 WS-DEBUG-AID              PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER                 PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT          PIC X(45) VALUE SPACES.
          05 FILLER                 PIC X(1)  VALUE '>'.
          05 FILLER                 PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP       PIC 9(8)  VALUE ZEROES.
          05 FILLER                 PIC X(1)  VALUE '>'.
          05 FILLER                 PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2      PIC 9(8)  VALUE ZEROES.
          05 FILLER                 PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE             PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                   VALUE 'Y'.
          88 NOT-DEBUGGING                    VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PSEUDO-CONVERSATIONAL PROGRAM DESIGN.

      *    START BY GETTING THE 'UPDATE' CONTAINER:
      *
      *    - IF IT DOES NOT YET EXIST -> 1ST STEP IN CONVERSATION
      *    - IF IT DOES ALREADY EXIST -> CONVERSATION IN PROGRESS

           EXEC CICS GET
                CONTAINER(APP-UPDATE-CONTAINER-NAME)
                CHANNEL(APP-UPDATE-CHANNEL-NAME)
                INTO (UPDATE-EMPLOYEE-CONTAINER)
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
                MOVE 'Error Retrieving Update Container!' TO WS-MESSAGE
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

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

      *    IF WE HAVE A LOGGED-IN USER:
      *      - SAVE HIS DATA ON THE APPLICATION CONTAINER.
      *      - FIND HIS OWN 'EMPLOYEE ID'.
           IF MON-USER-ID IS NOT EQUAL TO SPACES THEN
              MOVE MON-USER-ID TO UPD-USER-ID REG-USER-ID
              MOVE MON-USER-CATEGORY TO UPD-USER-CATEGORY

              PERFORM 1600-LOOKUP-USER-ID
           END-IF.

      *    IF THE LOGGED-IN USER IS A MANAGER, FIND HIS DEPARTMENT ID.
           IF UPD-CT-MANAGER AND
              UPD-USER-EMP-ID IS NOT EQUAL TO ZERO THEN

              PERFORM 1700-GET-DEPARTMENT-ID
           END-IF.

      *    CHECK IF WE ARE COMING FROM THE 'EMPLOYEE DETAILS' VIEW AND
      *    IF SO, RETRIEVE THE SELECTED RECORD FOR DISPLAY.
           IF EIBTRNID IS EQUAL TO APP-VIEW-TRANSACTION-ID THEN
              PERFORM 3000-GET-VIEW-CONTAINER
              IF UPD-EMPLOYEE-RECORD IS NOT EQUAL TO SPACES THEN
                 EXIT PARAGRAPH
              END-IF
           END-IF.

      *    IF NOT, JUST READ THE FIRST EMPLOYEE RECORD.
           PERFORM 1300-READ-EMPLOYEE-BY-KEY.

       1100-INITIALIZE-VARIABLES.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1100-INITIALIZE-VARIABLES' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    CLEAR ALL RECORDS AND VARIABLES.
           INITIALIZE ACTIVITY-MONITOR-CONTAINER.
           INITIALIZE EMPLOYEE-DETAILS-CONTAINER.
           INITIALIZE UPDATE-EMPLOYEE-CONTAINER.
           INITIALIZE LIST-EMPLOYEE-CONTAINER.
           INITIALIZE EMPLOYEE-MASTER-RECORD.
           INITIALIZE REGISTERED-USER-RECORD.
           INITIALIZE AUDIT-TRAIL-RECORD.
           INITIALIZE WS-WORKING-VARS.

       1200-INITIALIZE-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1200-INITIALIZE-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SET INITIAL VALUES FOR 'UPDATE' CONTAINER.
           MOVE 'ANONYMUS' TO UPD-USER-ID.
           MOVE 'STD' TO UPD-USER-CATEGORY.
           MOVE '1' TO UPD-SELECT-KEY-TYPE.
           MOVE LOW-VALUE TO UPD-SELECT-KEY-VALUE.

      *    GET CALLING PROGRAM NAME FROM ITS TRANSACTION ID.
           EXEC CICS INQUIRE
                TRANSACTION(EIBTRNID)
                PROGRAM(UPD-CALLING-PROGRAM)
                END-EXEC.

      *    >>> ---IMPORTANT!--- <<<
      *    THE USE OF 'CICS INQUIRE' REQUIRED ADDING THE 'SP' OPTION
      *    IN THE TRANSLATOR OPTIONS OF THE COMPILING JCL!
      *    >>> ---------------- <<<

       1300-READ-EMPLOYEE-BY-KEY.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1300-READ-EMPLOYEE-BY-KEY' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1310-START-BROWSING.

           PERFORM 1320-READ-NEXT-RECORD
              UNTIL FILTERS-PASSED OR UPD-END-OF-FILE.

           IF FILTERS-PASSED THEN
              PERFORM 1325-COPY-INTO-CONTAINER
           END-IF.

           IF NOT UPD-END-OF-FILE THEN
              PERFORM 1330-END-BROWSING
           END-IF.

       1310-START-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1310-START-BROWSING' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF UPD-SEL-BY-EMPLOYEE-ID THEN
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
                CONTINUE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET UPD-END-OF-FILE TO TRUE
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
           MOVE '1320-READ-NEXT-RECORD' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF UPD-SEL-BY-EMPLOYEE-ID THEN
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
                PERFORM 3200-APPLY-FILTERS
                PERFORM 3700-CHECK-DELETION
           WHEN DFHRESP(NOTFND)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET UPD-END-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'End of Employee Master File' TO WS-MESSAGE
                SET UPD-END-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1330-END-BROWSING.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1330-END-BROWSING' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF UPD-SEL-BY-EMPLOYEE-ID THEN
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
                CONTINUE
           WHEN OTHER
                MOVE 'Error Ending Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1325-COPY-INTO-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1325-COPY-INTO-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    MOVE RECORD TO THE APPLICATION CONTAINER'S WORKING AREA.
      *    THIS WILL BE CHANGED EVERY TIME THE USER UPDATES FIELDS
      *    WHILE EDITING A SINGLE EMPLOYEE RECORD.
           MOVE EMPLOYEE-MASTER-RECORD TO UPD-EMPLOYEE-RECORD.

      *    THEN, ALSO SAVE A COPY OF THE ORIGINAL RECORD HERE.
      *    THIS WILL REMAIN UNCHANGED UNTIL THE USER EITHER COMMITS
      *    HIS CHANGES OR SWITCHES TO EDIT ANOTHER EMPLOYEE RECORD.
           MOVE EMPLOYEE-MASTER-RECORD TO UPD-ORIGINAL-RECORD.

      *    FINALLY, SAVE THE EMPLOYEE'S PRIMARY NAME IN THE
      *    CONTAINER AS AN AID TO VALIDATING CHANGES TO THIS
      *    ALTERNATE-KEY FIELD LATER ON.
           MOVE EMP-PRIMARY-NAME TO UPD-EMP-ALT-KEY.

       1400-READ-BACKWARDS-BY-KEY.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1400-READ-BACKWARDS-BY-KEY' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1310-START-BROWSING.

      *    <<< PATCH FOR BACKWARDS BROWSING BY NAME CASE >>>
           IF UPD-SEL-BY-EMPLOYEE-NAME THEN
              EXEC CICS READPREV
                   FILE(APP-EMP-MASTER-PATH-NAME)
                   RIDFLD(EMP-PRIMARY-NAME)
                   INTO (EMPLOYEE-MASTER-RECORD)
                   END-EXEC
           END-IF.
      *    <<< ----------------------------------------- >>>

           PERFORM 1410-READ-PREV-RECORD
              UNTIL FILTERS-PASSED OR UPD-TOP-OF-FILE.

           IF FILTERS-PASSED THEN
              PERFORM 1325-COPY-INTO-CONTAINER
           END-IF.

           IF NOT UPD-TOP-OF-FILE THEN
              PERFORM 1330-END-BROWSING
           END-IF.

       1410-READ-PREV-RECORD.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1410-READ-PREV-RECORD' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF UPD-SEL-BY-EMPLOYEE-ID THEN
              EXEC CICS READPREV
                   FILE(APP-EMP-MASTER-FILE-NAME)
                   RIDFLD(EMP-EMPLOYEE-ID)
                   INTO (EMPLOYEE-MASTER-RECORD)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           ELSE
              EXEC CICS READPREV
                   FILE(APP-EMP-MASTER-PATH-NAME)
                   RIDFLD(EMP-PRIMARY-NAME)
                   INTO (EMPLOYEE-MASTER-RECORD)
                   RESP(WS-CICS-RESPONSE)
                   END-EXEC
           END-IF.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                PERFORM 3200-APPLY-FILTERS
                PERFORM 3700-CHECK-DELETION
           WHEN DFHRESP(NOTFND)
                MOVE 'No Previous Records Found!' TO WS-MESSAGE
                SET UPD-TOP-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'Start of Employee Master File' TO WS-MESSAGE
                SET UPD-TOP-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Previous Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1500-FIND-RECORD-BY-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1500-FIND-RECORD-BY-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS READ
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                INTO (EMPLOYEE-MASTER-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                PERFORM 3200-APPLY-FILTERS
                PERFORM 3700-CHECK-DELETION
           WHEN DFHRESP(NOTFND)
                MOVE 'No Record Found By That Id!' TO WS-MESSAGE
           WHEN DFHRESP(ENDFILE)
                MOVE 'End of Employee Master File' TO WS-MESSAGE
                SET UPD-END-OF-FILE TO TRUE
           WHEN OTHER
                MOVE 'Error Reading Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1600-LOOKUP-USER-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1600-LOOKUP-USER-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    LOOKUP THE USER ID IN VSAM FILE
           EXEC CICS READ
                FILE(APP-REG-USER-FILE-NAME)
                INTO (REGISTERED-USER-RECORD)
                RIDFLD(REG-USER-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE REG-EMPLOYEE-ID TO UPD-USER-EMP-ID
           WHEN DFHRESP(NOTFND)
                MOVE "User Not Found!" TO WS-MESSAGE
           WHEN OTHER
                MOVE "Error Reading Users File!" TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       1700-GET-DEPARTMENT-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1700-GET-DEPARTMENT-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE UPD-USER-EMP-ID TO EMP-EMPLOYEE-ID.

      *    READ THE EMPLOYEE MASTER FILE TO GET THE USER'S DEPT ID.
           EXEC CICS READ
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                INTO (EMPLOYEE-MASTER-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
      *         SAVE DEPARTMENT ID INTO APP CONTAINER.
                MOVE EMP-DEPARTMENT-ID TO UPD-USER-DEPT-ID
                INITIALIZE EMPLOYEE-MASTER-RECORD
           WHEN DFHRESP(NOTFND)
                MOVE "Employee Not Found!" TO WS-MESSAGE
           WHEN OTHER
                MOVE "Error Reading Employee File!" TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *-----------------------------------------------------------------
       UPDATING SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS RECEIVE
                MAP(APP-UPDATE-MAP-NAME)
                MAPSET(APP-UPDATE-MAPSET-NAME)
                INTO (EUPDMI)
                END-EXEC.

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

           EVALUATE EIBAID
           WHEN DFHENTER
      *         IF THE USER HAS ENTERED AN USER ID, THEN WE SEEK THAT
      *         EMPLOYEE RECORD FOR DISPLAY.
      *         IF HE DIDN'T, THEN WE PROCEED TO VALIDATE POTENTIALLY
      *         UPDATED FIELDS ON THE CURRENT ONE.
                PERFORM 2050-CHECK-USER-ID-INPUT

                IF USER-ID-INPUT THEN
                   PERFORM 2100-FIND-BY-EMPLOYEE-ID
                ELSE
                   PERFORM 2700-VALIDATE-USER-INPUT
                END-IF
           WHEN DFHPF3
           WHEN DFHPF5
           WHEN DFHPF12
                PERFORM 2200-TRANSFER-BACK-TO-CALLER
           WHEN DFHPF4
                PERFORM 2700-VALIDATE-USER-INPUT

                IF VALIDATION-PASSED THEN
                   PERFORM 2900-UPDATE-EMPLOYEE
                END-IF
           WHEN DFHPF6
                PERFORM 5500-SECRET-MODE-SWITCH
           WHEN DFHPF7
                PERFORM 2300-PREV-BY-EMPLOYEE-KEY
           WHEN DFHPF8
                PERFORM 2400-NEXT-BY-EMPLOYEE-KEY
           WHEN DFHPF9
                PERFORM 2500-SWITCH-DISPLAY-ORDER
           WHEN DFHPF10
                PERFORM 2600-SIGN-USER-OFF
           WHEN DFHPF11
                PERFORM 5000-DELETE-EMPLOYEE
           WHEN OTHER
                MOVE 'Invalid Key!' TO WS-MESSAGE
           END-EVALUATE.

       2050-CHECK-USER-ID-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2050-CHECK-USER-ID-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE WS-USER-ID-FLAG.

           IF EMPLIDL IS GREATER THAN ZERO THEN
              SET USER-ID-INPUT TO TRUE
           END-IF.

       2100-FIND-BY-EMPLOYEE-ID.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-FIND-BY-EMPLOYEE-ID' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF EMPLOYEE ID WAS ENTERED, THEN FIND BY ID.
           IF EMPLIDI IS NOT EQUAL TO LOW-VALUES AND
              EMPLIDI IS NOT EQUAL TO SPACES THEN
      *       --- BY ID ---
              SET UPD-SEL-BY-EMPLOYEE-ID TO TRUE

              EXEC CICS BIF DEEDIT
                   FIELD(EMPLIDI)
                   LENGTH(8)
                   END-EXEC

              MOVE EMPLIDI TO EMP-EMPLOYEE-ID
           END-IF.

           PERFORM 1500-FIND-RECORD-BY-ID.

           IF FILTERS-PASSED THEN
              PERFORM 1325-COPY-INTO-CONTAINER
           ELSE
              MOVE 'No Matching Record By That Key!' TO WS-MESSAGE
           END-IF.

       2200-TRANSFER-BACK-TO-CALLER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-TRANSFER-BACK-TO-CALLER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF WE ARE COMING FROM 'OURSELVES', I.E. WE INVOKED THE
      *    'EUPD' TRANSACTION DIRECTLY, THEN WE JUST RETURN TO CICS.
           IF UPD-CALLING-PROGRAM IS EQUAL TO APP-UPDATE-PROGRAM-NAME
              PERFORM 9200-RETURN-TO-CICS
           END-IF.

      *    IF WE ARE COMING FROM THE 'EMPLOYEE DETAILS' VIEW, UPDATE
      *    'VIEW' CONTAINER PRIOR TO RETURNING.
           IF UPD-CALLING-PROGRAM IS EQUAL TO APP-VIEW-PROGRAM-NAME
      *       >>> UPDATED RECORD! <<<
              MOVE UPD-EMPLOYEE-RECORD TO DET-EMPLOYEE-RECORD
      *       >>> --------------- <<<
              MOVE UPD-SELECT-KEY-TYPE TO DET-SELECT-KEY-TYPE
              MOVE APP-UPDATE-PROGRAM-NAME TO DET-SAVING-PROGRAM

              PERFORM 3100-PUT-VIEW-CONTAINER
           END-IF.

      *    OTHERWISE, WE TRANSFER BACK TO THE CALLING PROGRAM.
      *    I.E. 'EVIEWP' (EMPLOYEE DETAILS) OR 'EMENUP' (MENU).

      *    RESET THIS CONVERSATION BY DELETING CURRENT CONTAINER.
           PERFORM 2250-DELETE-UPDATE-CONTAINER.

           EXEC CICS XCTL
                PROGRAM(UPD-CALLING-PROGRAM)
                CHANNEL(APP-ACTMON-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN DFHRESP(PGMIDERR)
                MOVE 'Caller Program Not Found!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE 'Error Transferring To Caller!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       2250-DELETE-UPDATE-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2250-DELETE-UPDATE-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS DELETE
                CONTAINER(APP-UPDATE-CONTAINER-NAME)
                CHANNEL(APP-UPDATE-CHANNEL-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(NOTFND)
                MOVE 'Update Container Not Found!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Deleting Update Container!' TO WS-MESSAGE
           END-EVALUATE.

       2300-PREV-BY-EMPLOYEE-KEY.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2300-PREV-BY-EMPLOYEE-KEY' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF NOT UPD-TOP-OF-FILE THEN
              IF UPD-SEL-BY-EMPLOYEE-ID THEN
                 SUBTRACT 1 FROM EMP-EMPLOYEE-ID
              ELSE
                 CONTINUE
              END-IF

      *       RESET 'FILE BOUNDARY' FLAG.
              INITIALIZE UPD-FILE-FLAG

      *       READ FILE BACKWARDS!
              PERFORM 1400-READ-BACKWARDS-BY-KEY
           ELSE
              MOVE 'No Previous Records To Display!' TO WS-MESSAGE
              MOVE DFHPROTN TO HLPPF7A
           END-IF.

       2400-NEXT-BY-EMPLOYEE-KEY.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2400-NEXT-BY-EMPLOYEE-KEY' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE UPD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

           IF NOT UPD-END-OF-FILE THEN
              IF UPD-SEL-BY-EMPLOYEE-ID THEN
                 ADD 1 TO EMP-EMPLOYEE-ID
              ELSE
                 MOVE HIGH-VALUES TO EMP-PRIMARY-NAME(38:)
              END-IF

      *       RESET 'FILE BOUNDARY' FLAG.
              INITIALIZE UPD-FILE-FLAG

      *       READ FILE IN NORMAL (FORWARD) WAY.
              PERFORM 1300-READ-EMPLOYEE-BY-KEY
           ELSE
              MOVE 'No More Records To Display!' TO WS-MESSAGE
              MOVE DFHPROTN TO HLPPF8A
           END-IF.

       2500-SWITCH-DISPLAY-ORDER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2500-SWITCH-DISPLAY-ORDER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           IF UPD-SEL-BY-EMPLOYEE-ID THEN
              SET UPD-SEL-BY-EMPLOYEE-NAME TO TRUE
           ELSE
              SET UPD-SEL-BY-EMPLOYEE-ID TO TRUE
           END-IF.

      *    RESET 'FILE BOUNDARY' FLAG.
           INITIALIZE UPD-FILE-FLAG.

       2600-SIGN-USER-OFF.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2600-SIGN-USER-OFF' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    >>> CALL ACTIVITY MONITOR <<<
           SET MON-AC-SIGN-OFF TO TRUE.
           PERFORM 4200-CALL-ACTIVITY-MONITOR.
      *    >>> --------------------- <<<

           PERFORM 9200-RETURN-TO-CICS.

       2700-VALIDATE-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2700-VALIDATE-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    RESTORE LAST SAVED AND VALIDATED DATA FROM CONTAINER.
           MOVE UPD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

      *    GET MODIFIED FIELDS FROM MAP.
           PERFORM 2710-GET-UPDATED-FIELDS.

      *    VERY FIRST CHECK -> IF NO CHANGES WERE MADE, JUST EXIT.
           IF EMPLOYEE-MASTER-RECORD IS EQUAL TO UPD-ORIGINAL-RECORD
              MOVE 'No Changes Made!' TO WS-MESSAGE
              SET NO-CHANGES-MADE TO TRUE
              EXIT PARAGRAPH
           ELSE
      *       OTHERWISE, SAVE UPDATED STATE INTO APP CONTAINER.
              PERFORM 2800-CONVERT-TO-TITLE-CASE
              MOVE EMPLOYEE-MASTER-RECORD TO UPD-EMPLOYEE-RECORD
           END-IF.

      *    NOW, VALIDATE THE UPDATED FIELDS.
           PERFORM 2720-VALIDATE-UPDATED-FIELDS.

       2710-GET-UPDATED-FIELDS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2710-GET-UPDATED-FIELDS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    UPDATED FIELDS WILL HAVE POSITIVE LENGTHS.
           IF PRNAMEL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(PRNAMEI) TO EMP-PRIMARY-NAME
           END-IF.
           IF HONORL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(HONORI) TO EMP-HONORIFIC
           END-IF.
           IF SHNAMEL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(SHNAMEI) TO EMP-SHORT-NAME
           END-IF.
           IF FLNAMEL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(FLNAMEI) TO EMP-FULL-NAME
           END-IF.
           IF JBTITLL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(JBTITLI) TO EMP-JOB-TITLE
           END-IF.
           IF APPRRSL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(APPRRSI) TO EMP-APPRAISAL-RESULT
           END-IF.
           IF DELFLGL IS GREATER THAN ZERO THEN
              MOVE FUNCTION TRIM(DELFLGI) TO EMP-DELETE-FLAG
           END-IF.

      *    CLEAN UP NUMERIC VALUES LIKE DATES AND IDS.
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
              MOVE STDATEI(3:8) TO EMP-START-DATE
           END-IF.
           IF ENDATEL IS GREATER THAN ZERO THEN
              EXEC CICS BIF DEEDIT
                   FIELD(ENDATEI)
                   LENGTH(LENGTH OF ENDATEI)
                   END-EXEC
              MOVE ENDATEI(3:8) TO EMP-END-DATE
           END-IF.
           IF APPRDTL IS GREATER THAN ZERO THEN
              EXEC CICS BIF DEEDIT
                   FIELD(APPRDTI)
                   LENGTH(LENGTH OF APPRDTI)
                   END-EXEC
              MOVE APPRDTI(3:8) TO EMP-APPRAISAL-DATE
           END-IF.
           IF DELDTL IS GREATER THAN ZERO THEN
              EXEC CICS BIF DEEDIT
                   FIELD(DELDTI)
                   LENGTH(LENGTH OF DELDTI)
                   END-EXEC
              MOVE DELDTI(3:8) TO EMP-DELETE-DATE
           END-IF.

       2720-VALIDATE-UPDATED-FIELDS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2720-VALIDATE-UPDATED-FIELDS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    INITIALIZE VALIDATION STATE IS 'NOT PASSED'.
           SET VALIDATION-FAILED TO TRUE.

           IF EMP-PRIMARY-NAME IS EQUAL TO SPACES THEN
              MOVE 'Validation Error: Primary Name is required!'
                 TO WS-MESSAGE
              MOVE -1 TO PRNAMEL
              EXIT PARAGRAPH
           END-IF.

      *    CHECK IF THE PRIMARY NAME HAS ACTUALLY CHANGED FROM ITS
      *    ORIGINAL VALUE WHEN THE RECORD WAS READ FROM THE FILE.
           IF EMP-PRIMARY-NAME IS EQUAL TO UPD-EMP-ALT-KEY THEN
      *       NO ACTUAL PRIMARY NAME CHANGE, SO JUST MOVE ON.
              MOVE 'Primary Name has not changed' TO WS-MESSAGE
              CONTINUE
           ELSE
      *       ACTUAL PRIMARY NAME CHANGE, CHECK ALT-KEY AVAILABILTY.
              PERFORM 2730-CHECK-PRIMARY-NAME

              IF PRIMARY-NAME-EXISTS THEN
                 MOVE 'Validation Error: Primary Name already exists!'
                    TO WS-MESSAGE
                 MOVE -1 TO PRNAMEL
                 EXIT PARAGRAPH
              ELSE
                 MOVE 'New Primary Name is available' TO WS-MESSAGE
              END-IF
           END-IF.

           IF EMP-FULL-NAME IS EQUAL TO SPACES THEN
              MOVE 'Validation Error: Full Name is required!'
                 TO WS-MESSAGE
              MOVE -1 TO FLNAMEL
              EXIT PARAGRAPH
           END-IF.

           IF EMP-JOB-TITLE IS EQUAL TO SPACES THEN
              MOVE 'Validation Error: Job Title is required!'
                 TO WS-MESSAGE
              MOVE -1 TO JBTITLL
              EXIT PARAGRAPH
           END-IF.

           IF EMP-DEPARTMENT-ID IS EQUAL TO ZERO THEN
              MOVE 'Validation Error: Department Id is required!'
                 TO WS-MESSAGE
              MOVE -1 TO DEPTIDL
              EXIT PARAGRAPH
           END-IF.

           IF EMP-START-DATE IS EQUAL TO SPACES THEN
              MOVE 'Validation Error: Start Date is required!'
                 TO WS-MESSAGE
              MOVE -1 TO STDATEL
              EXIT PARAGRAPH
           END-IF.

           EVALUATE EMP-APPRAISAL-RESULT
           WHEN 'E'
           WHEN 'M'
           WHEN 'U'
                CONTINUE
           WHEN OTHER
                MOVE 'Validation Error: Invalid Appraisal Result!'
                   TO WS-MESSAGE
                MOVE -1 TO APPRRSL
                EXIT PARAGRAPH
           END-EVALUATE.

           EVALUATE EMP-DELETE-FLAG
           WHEN 'A'
           WHEN 'D'
                CONTINUE
           WHEN OTHER
                MOVE 'Validation Error: Invalid Delete Flag Value!'
                   TO WS-MESSAGE
                MOVE -1 TO DELFLGL
                EXIT PARAGRAPH
           END-EVALUATE.

      *    IF WE GET THIS FAR, THEN ALL FIELDS ARE VALIDATED!
           MOVE 'Updated Fields Successfully Validated!' TO WS-MESSAGE.
           SET VALIDATION-PASSED TO TRUE.

       2730-CHECK-PRIMARY-NAME.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2730-CHECK-PRIMARY-NAME' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE WS-FILE-FLAG.

           PERFORM 2740-START-BROWSING-NM.

           IF NOT END-OF-FILE THEN
              PERFORM 2750-END-BROWSING-NM
           END-IF.

       2740-START-BROWSING-NM.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2740-START-BROWSING-NM' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS STARTBR
                FILE(APP-EMP-MASTER-PATH-NAME)
                RIDFLD(EMP-PRIMARY-NAME)
                EQUAL
                RESP(WS-CICS-RESPONSE)
                END-EXEC

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Validation Error: Primary Name already exists!'
                   TO WS-MESSAGE
                SET PRIMARY-NAME-EXISTS TO TRUE
           WHEN DFHRESP(NOTFND)
                MOVE 'Primary Name is available!' TO WS-MESSAGE
                SET PRIMARY-NAME-VALID TO TRUE
                SET END-OF-FILE TO TRUE
           WHEN DFHRESP(ENDFILE)
                MOVE 'No Records Found!' TO WS-MESSAGE
                SET END-OF-FILE TO TRUE
                SET PRIMARY-NAME-VALID TO TRUE
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

       2750-END-BROWSING-NM.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2750-END-BROWSING-NM' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS ENDBR
                FILE(APP-EMP-MASTER-PATH-NAME)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (End Browse)!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Name Path Not Open!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE 'Error Ending Browse!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       2800-CONVERT-TO-TITLE-CASE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2800-CONVERT-TO-TITLE-CASE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    FIRST, CONVERT ALL EMPLOYEE DETAILS TO LOWER CASE.
           MOVE FUNCTION LOWER-CASE(EMP-DETAILS) TO EMP-DETAILS.

      *    THEN, CAPITALIZE FIRST LETTER OF EACH FIELD.
           MOVE FUNCTION UPPER-CASE(EMP-FULL-NAME(1:1))
              TO EMP-FULL-NAME(1:1).
           MOVE FUNCTION UPPER-CASE(EMP-PRIMARY-NAME(1:1))
              TO EMP-PRIMARY-NAME(1:1).
           MOVE FUNCTION UPPER-CASE(EMP-HONORIFIC(1:1))
              TO EMP-HONORIFIC(1:1).
           MOVE FUNCTION UPPER-CASE(EMP-SHORT-NAME(1:1))
              TO EMP-SHORT-NAME(1:1).
           MOVE FUNCTION UPPER-CASE(EMP-JOB-TITLE(1:1))
              TO EMP-JOB-TITLE(1:1).
           MOVE FUNCTION UPPER-CASE(EMP-APPRAISAL-RESULT)
              TO EMP-APPRAISAL-RESULT.
           MOVE FUNCTION UPPER-CASE(EMP-DELETE-FLAG)
              TO EMP-DELETE-FLAG.

      *    FINALLY, CAPITALIZE INITIAL LETTERS OF ALL INNER WORDS.
           INSPECT EMP-DETAILS
              REPLACING
              ALL ' a' BY ' A',
              ALL ' b' BY ' B',
              ALL ' c' BY ' C',
              ALL ' d' BY ' D',
              ALL ' e' BY ' E',
              ALL ' f' BY ' F',
              ALL ' g' BY ' G',
              ALL ' h' BY ' H',
              ALL ' i' BY ' I',
              ALL ' j' BY ' J',
              ALL ' k' BY ' K',
              ALL ' l' BY ' L',
              ALL ' m' BY ' M',
              ALL ' n' BY ' N',
              ALL ' o' BY ' O',
              ALL ' p' BY ' P',
              ALL ' q' BY ' Q',
              ALL ' r' BY ' R',
              ALL ' s' BY ' S',
              ALL ' t' BY ' T',
              ALL ' u' BY ' U',
              ALL ' v' BY ' V',
              ALL ' w' BY ' W',
              ALL ' x' BY ' X',
              ALL ' y' BY ' Y',
              ALL ' z' BY ' Z'.

      *    PLUS -> THE *O'CONNOR* CASE!
           INSPECT EMP-DETAILS
              REPLACING
              ALL "'a" BY "'A",
              ALL "'b" BY "'B",
              ALL "'c" BY "'C",
              ALL "'d" BY "'D",
              ALL "'e" BY "'E",
              ALL "'f" BY "'F",
              ALL "'g" BY "'G",
              ALL "'h" BY "'H",
              ALL "'i" BY "'I",
              ALL "'j" BY "'J",
              ALL "'k" BY "'K",
              ALL "'l" BY "'L",
              ALL "'m" BY "'M",
              ALL "'n" BY "'N",
              ALL "'o" BY "'O",
              ALL "'p" BY "'P",
              ALL "'q" BY "'Q",
              ALL "'r" BY "'R",
              ALL "'s" BY "'S",
              ALL "'t" BY "'T",
              ALL "'u" BY "'U",
              ALL "'v" BY "'V",
              ALL "'w" BY "'W",
              ALL "'x" BY "'X",
              ALL "'y" BY "'Y",
              ALL "'z" BY "'Z".

      *    AND -> THE 'JAN LEVINSON-GOULD' CASE! :)
           INSPECT EMP-DETAILS
              REPLACING
              ALL '-a' BY '-A',
              ALL '-b' BY '-B',
              ALL '-c' BY '-C',
              ALL '-d' BY '-D',
              ALL '-e' BY '-E',
              ALL '-f' BY '-F',
              ALL '-g' BY '-G',
              ALL '-h' BY '-H',
              ALL '-i' BY '-I',
              ALL '-j' BY '-J',
              ALL '-k' BY '-K',
              ALL '-l' BY '-L',
              ALL '-m' BY '-M',
              ALL '-n' BY '-N',
              ALL '-o' BY '-O',
              ALL '-p' BY '-P',
              ALL '-q' BY '-Q',
              ALL '-r' BY '-R',
              ALL '-s' BY '-S',
              ALL '-t' BY '-T',
              ALL '-u' BY '-U',
              ALL '-v' BY '-V',
              ALL '-w' BY '-W',
              ALL '-x' BY '-X',
              ALL '-y' BY '-Y',
              ALL '-z' BY '-Z'.

       2900-UPDATE-EMPLOYEE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2900-UPDATE-EMPLOYEE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    READ EMPLOYEE RECORD AGAIN, THIS TIME FOR UPDATE.
           PERFORM 2910-READ-FOR-UPDATE UNTIL AVAILABLE-FOR-UPDATE.

      *    IF WE GET EXACTLY THE SAME DATA AS ORIGINALLY READ FIRST
      *    TIME THROUGH, THEN WE ARE GOOD TO GO!
           IF EMPLOYEE-MASTER-RECORD IS EQUAL TO UPD-ORIGINAL-RECORD
      *       >>> ACTUAL UPDATING <<<
              PERFORM 2920-REWRITE-RECORD
      *       >>> --------------- <<<
           ELSE
              MOVE 'Employee Record Has Changed Since!' TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

       2910-READ-FOR-UPDATE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2910-READ-FOR-UPDATE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    GET THE CURRENTLY EDITED EMPLOYEE ID FROM THE APP CONTAINER.
           MOVE UPD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

      *    AND THEN RE-READ THE RECORD FROM THE FILE BUT THIS TIME
      *    FOR UPDATE! (NOTE 'UPDATE' CLAUSE)
           EXEC CICS READ
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                INTO (EMPLOYEE-MASTER-RECORD)
                RESP(WS-CICS-RESPONSE)
                UPDATE
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                SET AVAILABLE-FOR-UPDATE TO TRUE
           WHEN DFHRESP(RECORDBUSY)
                MOVE 'Record is Busy!' TO WS-MESSAGE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (Read For Update)!' TO WS-MESSAGE
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Master File Not Open!' TO WS-MESSAGE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Record Found By That Id!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Reading Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       2920-REWRITE-RECORD.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2920-REWRITE-RECORD' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF WE GOT HERE, THEN WE ARE READY TO REWRITE THE RECORD
      *    FROM THE SCREEN-UPDATED DATA.
           EXEC CICS REWRITE
                FILE(APP-EMP-MASTER-FILE-NAME)
                FROM (UPD-EMPLOYEE-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Employee Record Successfully Updated!'
                   TO WS-MESSAGE

      *         WRITE AUDIT TRAIL FOR UPDATE ACTION.
                SET AUD-ACTION-UPDATE TO TRUE
                PERFORM 7000-WRITE-AUDIT-TRAIL

      *         SET THE UPDATED VERSION AS THE NEW 'ORIGINAL' FOR
      *         COMPARING AGAINST FUTURE REWRITES.
                MOVE UPD-EMPLOYEE-RECORD TO UPD-ORIGINAL-RECORD

           WHEN DFHRESP(DUPREC)
                MOVE 'Invalid Duplicate Key (Rewrite)!' TO WS-MESSAGE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (Rewrite)!' TO WS-MESSAGE
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Master File Not Open!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Rewriting Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

      *-----------------------------------------------------------------
       FILTERS SECTION.
      *-----------------------------------------------------------------

       3000-GET-VIEW-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3000-GET-VIEW-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS GET
                CONTAINER(APP-VIEW-CONTAINER-NAME)
                CHANNEL(APP-VIEW-CHANNEL-NAME)
                INTO (EMPLOYEE-DETAILS-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
                MOVE 'No Details Container Found!' TO WS-MESSAGE
                INITIALIZE UPD-EMPLOYEE-RECORD
           WHEN DFHRESP(NORMAL)
      *         GET RECORD AND FILTERS FROM 'DETAILS' CONTAINER.
                MOVE DET-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD
                MOVE DET-SELECT-KEY-TYPE TO UPD-SELECT-KEY-TYPE
                MOVE DET-FILTERS TO UPD-FILTERS

                PERFORM 1325-COPY-INTO-CONTAINER

      *         ALSO UPDATE CONTAINER WITH THIS PROGRAM'S NAME.
                MOVE APP-UPDATE-PROGRAM-NAME TO DET-SAVING-PROGRAM
                PERFORM 3100-PUT-VIEW-CONTAINER
           WHEN OTHER
                MOVE 'Error Retrieving Details Container!' TO WS-MESSAGE
           END-EVALUATE.

       3100-PUT-VIEW-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3100-PUT-VIEW-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

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
                MOVE 'Error Putting Details Container!' TO WS-MESSAGE
           END-EVALUATE.

       3200-APPLY-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3200-APPLY-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    RESET ALL FILTERS FLAGS.
           INITIALIZE WS-FILTER-FLAGS.

      *    RESTORE FILTERS FROM APP CONTAINER.
           MOVE UPD-FILTERS TO LST-FILTERS.

      *    IF NO FILTERS WERE SET, THEN WE JUST 'OK' THE RECORD.
           IF LST-NO-FILTERS-SET THEN
              SET FILTERS-PASSED TO TRUE
              EXIT PARAGRAPH
           END-IF.

      *    IF FILTERS WERE SET, THEN WE CHECK THEM ALL.
           PERFORM 3300-APPLY-KEY-FILTERS.
           PERFORM 3400-APPLY-DEPT-FILTERS.
           PERFORM 3500-APPLY-DATE-FILTERS.

      *    IF *ALL* FILTERS WERE MET, THEN WE SET THE 'PASSED' FLAG.
           IF KEY-FILTER-PASSED AND
              DEPT-FILTER-PASSED AND
              DATE-FILTER-PASSED THEN
              SET FILTERS-PASSED TO TRUE
           END-IF.

       3300-APPLY-KEY-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3300-APPLY-KEY-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF 'VALUE' WAS OMITTED, WE IGNORE THE FILTER.
           IF LST-SELECT-KEY-VALUE IS EQUAL TO SPACES THEN
              SET KEY-FILTER-PASSED TO TRUE
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
                 SET KEY-FILTER-PASSED TO TRUE
              END-IF
           END-IF.

      *    SELECT OPTION '2' -> 'EMPLOYEE NAME' FILTER.
           IF LST-SEL-BY-EMPLOYEE-NAME THEN
              INITIALIZE WS-INSP-COUNTER

              INSPECT FUNCTION UPPER-CASE(EMP-PRIMARY-NAME)
                 TALLYING WS-INSP-COUNTER
                 FOR ALL FUNCTION TRIM(LST-SELECT-KEY-VALUE)

              IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
                 SET KEY-FILTER-PASSED TO TRUE
              END-IF
           END-IF.

       3400-APPLY-DEPT-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3400-APPLY-DEPT-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF NO DEPARTMENT FILTERS WERE SET, WE JUST 'OK' IT.
           IF LST-INCLUDE-DEPT-FILTERS IS EQUAL TO SPACES AND
              LST-EXCLUDE-DEPT-FILTERS IS EQUAL TO SPACES THEN
              SET DEPT-FILTER-PASSED TO TRUE
              EXIT PARAGRAPH
           END-IF.

      *    OTHERWISE, WE CHECK THE DEPARTMENT FILTERS.
           MOVE EMP-DEPARTMENT-ID TO WS-DEPT-KEY.

      *    FIRST, THE 'POSITIVE' DEPARTMENT INCLUSION FILTERS.
           IF LST-INCLUDE-DEPT-FILTERS IS EQUAL TO SPACES THEN
      *       NO 'INCLUDE' FILTERS, SO *ALL* DEPARTMENTS ARE FINE.
              SET DEPT-FILTER-PASSED TO TRUE
           ELSE
      *       WE NEED TO MATCH A 'WHITE-LISTED' DEPARTMENT TO PASS.
              PERFORM VARYING LST-IN-DEPT-INDEX
                 FROM 1 BY 1
                 UNTIL LST-IN-DEPT-INDEX IS GREATER THAN 4
                 OR DEPT-FILTER-PASSED
                      IF LST-INCL-DEPT-ID(LST-IN-DEPT-INDEX)
                         IS NOT EQUAL TO SPACES THEN

                         INITIALIZE WS-INSP-COUNTER

                         INSPECT WS-DEPT-KEY
                            TALLYING WS-INSP-COUNTER
                            FOR ALL FUNCTION TRIM
                            (LST-INCL-DEPT-ID(LST-IN-DEPT-INDEX))

                         IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
      *                     SUCCESS! IT PASSES THE FILTER.
                            SET DEPT-FILTER-PASSED TO TRUE
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
                 OR DEPT-FILTER-FAILED
                      IF LST-EXCL-DEPT-ID(LST-EX-DEPT-INDEX)
                         IS NOT EQUAL TO SPACES THEN

                         INITIALIZE WS-INSP-COUNTER

                         INSPECT WS-DEPT-KEY
                            TALLYING WS-INSP-COUNTER
                            FOR ALL FUNCTION TRIM
                            (LST-EXCL-DEPT-ID(LST-EX-DEPT-INDEX))

                         IF WS-INSP-COUNTER IS GREATER THAN ZERO THEN
      *                     BLACKLISTED! IT DOESN'T MAKE THE CUT.
                            SET DEPT-FILTER-FAILED TO TRUE
                         END-IF
                      END-IF
              END-PERFORM
           END-IF.

       3500-APPLY-DATE-FILTERS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3500-APPLY-DATE-FILTERS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF NO DATE FILTERS WERE SET, WE JUST 'OK' IT AND RETURN
           IF LST-EMPLOYMENT-DATE-FILTERS IS EQUAL TO SPACES THEN
              SET DATE-FILTER-PASSED TO TRUE
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
                 SET DATE-FILTER-PASSED TO TRUE
                 EXIT PARAGRAPH
              END-IF
           END-IF.

      *    IF ONLY DATE-BEFORE FILTER WAS SET.
           IF LST-EMPL-DATE-AFTER IS EQUAL TO SPACES THEN
              IF EMP-START-DATE IS LESS THAN LST-EMPL-DATE-BEFORE THEN
      *          SUCCESS!
                 SET DATE-FILTER-PASSED TO TRUE
                 EXIT PARAGRAPH
              END-IF
           END-IF.

      *    IF ONLY DATE-AFTER FILTER WAS SET.
           IF LST-EMPL-DATE-BEFORE IS EQUAL TO SPACES THEN
              IF EMP-START-DATE IS GREATER THAN LST-EMPL-DATE-AFTER THEN
      *          SUCCESS!
                 SET DATE-FILTER-PASSED TO TRUE
                 EXIT PARAGRAPH
              END-IF
           END-IF.

       3700-CHECK-DELETION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3700-CHECK-DELETION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    FILTER 'LOGICALLY DELETED' RECORDS FROM STANDARD USERS, BUT
      *    ALLOW MANAGERS AND ADMINISTRATORS TO SEE THEM.

           IF UPD-CT-MANAGER OR UPD-CT-ADMINISTRATOR THEN
              EXIT PARAGRAPH
           END-IF.

           IF EMP-DELETED THEN
              SET FILTERS-FAILED TO TRUE
           END-IF.

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
           MOVE APP-UPDATE-PROGRAM-NAME TO MON-LINKING-PROGRAM.
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
       DELETING SECTION.
      *-----------------------------------------------------------------

       5000-DELETE-EMPLOYEE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '5000-DELETE-EMPLOYEE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    STEP 1. FILTER OUT UNAUTHORIZED REQUESTS.
           IF UPD-CT-STANDARD THEN
              MOVE 'Standard Users Cannot Delete Employees'
                 TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

           IF UPD-CT-MANAGER AND
              EMP-DEPARTMENT-ID IS NOT EQUAL TO UPD-USER-DEPT-ID THEN
              MOVE 'Managers Cannot Delete Employees From Other Depts'
                 TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

           IF EMP-EMPLOYEE-ID IS EQUAL TO UPD-USER-EMP-ID THEN
              MOVE 'Invalid Action, You Cannot Delete Yourself!'
                 TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

           IF EMP-DELETED AND UPD-LOGICAL-MODE THEN
              MOVE 'Employee Record is Already Logically Deleted!'
                 TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

      *    STEP 2. ASK USER FOR CONFIRMATION.
           PERFORM 5100-ASK-FOR-CONFIRMATION
              UNTIL CONFIRM-DELETION OR CANCEL-DELETION.

           IF CANCEL-DELETION THEN
              MOVE 'Deletion Cancelled!' TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

      *    STEP 3. PROCEED WITH DELETION.

      *    READ EMPLOYEE RECORD AGAIN, ALSO FOR 'UPDATE'.
           PERFORM 2910-READ-FOR-UPDATE UNTIL AVAILABLE-FOR-UPDATE.

           IF EMPLOYEE-MASTER-RECORD IS EQUAL TO UPD-ORIGINAL-RECORD
      *       >>> DELETION (LOGICAL BY DEFAULT) <<<
              IF UPD-LOGICAL-MODE THEN
                 PERFORM 5200-LOGICAL-DELETION
              END-IF
              IF UPD-PHYSICAL-MODE THEN
                 PERFORM 5300-PHYSICAL-DELETION
              END-IF
      *       >>> ----------------------------- <<<
           ELSE
              MOVE 'Employee Record Has Changed Since!' TO WS-MESSAGE
              EXIT PARAGRAPH
           END-IF.

       5100-ASK-FOR-CONFIRMATION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '5100-ASK-FOR-CONFIRMATION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    THIS IS A 'FULLY CONVERSATIONAL' INVOCATION TO THE 'CONFIRM'
      *    MAP SCREEN. THE MAP IS DISPLAYED AND THE USER CAN ENTER
      *    THE APPROPIATE AID-KEY. THE MAP IS THEN SENT BACK TO THIS
      *    PARAGRAPH (ALTHOUGH WE ONLY CARE ABOUT THE PRESSED KEY).
      *
      *    UNLIKE THE PLURALSIGHT EXAMPLE, I CHOSE *NOT* TO DO A
      *    PSEUDO-CONVERSATIONAL 'CONFIRMATION' LOGIC BECAUSE I THOUGHT
      *    IT WAS UNNECESSARY AND PERHAPS CONFUSING FOR JUST A SIMPLE
      *    TASK AT HAND.

      *    MOVE CURRENT EMPLOYEE ID TO MAP FIELD FOR DISPLAY.
           MOVE UPD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.
           MOVE EMP-EMPLOYEE-ID TO DELEMPO.

      *    SET MODIFIED DATA TAG (MDT) TO AVOID CLASSIC 'AEI9' ABEND.
           MOVE DFHBMFSE TO DELEMPA.

      *    RENDER THE DELETE CONFIRMATION MAP.
           EXEC CICS SEND
                MAP(APP-DELETE-MAP-NAME)
                MAPSET(APP-DELETE-MAPSET-NAME)
                FROM (EDELMI)
                END-EXEC.

      *    <<<<<     PROGRAM EXECUTION HALTS HERE    >>>>>

      *    AND WAIT FOR THE USER TO ENTER APPROPIATE KEY.

      *    NOTE: A SIMPLE 'RECEIVE' COMMAND WITH NO 'MAP' CLAUSE COULD
      *          HAVE BEEN USED HERE, JUST WITH A 'LENGTH OF EIBAID'
      *          OPTION, SINCE WE ARE NOT GETTING ANY INPUT DATA FROM
      *          THE USER, BUT FOR CLARITY I USED THE CLASSIC FORMAT
      *          OF 'RECEIVE MAP INTO' INSTEAD.

           EXEC CICS RECEIVE
                MAP(APP-DELETE-MAP-NAME)
                MAPSET(APP-DELETE-MAPSET-NAME)
                INTO (EDELMI)
                END-EXEC.

      *    <<<<<    PROGRAM EXECUTION RESUMES HERE   >>>>>

      *    >>> CALL ACTIVITY MONITOR <<<
           PERFORM 4000-CHECK-USER-STATUS.
      *    >>> --------------------- <<<

           EVALUATE EIBAID
           WHEN DFHPF11
                SET CONFIRM-DELETION TO TRUE
           WHEN DFHPF3
           WHEN DFHPF12
           WHEN DFHENTER
                SET CANCEL-DELETION TO TRUE
           END-EVALUATE.

       5200-LOGICAL-DELETION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '5200-LOGICAL-DELETION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF WE GOT THIS FAR, WE ARE READY TO LOGICALLY DELETE THE
      *    RECORD FROM THE VSAM FILE.
           MOVE UPD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

      *    LOGICAL DELETION IS JUST A 'REWRITE' WITH THE 'DELETE FLAG'
      *    SET TO 'D' (YES) AND THE CURRENT DATE AS THE 'DELETE DATE'.
           MOVE FUNCTION CURRENT-DATE(1:8) TO EMP-DELETE-DATE.
           SET EMP-DELETED TO TRUE.

           MOVE EMPLOYEE-MASTER-RECORD TO UPD-EMPLOYEE-RECORD.

           EXEC CICS REWRITE
                FILE(APP-EMP-MASTER-FILE-NAME)
                FROM (UPD-EMPLOYEE-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Employee Record Logically Deleted!'
                   TO WS-MESSAGE

      *         WRITE AUDIT TRAIL FOR DELETE ACTION.
                SET AUD-ACTION-DELETE TO TRUE
                PERFORM 7000-WRITE-AUDIT-TRAIL

      *         CLEAN UP EMPLOYEE BUFFERS IN APP CONTAINER.
                INITIALIZE UPD-EMPLOYEE-RECORD
                           UPD-ORIGINAL-RECORD

           WHEN DFHRESP(DUPREC)
                MOVE 'Invalid Duplicate Key (Rewrite)!' TO WS-MESSAGE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (Rewrite)!' TO WS-MESSAGE
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Master File Not Open!' TO WS-MESSAGE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Record Found By That Id!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Rewriting Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       5300-PHYSICAL-DELETION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '5300-PHYSICAL-DELETION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    IF WE GOT THIS FAR, WE ARE READY TO PHYSICALLY DELETE THE
      *    RECORD FROM THE VSAM FILE.
           EXEC CICS DELETE
                FILE(APP-EMP-MASTER-FILE-NAME)
                RIDFLD(EMP-EMPLOYEE-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                MOVE 'Employee Record Physically Deleted!'
                   TO WS-MESSAGE

      *         CLEAN UP CURRENT BUFFER IN APP CONTAINER.
                INITIALIZE UPD-EMPLOYEE-RECORD

      *         WRITE AUDIT TRAIL FOR DELETE ACTION.
                SET AUD-ACTION-DELETE TO TRUE
                PERFORM 7000-WRITE-AUDIT-TRAIL

      *         CLEAN UP ORIGINAL BUFFER IN APP CONTAINER.
                INITIALIZE UPD-ORIGINAL-RECORD

           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (Delete)!' TO WS-MESSAGE
           WHEN DFHRESP(NOTOPEN)
                MOVE 'Employee Master File Not Open!' TO WS-MESSAGE
           WHEN DFHRESP(NOTFND)
                MOVE 'No Record Found By That Id!' TO WS-MESSAGE
           WHEN OTHER
                MOVE 'Error Deleting Employee Record!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

       5500-SECRET-MODE-SWITCH.
      *    >>> DEBUGGING ONLY <<<
           MOVE '5500-SECRET-MODE-SWITCH' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    SECRET <PF6> OPTION -> SWITCH DELETION MODE!
           IF UPD-LOGICAL-MODE THEN
              MOVE 'Physical Deletion Mode Set!' TO WS-MESSAGE
              SET UPD-PHYSICAL-MODE TO TRUE
           ELSE
              MOVE 'Logical Deletion Mode Set!' TO WS-MESSAGE
              SET UPD-LOGICAL-MODE TO TRUE
           END-IF.

      *-----------------------------------------------------------------
       AUDIT-TRAIL SECTION.
      *-----------------------------------------------------------------

       7000-WRITE-AUDIT-TRAIL.
      *    >>> DEBUGGING ONLY <<<
           MOVE '7000-WRITE-AUDIT-TRAIL' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    LOAD AUDIT TRAIL WITH:
      *
      *      - LOGGED-IN USER'S ID.
      *      - CURRENT DATE AND TIME.
      *      - ACTION INDICATOR.
      *      - ORGINAL EMPLOYEE RECORD (BEFORE UPDATE).
      *      - UPDATED EMPLOYEE RECORD.

           MOVE FUNCTION CURRENT-DATE TO AUD-TIMESTAMP.
           MOVE UPD-USER-ID TO AUD-USER-ID.

           MOVE UPD-ORIGINAL-RECORD TO AUD-RECORD-BEFORE.
           MOVE UPD-EMPLOYEE-RECORD TO AUD-RECORD-AFTER.

      *    CALL AUDIT TRAIL ASYNC TRANSACTION TO LOG THE UPDATE.
      *    ('FIRE AND FORGET' STYLE)
           EXEC CICS START
                TRANSID(APP-AUDIT-TRANSACTION-ID)
                TERMID(EIBTRMID)
                FROM (AUDIT-TRAIL-RECORD)
                REQID(APP-AUDIT-REQUEST-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN DFHRESP(INVREQ)
                MOVE 'Invalid Request (Audit Trail)!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN DFHRESP(TRANSIDERR)
                MOVE 'Audit Trail Transaction Not Found!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           WHEN OTHER
                MOVE 'Error Writing Audit Trail!' TO WS-MESSAGE
                PERFORM 9000-SEND-MAP-AND-RETURN
           END-EVALUATE.

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
           PERFORM 9130-SET-PROTECTED-FIELDS.
           PERFORM 9150-PUT-UPDATE-CONTAINER.

           EXEC CICS SEND
                MAP(APP-UPDATE-MAP-NAME)
                MAPSET(APP-UPDATE-MAPSET-NAME)
                FROM (EUPDMO)
                ERASE
                CURSOR
                END-EXEC.

           EXEC CICS RETURN
                CHANNEL(APP-UPDATE-CHANNEL-NAME)
                TRANSID(APP-UPDATE-TRANSACTION-ID)
                END-EXEC.

       9100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-POPULATE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE UPD-EMPLOYEE-RECORD TO EMPLOYEE-MASTER-RECORD.

      *    ALL USERS -> DISPLAY TRANSACTION ID.
           MOVE EIBTRNID TO TRANIDO.

      *    ALL USERS -> DISPLAY BROWSING ORDER.
           IF UPD-SEL-BY-EMPLOYEE-ID THEN
              MOVE 'By Id  ' TO SELBYO
           ELSE
              MOVE 'By Name' TO SELBYO
           END-IF.

      *    ALL USERS -> DISPLAY CURRENTLY LOGGED-IN USER.
           IF MON-USER-ID IS NOT EQUAL TO SPACES THEN
              MOVE MON-USER-ID TO LOGDINO
           ELSE
              MOVE '<Anonym>' TO LOGDINO
           END-IF.

      *    ALL USERS -> DISPLAY ID AND ALT-KEY FIELDS.
           MOVE EMP-EMPLOYEE-ID TO EMPLIDO.
           MOVE EMP-PRIMARY-NAME TO PRNAMEO.

      *    ALL USERS -> DISPLAY BASIC EMPLOYEE DATA.
           MOVE EMP-HONORIFIC TO HONORO.
           MOVE EMP-SHORT-NAME TO SHNAMEO.
           MOVE EMP-FULL-NAME TO FLNAMEO.

      *    STANDARD & MANAGERS -> DISPLAY FURTHER EMPLOYEE DATA.
           IF UPD-CT-STANDARD OR UPD-CT-MANAGER THEN
              MOVE EMP-JOB-TITLE TO JBTITLO
              MOVE EMP-DEPARTMENT-ID TO DEPTIDO
              MOVE SPACES TO DEPTNMO

              MOVE EMP-START-DATE TO WS-INPUT-DATE
              MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE
              MOVE WS-OUTPUT-DATE TO STDATEO

              MOVE EMP-END-DATE TO WS-INPUT-DATE
              MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE
              MOVE WS-OUTPUT-DATE TO ENDATEO
           ELSE
              MOVE SPACES TO JBTITLO DEPTIDO DEPTNMO STDATEO ENDATEO
           END-IF.

      *    USER HIMSELF & MANAGERS -> DISPLAY APPRAISAL DATA.
           IF UPD-CT-MANAGER OR
              (UPD-CT-STANDARD AND
              UPD-USER-EMP-ID IS EQUAL TO EMP-EMPLOYEE-ID) THEN

              MOVE EMP-APPRAISAL-DATE TO WS-INPUT-DATE
              MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE
              MOVE WS-OUTPUT-DATE TO APPRDTO

              EVALUATE TRUE
              WHEN EMP-EXCEEDS-EXPECTATIONS
                   MOVE 'Exceeds Expectations' TO APPRRSO
              WHEN EMP-MEETS-EXPECTATIONS
                   MOVE 'Meets Expectations' TO APPRRSO
              WHEN EMP-UH-OH
                   MOVE 'You Are Truly Fucked' TO APPRRSO
              WHEN OTHER
                   MOVE SPACES TO APPRRSO
              END-EVALUATE
           ELSE
              MOVE SPACES TO APPRDTO APPRRSO
           END-IF.

      *    MANAGERS & ADMINS -> DISPLAY LOGICAL RECORD STATUS.
           IF UPD-CT-MANAGER OR UPD-CT-ADMINISTRATOR THEN
              MOVE EMP-DELETE-FLAG TO DELFLGO

              EVALUATE TRUE
              WHEN EMP-ACTIVE
                   MOVE 'Active' TO DELDSCO
              WHEN EMP-DELETED
                   MOVE 'Deleted' TO DELDSCO
              WHEN OTHER
                   MOVE SPACES TO DELDSCO DELFLGO
              END-EVALUATE

              MOVE EMP-DELETE-DATE TO WS-INPUT-DATE
              MOVE CORRESPONDING WS-INPUT-DATE TO WS-OUTPUT-DATE
              MOVE WS-OUTPUT-DATE TO DELDTO
           ELSE
              MOVE SPACES TO DELFLGO DELDSCO DELDTO
           END-IF.

      *    USER HIMSELF -> SPECIAL GREETING!
           IF UPD-USER-EMP-ID IS GREATER THAN ZERO AND
              UPD-USER-EMP-ID IS EQUAL TO EMP-EMPLOYEE-ID AND
              WS-MESSAGE IS EQUAL TO SPACES THEN
              MOVE 'Hey! This Is Actually You!' TO WS-MESSAGE
           END-IF.

      *    ALL USERS -> DEFAULT 'ALL OK' MESSAGE.
           IF WS-MESSAGE IS EQUAL TO SPACES THEN
              MOVE 'So Far, So Good...' TO WS-MESSAGE
           END-IF.

      *    ALL USERS -> DISPLAY ALL-IMPORTANT MESSAGE LINE!
           MOVE WS-MESSAGE TO MESSO.
           MOVE DFHTURQ TO MESSC.

      *    ALL USERS -> COLOR MESSAGE ACCORDING TO TYPE/CONTENT.
           EVALUATE TRUE
           WHEN MESSO(01:5) IS EQUAL TO 'Error'
           WHEN MESSO(12:5) IS EQUAL TO 'Error'
           WHEN MESSO(01:7) IS EQUAL TO 'Invalid'
                MOVE DFHRED TO MESSC
           WHEN MESSO(01:3) IS EQUAL TO 'No '
           WHEN MESSO(01:8) IS EQUAL TO 'Standard'
           WHEN MESSO(01:8) IS EQUAL TO 'Managers'
           WHEN MESSO(01:8) IS EQUAL TO 'Employee'
                MOVE DFHYELLO TO MESSC
           WHEN MESSO(01:4) IS EQUAL TO 'Hey!'
           WHEN MESSO(01:8) IS EQUAL TO 'Physical'
           WHEN MESSO(01:8) IS EQUAL TO 'Logical '
                MOVE DFHPINK TO MESSC
           END-EVALUATE.

      *    HERE, WE SET THE MODIFIED DATA TAG (MDT) OF ONE THE FIELDS
      *    TO 'ON' TO AVOID THE 'AEI9' ABEND THAT HAPPENS DUE TO A
      *    'MAPFAIL' CONDITION WHEN WE LATER RECEIVE THE MAP WITH JUST
      *    AN AID KEY PRESS AND NO MODIFIED DATA ON IT.
           MOVE DFHBMFSE TO TRANIDA.

      *    ALL USERS -> HIDE NAVIGATION KEY LABELS IF NEEDED.
           IF UPD-TOP-OF-FILE THEN
              MOVE SPACES TO HLPPF7O
           END-IF.
           IF UPD-END-OF-FILE THEN
              MOVE SPACES TO HLPPF8O
           END-IF.

      *    STANDARD -> HIDE DELETION KEY LABEL.
           IF UPD-CT-STANDARD THEN
              MOVE SPACES TO HLPPF11O
           END-IF.

      *    MANAGERS -> HIDE DELETION KEY LABEL IF NOT IN SAME DEPT.
           IF UPD-CT-MANAGER AND
              EMP-DEPARTMENT-ID IS NOT EQUAL TO UPD-USER-DEPT-ID THEN
              MOVE SPACES TO HLPPF11O
           END-IF.

      *    SET INITIAL FOCUS ON 'EMPLOYEE ID' FIELD BY DEFAULT.
           IF VALIDATION-PASSED OR NO-CHANGES-MADE THEN
              MOVE -1 TO EMPLIDL
           END-IF.

       9130-SET-PROTECTED-FIELDS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9130-SET-PROTECTED-FIELDS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    PROTECT FIELDS THAT ARE NOT ALLOWED TO BE MODIFIED,
      *    DEPENDING ON THE LOGGED-IN USER TYPE.

      *    >>> -------------- <<<
      *    STANDARD RESTRICTIONS.
      *    >>> -------------- <<<

           IF UPD-CT-STANDARD THEN
      *       FIELDS NEVER ALLOWED TO UPDATE.
              MOVE DFHBMPRO TO JBTITLA
                               DEPTIDA
                               STDATEA
                               ENDATEA
                               APPRDTA
                               APPRRSA
                               DELFLGA
                               DELDSCA

      *       ALLOWED ONLY IF THE EMPLOYEE BEING UPDATED IS HIMSELF.
              IF EMP-EMPLOYEE-ID IS NOT EQUAL TO UPD-USER-EMP-ID THEN
                 MOVE DFHBMPRO TO PRNAMEA
                                  FLNAMEA
                                  HONORA
                                  SHNAMEA
              END-IF
           END-IF.

      *    >>> ------------- <<<
      *    MANAGER RESTRICTIONS.
      *    >>> ------------- <<<

           IF UPD-CT-MANAGER THEN
      *       FIELDS NEVER ALLOWED TO UPDATE.
              MOVE DFHBMPRO TO DEPTIDA

      *       ALLOWED ONLY IF UPDATING EMPLOYEES IN HIS OWN DEPARTMENT.
              IF EMP-DEPARTMENT-ID IS NOT EQUAL TO UPD-USER-DEPT-ID
                 MOVE DFHBMPRO TO JBTITLA
                                  DEPTIDA
                                  STDATEA
                                  ENDATEA
                                  APPRDTA
                                  APPRRSA
                                  DELFLGA
                                  DELDSCA
              END-IF

      *       ALLOWED ONLY IF THE EMPLOYEE BEING UPDATED IS HIMSELF.
              IF EMP-EMPLOYEE-ID IS NOT EQUAL TO UPD-USER-EMP-ID THEN
                 MOVE DFHBMPRO TO PRNAMEA
                                  FLNAMEA
                                  HONORA
                                  SHNAMEA
              END-IF
           END-IF.

      *    >>> ------------------- <<<
      *    ADMINISTRATOR RESTRICTIONS.
      *    >>> ------------------- <<<

           IF UPD-CT-ADMINISTRATOR THEN
      *       FIELDS NEVER ALLOWED TO UPDATE.
              MOVE DFHBMPRO TO JBTITLA
                               DEPTIDA
                               STDATEA
                               ENDATEA
                               APPRDTA
                               APPRRSA

      *       ALLOWED ONLY IF THE EMPLOYEE BEING UPDATED IS HIMSELF.
              IF EMP-EMPLOYEE-ID IS NOT EQUAL TO UPD-USER-EMP-ID THEN
                 MOVE DFHBMPRO TO FLNAMEA
                                  HONORA
                                  SHNAMEA
              END-IF
           END-IF.

       9150-PUT-UPDATE-CONTAINER.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9150-PUT-UPDATE-CONTAINER' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS PUT
                CONTAINER(APP-UPDATE-CONTAINER-NAME)
                CHANNEL(APP-UPDATE-CHANNEL-NAME)
                FROM (UPDATE-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE
           WHEN OTHER
                MOVE 'Error Putting Update Container!' TO WS-MESSAGE
           END-EVALUATE.

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