       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVIEWP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'
      *      - 'SHOW EMPLOYEE DETAILS' PROGRAM.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - EMPLOYEE DETAILS MAPSET.
      *      - LIST EMPLOYEES CONTAINER.
      *      - EMPLOYEE MASTER RECORD.
      *      - IBM'S AID KEYS.
      *      - IBM'S BMS VALUES.
      ******************************************************************
       COPY ECONST.
       COPY EDETMAP.
       COPY ELSTCTR.
       COPY EMPMAST.
       COPY DFHAID.
       COPY DFHBMSCA.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE   PIC S9(8) USAGE IS BINARY.
          05 WS-EXIT-FLAG       PIC X(1)  VALUE 'N'.
             88 WS-EXIT                   VALUE 'Y'.
      *
       01 WS-MAX-INDEX          PIC S9(4) USAGE IS BINARY
                                          VALUE +16.
      *
       01 WS-DISPLAY-MESSAGES.
          05 WS-PF7-LABEL       PIC X(9)  VALUE 'PF7 Prev '.
          05 WS-PF8-LABEL       PIC X(9)  VALUE 'PF8 Next '.
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
      *   DEBUGGING MODE -> SET TO 'Y' FOR TESTING PURPOSES ONLY!
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

           IF I-AM-DEBUGGING THEN
              MOVE +3 TO WS-MAX-INDEX
           END-IF.
      *    >>> -------------- <<<

           PERFORM 1000-FIRST-INTERACTION.

           EXEC CICS GET
                CONTAINER(APP-LIST-CONTAINER-NAME)
                CHANNEL(APP-LIST-CHANNEL-NAME)
                INTO (LIST-EMPLOYEE-CONTAINER)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(CHANNELERR)
           WHEN DFHRESP(CONTAINERERR)
      *         >>> DEBUGGING ONLY <<<
                MOVE 'MAIN-> NO CONTAINER FOUND!' TO WS-DEBUG-AID
      *         >>> -------------- <<<
           WHEN DFHRESP(NORMAL)
      *         >>> DEBUGGING ONLY <<<
                MOVE 'MAIN-> ALL GOOD, PROCESS INPUT' TO WS-DEBUG-AID
      *         >>> -------------- <<<
                SET LST-RECORD-INDEX TO 1
                PERFORM 2000-PROCESS-USER-INPUT UNTIL WS-EXIT
           WHEN OTHER
      *         >>> DEBUGGING ONLY <<<
                MOVE 'MAIN-> GET CONTAINER ERROR!' TO WS-DEBUG-AID
      *         >>> -------------- <<<
           END-EVALUATE.

      *    >>> DEBUGGING ONLY <<<
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 9000-RETURN-TO-CICS.

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-FIRST-INTERACTION.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-FIRST-INTERACTION' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           CONTINUE.

      *-----------------------------------------------------------------
       USE-CASE SECTION.
      *-----------------------------------------------------------------

       2000-PROCESS-USER-INPUT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-PROCESS-USER-INPUT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE LST-CURRENT-RECORD(LST-RECORD-INDEX)
              TO EMPLOYEE-MASTER-RECORD.

           PERFORM 2100-POPULATE-MAP.
           PERFORM 2200-SEND-AND-RECEIVE-MAP.

       2100-POPULATE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-POPULATE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE EDETMO.

           MOVE EIBTRNID TO TRANIDO.
           MOVE EMP-EMPLOYEE-ID TO EMPLIDO.
           MOVE EMP-PRIMARY-NAME TO PRNAMEO.
           MOVE EMP-HONORIFIC TO HONORO.
           MOVE EMP-SHORT-NAME TO SHNAMEO.
           MOVE EMP-FULL-NAME TO FLNAMEO.

           MOVE EMP-JOB-TITLE TO JBTITLO.
           MOVE EMP-DEPARTMENT-ID TO DEPTIDO.
           MOVE 'Unknown' TO DEPTNMO.

           MOVE EMP-START-DATE TO STDATEO.
           MOVE EMP-END-DATE TO ENDATEO.
           MOVE EMP-APPRAISAL-DATE TO APPRDTO.

           EVALUATE EMP-APPRAISAL-RESULT
           WHEN 'E'
                MOVE 'Exceeds Expectations' TO APPRRSO
           WHEN 'M'
                MOVE 'Meets Expectations' TO APPRRSO
           WHEN 'U'
                MOVE 'Uh Oh!' TO APPRRSO
           WHEN OTHER
                MOVE 'Unknown' TO APPRRSO
           END-EVALUATE.

           MOVE EMP-DELETE-FLAG TO DELFLGO.
           MOVE EMP-DELETE-DATE TO DELDTO.

           EVALUATE EMP-DELETE-FLAG
           WHEN 'D'
                MOVE 'Deleted' TO DELDSCO
           WHEN 'A'
                MOVE 'Active' TO DELDSCO
           WHEN OTHER
                MOVE 'Unknown' TO DELDSCO
           END-EVALUATE.

           EVALUATE LST-USER-CATEGORY
           WHEN 'STD'
                MOVE 'I am a Standard Employee' TO MESSO
           WHEN 'MGR'
                MOVE 'I am a Manager' TO MESSO
           WHEN 'ADM'
                MOVE 'I am an Administrator' TO MESSO
           WHEN OTHER
                MOVE 'I do not have a Category!' TO MESSO
           END-EVALUATE.

           IF LST-RECORD-INDEX IS GREATER THAN 1 THEN
              MOVE 'PF7 Prev' TO HLPPF7O
           END-IF.
           IF LST-RECORD-INDEX IS LESS THAN WS-MAX-INDEX THEN
              MOVE 'PF8 Next' TO HLPPF8O
           END-IF.

       2200-SEND-AND-RECEIVE-MAP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-SEND-AND-RECEIVE-MAP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS SEND
                MAP(APP-VIEW-MAP-NAME)
                MAPSET(APP-VIEW-MAPSET-NAME)
                FROM (EDETMO)
                ERASE
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EXEC CICS RECEIVE
                MAP(APP-VIEW-MAP-NAME)
                MAPSET(APP-VIEW-MAPSET-NAME)
                INTO (EDETMI)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE EIBAID
           WHEN DFHENTER
                MOVE '2000-> Pressed ENTER' TO WS-DEBUG-AID
                IF LST-RECORD-INDEX IS LESS THAN WS-MAX-INDEX
                   SET LST-RECORD-INDEX UP BY 1
                END-IF
           WHEN DFHPF3
                MOVE '2000-> Pressed PF3' TO WS-DEBUG-AID
                SET WS-EXIT TO TRUE
           WHEN DFHPF7
                MOVE '2000-> Pressed PF7' TO WS-DEBUG-AID
                IF LST-RECORD-INDEX IS GREATER THAN 1
                   SET LST-RECORD-INDEX DOWN BY 1
                END-IF
           WHEN DFHPF8
                MOVE '2000-> Pressed PF8' TO WS-DEBUG-AID
                IF LST-RECORD-INDEX IS LESS THAN WS-MAX-INDEX
                   SET LST-RECORD-INDEX UP BY 1
                END-IF
           WHEN DFHPF10
                MOVE '2000-> Pressed PF10' TO WS-DEBUG-AID
                SET WS-EXIT TO TRUE
           WHEN DFHPF12
                MOVE '2000-> Pressed PF12' TO WS-DEBUG-AID
                SET WS-EXIT TO TRUE
           WHEN OTHER
                MOVE '2000-> Invalid Key!' TO WS-DEBUG-AID
           END-EVALUATE.

      *    >>> DEBUGGING ONLY <<<
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-RETURN-TO-CICS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-RETURN-TO-CICS' TO WS-DEBUG-AID.
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