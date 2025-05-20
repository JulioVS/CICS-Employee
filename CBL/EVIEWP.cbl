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
      *    >>> -------------- <<<

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
                PERFORM 9300-DEBUG-AID
      *         >>> -------------- <<<
                PERFORM 9000-RETURN-TO-CICS
           WHEN DFHRESP(NORMAL)
      *         >>> DEBUGGING ONLY <<<
                MOVE 'MAIN-> ALL GOOD, PROCESS INPUT' TO WS-DEBUG-AID
                PERFORM 9300-DEBUG-AID
      *         >>> -------------- <<<
                PERFORM 2000-PROCESS-USER-INPUT
           WHEN OTHER
      *         >>> DEBUGGING ONLY <<<
                MOVE 'MAIN-> GET CONTAINER ERROR!' TO WS-DEBUG-AID
                PERFORM 9300-DEBUG-AID
      *         >>> -------------- <<<
                PERFORM 9000-RETURN-TO-CICS
           END-EVALUATE.

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
           WHEN DFHPF3
                MOVE '2000-> Pressed PF3' TO WS-DEBUG-AID
           WHEN DFHPF7
                MOVE '2000-> Pressed PF7' TO WS-DEBUG-AID
           WHEN DFHPF8
                MOVE '2000-> Pressed PF8' TO WS-DEBUG-AID
           WHEN DFHPF10
                MOVE '2000-> Pressed PF10' TO WS-DEBUG-AID
           WHEN DFHPF12
                MOVE '2000-> Pressed PF12' TO WS-DEBUG-AID
           WHEN OTHER
                MOVE '2000-> Invalid Key!' TO WS-DEBUG-AID
           END-EVALUATE.

      *    >>> DEBUGGING ONLY <<<
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           CONTINUE.

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

           MOVE LST-CURRENT-RECORD(1) TO EMPLOYEE-MASTER-RECORD.

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

           MOVE 'So far so good!' TO MESSO.

           CONTINUE.

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
