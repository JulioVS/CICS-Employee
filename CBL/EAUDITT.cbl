       IDENTIFICATION DIVISION.
       PROGRAM-ID. EAUDITT.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'AUDIT TRAIL' QUICK TEST PROGRAM.
      *      - INVOKED DIRECTLY FROM CICS SCREEN BY TESTER.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      *      - AUDIT TRAIL RECORD.
      ******************************************************************
       COPY ECONST.
       COPY EAUDIT.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE   PIC S9(8) USAGE IS BINARY.
      *
       01 WS-MESSAGE.
          05 FILLER             PIC X(29) VALUE
                'AUDIT TRAIL TEST - TASK ID: <'.
          05 WS-TASK-ID         PIC X(8)  VALUE SPACES.
          05 FILLER             PIC X(14) VALUE '> TIMESTAMP: <'.
          05 WS-TIMESTAMP       PIC X(16) VALUE SPACES.
          05 FILLER             PIC X(2)  VALUE '>'.
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

           PERFORM 1000-INITIALIZE.
           PERFORM 2000-TEST-CASE.
           PERFORM 9000-NORMAL-RETURN.

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-INITIALIZE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-INITIALIZE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE AUDIT-TRAIL-RECORD.
           INITIALIZE WS-WORKING-VARS.

      *-----------------------------------------------------------------
       TESTING SECTION.
      *-----------------------------------------------------------------

       2000-TEST-CASE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-TEST-CASE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP.
           MOVE EIBTASKN TO WS-TASK-ID.
           MOVE WS-MESSAGE TO AUDIT-TRAIL-RECORD.

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
           WHEN OTHER
                PERFORM 9100-ABEND-CICS
           END-EVALUATE.

      *-----------------------------------------------------------------
       EXIT-ROUTE SECTION.
      *-----------------------------------------------------------------

       9000-NORMAL-RETURN.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9000-NORMAL-RETURN' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS SEND CONTROL
                ERASE
                FREEKB
                END-EXEC.

           EXEC CICS RETURN
                END-EXEC.
           
       9100-ABEND-CICS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-ABEND-CICS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS ABEND
                ABCODE(APP-AUDIT-TESTING-TRN-ID)
                NODUMP 
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