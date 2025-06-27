       IDENTIFICATION DIVISION.
       PROGRAM-ID. EAUDITP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'AUDIT TRAIL' PROGRAM.
      *      - INVOKED VIA 'START FROM REQID'.
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
          05 WS-CICS-RESPONSE      PIC S9(8) USAGE IS BINARY.
          05 WS-RBA-FIELD          PIC S9(8) USAGE IS BINARY.
          05 WS-EOQ-FLAG           PIC X(1).
             88 END-OF-QUEUE                 VALUE 'Y'.
          05 WS-LOCK-FLAG          PIC X(1).
             88 LOCK-ACQUIRED                VALUE 'Y'.
             88 LOCK-NOT-ACQUIRED            VALUE 'N'.
      *
       01 WS-DEBUG-AID             PIC X(45) VALUE SPACES.
      *
       01 WS-DEBUG-MESSAGE.
          05 FILLER                PIC X(5)  VALUE '<MSG:'.
          05 WS-DEBUG-TEXT         PIC X(45) VALUE SPACES.
          05 FILLER                PIC X(1)  VALUE '>'.
          05 FILLER                PIC X(5)  VALUE '<EB1='.
          05 WS-DEBUG-EIBRESP      PIC 9(8)  VALUE ZEROES.
          05 FILLER                PIC X(1)  VALUE '>'.
          05 FILLER                PIC X(5)  VALUE '<EB2='.
          05 WS-DEBUG-EIBRESP2     PIC 9(8)  VALUE ZEROES.
          05 FILLER                PIC X(1)  VALUE '>'.
      *
       01 WS-DEBUG-MODE            PIC X(1)  VALUE 'N'.
          88 I-AM-DEBUGGING                  VALUE 'Y'.
          88 NOT-DEBUGGING                   VALUE 'N'.

       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       MAIN-LOGIC SECTION.
      *-----------------------------------------------------------------

      *    >>> DEBUGGING ONLY <<<
           MOVE 'MAIN-LOGIC' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           PERFORM 1000-INITIALIZE.
           PERFORM 2000-MAIN-LOOP UNTIL END-OF-QUEUE.
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
       PROCESS-AUDIT SECTION.
      *-----------------------------------------------------------------

       2000-MAIN-LOOP.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-MAIN-LOOP' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS RETRIEVE
                INTO (AUDIT-TRAIL-RECORD)
                LENGTH(LENGTH OF AUDIT-TRAIL-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                PERFORM 3000-PROCESS-AUDIT
           WHEN DFHRESP(ENDDATA)
                SET END-OF-QUEUE TO TRUE
           WHEN OTHER
                PERFORM 9100-ABEND-CICS
           END-EVALUATE.

       3000-PROCESS-AUDIT.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3000-PROCESS-AUDIT' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    BECAUSE THERE WILL BE *TWO* PHYSICAL 'EAUDIT' FILES, ONE
      *    MEANT TO BE WRITTEN AS THE APP OPERATES WHILE THE OTHER GETS 
      *    PROCESSED IN BATCH MODE, AT ONE POINT THEY WILL BE SWTICHED
      *    OVER SO WE CAN ALWAYS REFER TO THE FILE SIMPLY AS 'EAUDIT' 
      *    AS IF IT WAS A SINGLE FILE.

      *    HOWEVER, TO ENSURE WE ARE NOT ATTEMPTING TO WRITE JUST AT THE
      *    MOMENT WHEN THE FILE IS BEING SWITCHED OVER, WE WILL ENQUEUE
      *    THE RESOURCE BEFORE WRITING.

           INITIALIZE WS-LOCK-FLAG.

           PERFORM 3100-ENQUEUE-RESOURCE UNTIL LOCK-ACQUIRED.
           PERFORM 3200-WRITE-AUDIT-TRAIL.
           PERFORM 3300-DEQUEUE-RESOURCE.

       3100-ENQUEUE-RESOURCE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3100-ENQUEUE-RESOURCE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<
           
           EXEC CICS ENQ
                RESOURCE(APP-AUDIT-REQUEST-ID)
                LENGTH(LENGTH OF APP-AUDIT-REQUEST-ID)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.
     
           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                SET LOCK-ACQUIRED TO TRUE 
           WHEN DFHRESP(ENQBUSY)
                SET LOCK-NOT-ACQUIRED TO TRUE 
           WHEN OTHER
                PERFORM 9100-ABEND-CICS 
           END-EVALUATE.

       3200-WRITE-AUDIT-TRAIL.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3200-WRITE-AUDIT-TRAIL' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<
      
           EXEC CICS WRITE
                FILE(APP-AUDIT-TRAIL-FILE-NAME)
                RIDFLD(WS-RBA-FIELD)
                RBA
                FROM (AUDIT-TRAIL-RECORD)
                LENGTH(LENGTH OF AUDIT-TRAIL-RECORD)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.
                 
           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE 
           WHEN OTHER
                PERFORM 9100-ABEND-CICS 
           END-EVALUATE.

       3300-DEQUEUE-RESOURCE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '3300-DEQUEUE-RESOURCE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<
      
           EXEC CICS DEQ
                RESOURCE(APP-AUDIT-REQUEST-ID)
                LENGTH(LENGTH OF APP-AUDIT-REQUEST-ID)
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

           EXEC CICS RETURN
                END-EXEC.

       9100-ABEND-CICS.
      *    >>> DEBUGGING ONLY <<<
           MOVE '9100-ABEND-CICS' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           EXEC CICS ABEND
                ABCODE(APP-AUDIT-TRANSACTION-ID)
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