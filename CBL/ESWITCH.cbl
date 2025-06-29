       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESWITCH.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'SWITCH FILE' PROGRAM.
      *      - SWITCHES 'EAUDIT' CICS-DEFINED RESOURCE FROM 
      *        '<HLQ>.<MLQ>.EAUDIT1' TO '<HLQ>.<MLQ>.EAUDIT2' PHYSICAL
      *        FILES (AND VICEVERSA) SO THAT ONE CAN BE USED FOR ONLINE
      *        AUDITING AND THE OTHER FOR BATCH PROCESSING.
      *      - INVOKED MANUALLY ON CICS FOR THIS COURSE.
      *      - MEANT TO BE INVOKED IN BATCH ON A REAL LIFE SCENARIO.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE COPYBOOKS FOR:
      *      - APPLICATION CONSTANTS.
      ******************************************************************
       COPY ECONST.
      ******************************************************************
      *   DEFINE MY WORKING VARIABLES.
      ******************************************************************
       01 WS-WORKING-VARS.
          05 WS-CICS-RESPONSE      PIC S9(8) USAGE IS BINARY.
          05 WS-AUDIT-TRAIL-DSN    PIC X(44).
          05 WS-COUNTER            PIC S9(2) USAGE IS BINARY.
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

      *    >>> ---IMPORTANT!--- <<<
      *    THE USE OF 'CICS INQUIRE' AND 'CICS SET' SYSTEM COMMANDS 
      *    REQUIRE ADDING THE 'SP' OPTION IN THE TRANSLATOR OPTIONS 
      *    OF THE COMPILING JCL!
      *    >>> ---------------- <<<

           PERFORM 1000-INITIALIZE.
           PERFORM 2000-SWITCH-FILES.
           PERFORM 9000-NORMAL-RETURN.

      *-----------------------------------------------------------------
       START-UP SECTION.
      *-----------------------------------------------------------------

       1000-INITIALIZE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '1000-INITIALIZE' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

           INITIALIZE WS-WORKING-VARS.

      *-----------------------------------------------------------------
       PROCESS-AUDIT SECTION.
      *-----------------------------------------------------------------

       2000-SWITCH-FILES.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2000-SWITCH-FILES' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    WE HAVE TWO PHYSICAL INSTANCES OF THE AUDIT TRAIL FILE,
      *    NAMELY 'Z45864.ESDS.EUADIT1' AND 'Z45864.ESDS.EUADIT2'.

           EXEC CICS INQUIRE
                FILE(APP-AUDIT-TRAIL-FILE-NAME)
                DSNAME(WS-AUDIT-TRAIL-DSN)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE 
           WHEN OTHER
                PERFORM 9100-ABEND-CICS 
           END-EVALUATE.

      *    CHECK IF THE DSN IS CURRENTLY POINTING TO THE FIRST ONE.
           INSPECT WS-AUDIT-TRAIL-DSN TALLYING WS-COUNTER
              FOR ALL 'EAUDIT1'.

      *    IF SO, SWITCH TO THE SECOND AUDIT TRAIL DATASET.
           IF WS-COUNTER IS GREATER THAN ZERO THEN
              INSPECT WS-AUDIT-TRAIL-DSN
                 REPLACING FIRST 'EAUDIT1' BY 'EAUDIT2'
           ELSE 
      *       IF NOT, DO THE OPPOSITE.
              INSPECT WS-AUDIT-TRAIL-DSN
                 REPLACING FIRST 'EAUDIT2' BY 'EAUDIT1'
           END-IF.

      *    LOCK THE RESOURCE AND THEN MAKE THE FILE SWITCH!
           PERFORM 2100-ENQUEUE-RESOURCE UNTIL LOCK-ACQUIRED.
           PERFORM 2200-MAKE-SWITCH.
           PERFORM 2300-DEQUEUE-RESOURCE.

       2100-ENQUEUE-RESOURCE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2100-ENQUEUE-RESOURCE' TO WS-DEBUG-AID.
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

       2200-MAKE-SWITCH.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2200-MAKE-SWITCH' TO WS-DEBUG-AID.
           PERFORM 9300-DEBUG-AID.
      *    >>> -------------- <<<

      *    ISSUE A 'CECI' COMMAND TO CLOSE THE CURRENT AUDIT TRAIL FILE.
           EXEC CICS SET
                FILE(APP-AUDIT-TRAIL-FILE-NAME)
                OPENSTATUS(DFHVALUE(CLOSED))
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE 
           WHEN OTHER
                PERFORM 9100-ABEND-CICS 
           END-EVALUATE.

      *    SWITCH DATA SET NAME TO THE NEW/ALTERNATE/UPDATED ONE.
           EXEC CICS SET
                FILE(APP-AUDIT-TRAIL-FILE-NAME)
                DSNAME(WS-AUDIT-TRAIL-DSN)
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE 
           WHEN OTHER
                PERFORM 9100-ABEND-CICS 
           END-EVALUATE.

      *    OPEN THE ALTERNATE AUDIT TRAIL FILE WITH AN 'EMPTY' REQUEST.
           EXEC CICS SET
                FILE(APP-AUDIT-TRAIL-FILE-NAME)
                OPENSTATUS(DFHVALUE(OPEN))
                EMPTYSTATUS(DFHVALUE(EMPTYREQ))
                RESP(WS-CICS-RESPONSE)
                END-EXEC.

           EVALUATE WS-CICS-RESPONSE
           WHEN DFHRESP(NORMAL)
                CONTINUE 
           WHEN OTHER
                PERFORM 9100-ABEND-CICS 
           END-EVALUATE.

       2300-DEQUEUE-RESOURCE.
      *    >>> DEBUGGING ONLY <<<
           MOVE '2300-DEQUEUE-RESOURCE' TO WS-DEBUG-AID.
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
                ABCODE(APP-SWITCH-TRANSACTION-ID)
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