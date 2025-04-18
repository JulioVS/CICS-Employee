       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESONP.
      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYE APP'
      *      - 'SIGN IN' PROGRAM
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *   INCLUDE MY SYMBOLIC MAP COPYBOOK AND IBM'S AID KEYS' ONE.
      ******************************************************************
       COPY ESONMAP.
       COPY DFHAID.

       PROCEDURE DIVISION.
           INITIALIZE ESONMO.

           MOVE EIBTRNID TO TRANIDO.

           EXEC CICS SEND
                MAP('ESONM')
                MAPSET('ESONMAP')
                FROM (ESONMO)
                ERASE
                END-EXEC.

           EXEC CICS RECEIVE
                MAP('ESONM')
                MAPSET('ESONMAP')
                INTO (ESONMI)
                END-EXEC.

           STRING "Hello " DELIMITED BY SIZE
                  USERIDI DELIMITED BY SPACE
                  "!" DELIMITED BY SIZE
              INTO MESSO
           END-STRING.

           EXEC CICS SEND
                MAP('ESONM')
                MAPSET('ESONMAP')
                FROM (ESONMO)
                ERASE
                END-EXEC.

           EXEC CICS RETURN
                END-EXEC.
