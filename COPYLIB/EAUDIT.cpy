      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - AUDIT TRAIL.
      *      - RECORD LAYOUT FOR 'EAUDIT' VSAM <ESDS> FILE.
      *      - VSAM CLUSTERS -> 'Z45864.ESDS.EAUDIT1'.
      *                         'Z45864.ESDS.EAUDIT2'.
      ******************************************************************
       01 AUDIT-TRAIL-RECORD.
          05 AUD-TIMESTAMP         PIC X(21).
          05 AUD-USER-ID           PIC X(8).
          05 AUD-ACTION            PIC X(1).
             88 AUD-ACTION-ADD                VALUE 'A'.
             88 AUD-ACTION-UPDATE             VALUE 'U'.
             88 AUD-ACTION-DELETE             VALUE 'D'.
          05 AUD-RECORD-BEFORE     PIC X(251).
          05 AUD-RECORD-AFTER      PIC X(251).