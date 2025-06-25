      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP' - EMPLOYEE MASTER FILE.
      *      - RECORD LAYOUT FOR 'EMPMAST' VSAM <KSDS> FILE.
      *      - VSAM CLUSTER -> 'Z45864.KSDS.EMPMAST'.
      *      - SEED DATASET -> 'Z45864.DATA.EMPMAST'.
      ******************************************************************
       01 EMPLOYEE-MASTER-RECORD.
          05 EMP-KEY.
             10 EMP-EMPLOYEE-ID         PIC 9(8).
          05 EMP-DETAILS.
             10 EMP-FULL-NAME           PIC X(79).
             10 EMP-PRIMARY-NAME        PIC X(38).
             10 EMP-HONORIFIC           PIC X(8).
             10 EMP-SHORT-NAME          PIC X(38).
             10 EMP-JOB-TITLE           PIC X(38).
             10 EMP-DEPARTMENT-ID       PIC 9(8).
             10 EMP-START-DATE          PIC X(8).
             10 EMP-END-DATE            PIC X(8).
             10 EMP-APPRAISAL-DATE      PIC X(8).
             10 EMP-APPRAISAL-RESULT    PIC X(1).
                88 EMP-EXCEEDS-EXPECTATIONS       VALUE 'E'.
                88 EMP-MEETS-EXPECTATIONS         VALUE 'M'.
                88 EMP-UH-OH                      VALUE 'U'.
             10 EMP-DELETE-FLAG         PIC X(1).
                88 EMP-DELETED                    VALUE 'D'.
                88 EMP-ACTIVE                     VALUE 'A'.
             10 EMP-DELETE-DATE         PIC X(8).