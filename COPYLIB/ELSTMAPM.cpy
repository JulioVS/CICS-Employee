      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - MODIFIED VERSION OF THE 'LIST EMPLOYEE' MAP.
      *      - WE INSERT CUSTOM 16-ITEM ARRAYS TO REPLACE THE
      *        THE HARD-CODED REPEATED ITEMS IN THE ORIGINAL MAP.
      ******************************************************************
      *-----------------------------------------------------------------
      *   INPUT MAP SECTION.-
      *-----------------------------------------------------------------
       01 ELSTMI.
      *
      *   HEADING FIELDS (WE KEEP THEM 'AS IS')
      *
          02 FILLER        PIC X(12).
          02 TRANIDL COMP  PIC  S9(4).
          02 TRANIDF       PICTURE X.
          02 FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 TRANIDI       PIC X(4).
          02 PAGENL COMP   PIC  S9(4).
          02 PAGENF        PICTURE X.
          02 FILLER REDEFINES PAGENF.
             03 PAGENA     PICTURE X.
          02 FILLER        PICTURE X(1).
          02 PAGENI        PIC X(6).
          02 FLTRSL COMP   PIC  S9(4).
          02 FLTRSF        PICTURE X.
          02 FILLER REDEFINES FLTRSF.
             03 FLTRSA     PICTURE X.
          02 FILLER        PICTURE X(1).
          02 FLTRSI        PIC X(69).
      *
      *   CUSTOM 16-ITEM ARRAY (IN PLACE OF HARD-CODED REPEATED ITEMS)
      *
          02 LIST-LINEI OCCURS 16 TIMES INDEXED BY LINEI-INDEX.
             03 SELCTL COMP
                           PIC  S9(4).
             03 SELCTF     PICTURE X.
             03 FILLER REDEFINES SELCTF.
                04 SELCTA  PICTURE X.
             03 FILLER     PICTURE X(1).
             03 SELCTI     PIC X(1).
             03 EMPIDL COMP
                           PIC  S9(4).
             03 EMPIDF     PICTURE X.
             03 FILLER REDEFINES EMPIDF.
                04 EMPIDA  PICTURE X.
             03 FILLER     PICTURE X(1).
             03 EMPIDI     PIC X(8).
             03 PRMNML COMP
                           PIC  S9(4).
             03 PRMNMF     PICTURE X.
             03 FILLER REDEFINES PRMNMF.
                04 PRMNMA  PICTURE X.
             03 FILLER     PICTURE X(1).
             03 PRMNMI     PIC X(29).
             03 JOBTLL COMP
                           PIC  S9(4).
             03 JOBTLF     PICTURE X.
             03 FILLER REDEFINES JOBTLF.
                04 JOBTLA  PICTURE X.
             03 FILLER     PICTURE X(1).
             03 JOBTLI     PIC X(29).
             03 DPTIDL COMP
                           PIC  S9(4).
             03 DPTIDF     PICTURE X.
             03 FILLER REDEFINES DPTIDF.
                04 DPTIDA  PICTURE X.
             03 FILLER     PICTURE X(1).
             03 DPTIDI     PIC X(8).
      *
      *   MESSAGE SECTION (WE KEEP IT 'AS IS')
      *
          02 MESSL COMP    PIC  S9(4).
          02 MESSF         PICTURE X.
          02 FILLER REDEFINES MESSF.
             03 MESSA      PICTURE X.
          02 FILLER        PICTURE X(1).
          02 MESSI         PIC X(79).
      *-----------------------------------------------------------------
      *   OUTPUT MAP SECTION.-
      *-----------------------------------------------------------------
       01 ELSTMO REDEFINES ELSTMI.
      *
      *   HEADING FIELDS (WE KEEP THEM 'AS IS')
      *
          02 FILLER        PIC X(12).
          02 FILLER        PICTURE X(3).
          02 TRANIDC       PICTURE X.
          02 TRANIDO       PIC X(4).
          02 FILLER        PICTURE X(3).
          02 PAGENC        PICTURE X.
          02 PAGENO        PIC ZZZZZ9.
          02 FILLER        PICTURE X(3).
          02 FLTRSC        PICTURE X.
          02 FLTRSO        PIC X(69).
      *
      *   CUSTOM 16-ITEM ARRAY (IN PLACE OF HARD-CODED REPEATED ITEMS)
      *
          02 LIST-LINEO OCCURS 16 TIMES INDEXED BY LINEO-INDEX.
             03 FILLER     PICTURE X(3).
             03 SELCTC     PICTURE X.
             03 SELCTO     PIC X(1).
             03 FILLER     PICTURE X(3).
             03 EMPIDC     PICTURE X.
             03 EMPIDO     PIC X(8).
             03 FILLER     PICTURE X(3).
             03 PRMNMC     PICTURE X.
             03 PRMNMO     PIC X(29).
             03 FILLER     PICTURE X(3).
             03 JOBTLC     PICTURE X.
             03 JOBTLO     PIC X(29).
             03 FILLER     PICTURE X(3).
             03 DPTIDC     PICTURE X.
             03 DPTIDO     PIC X(8).
      *
      *   MESSAGE SECTION (WE KEEP IT 'AS IS')
      *
          02 FILLER        PICTURE X(3).
          02 MESSC         PICTURE X.
          02 MESSO         PIC X(79).