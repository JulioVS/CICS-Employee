      ******************************************************************
      *   CICS PLURALSIGHT 'EMPLOYEE APP'.
      *      - 'ELSTMAP' SYMBOLIC MAPSET.
      ******************************************************************
      ******************************************************************
      *   'LIST EMPLOYEES' MAP (FIRST IN MAPSET).
      *      - MODIFIED VERSION OF THE 'LIST EMPLOYEE' MAP.
      *      - WE INSERT CUSTOM 16-ITEM ARRAYS TO REPLACE THE
      *        HARD-CODED REPEATED ITEMS IN THE ORIGINAL SYMBOLIC
      *        MAP.
      ******************************************************************
      *-----------------------------------------------------------------
      *   'ELSTM' - INPUT SECTION.
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
      *
      *   NEW PF7/PF8 SECTION (NEEDED TO HIDE/UNHIDE)
      *
          02 HLPPF7L COMP  PIC  S9(4).
          02 HLPPF7F       PICTURE X.
          02 FILLER REDEFINES HLPPF7F.
             03 HLPPF7A    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 HLPPF7I       PIC X(9).
          02 HLPPF8L COMP  PIC  S9(4).
          02 HLPPF8F       PICTURE X.
          02 FILLER REDEFINES HLPPF8F.
             03 HLPPF8A    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 HLPPF8I       PIC X(9).
      *-----------------------------------------------------------------
      *   'ELSTM' - OUTPUT SECTION.
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
      *
      *   NEW PF7/PF8 SECTION (NEEDED TO HIDE/UNHIDE)
      *
          02 FILLER        PICTURE X(3).
          02 HLPPF7C       PICTURE X.
          02 HLPPF7O       PIC X(9).
          02 FILLER        PICTURE X(3).
          02 HLPPF8C       PICTURE X.
          02 HLPPF8O       PIC X(9).
      *-----------------------------------------------------------------
      *   'ELSTM' - END OF MAP.
      *-----------------------------------------------------------------
      ******************************************************************
      *   'FILTERS' MAP (SECOND IN MAPSET).
      *      - WS KEEP THIS ONE ENTIRELY 'AS IS'.
      ******************************************************************
      *-----------------------------------------------------------------
      *   'EFILM' - INPUT SECTION.
      *-----------------------------------------------------------------
       01 EFILMI.
          02 FILLER        PIC X(12).
          02 TRANFLL COMP  PIC  S9(4).
          02 TRANFLF       PICTURE X.
          02 FILLER REDEFINES TRANFLF.
             03 TRANFLA    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 TRANFLI       PIC X(4).
          02 KEYSELL COMP  PIC  S9(4).
          02 KEYSELF       PICTURE X.
          02 FILLER REDEFINES KEYSELF.
             03 KEYSELA    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 KEYSELI       PIC X(1).
          02 MATCHL COMP   PIC  S9(4).
          02 MATCHF        PICTURE X.
          02 FILLER REDEFINES MATCHF.
             03 MATCHA     PICTURE X.
          02 FILLER        PICTURE X(1).
          02 MATCHI        PIC X(30).
          02 DPTINCLD OCCURS 4 TIMES.
             03 DPTINCLL COMP
                           PIC  S9(4).
             03 DPTINCLF   PICTURE X.
             03 FILLER     PICTURE X(1).
             03 DPTINCLI   PIC X(8).
          02 DPTEXCLD OCCURS 4 TIMES.
             03 DPTEXCLL COMP
                           PIC  S9(4).
             03 DPTEXCLF   PICTURE X.
             03 FILLER     PICTURE X(1).
             03 DPTEXCLI   PIC X(8).
          02 EDATEAL COMP  PIC  S9(4).
          02 EDATEAF       PICTURE X.
          02 FILLER REDEFINES EDATEAF.
             03 EDATEAA    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 EDATEAI       PIC X(8).
          02 EDATEBL COMP  PIC  S9(4).
          02 EDATEBF       PICTURE X.
          02 FILLER REDEFINES EDATEBF.
             03 EDATEBA    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 EDATEBI       PIC X(8).
          02 MESSFLL COMP  PIC  S9(4).
          02 MESSFLF       PICTURE X.
          02 FILLER REDEFINES MESSFLF.
             03 MESSFLA    PICTURE X.
          02 FILLER        PICTURE X(1).
          02 MESSFLI       PIC X(79).
      *-----------------------------------------------------------------
      *   'EFILM' - OUTPUT SECTION.
      *-----------------------------------------------------------------
       01 EFILMO REDEFINES EFILMI.
          02 FILLER        PIC X(12).
          02 FILLER        PICTURE X(3).
          02 TRANFLC       PICTURE X.
          02 TRANFLO       PIC X(4).
          02 FILLER        PICTURE X(3).
          02 KEYSELC       PICTURE X.
          02 KEYSELO       PIC X(1).
          02 FILLER        PICTURE X(3).
          02 MATCHC        PICTURE X.
          02 MATCHO        PIC X(30).
          02 DFHMS1 OCCURS 4 TIMES.
             03 FILLER     PICTURE X(2).
             03 DPTINCLA   PICTURE X.
             03 DPTINCLC   PICTURE X.
             03 DPTINCLO   PIC X(8).
          02 DFHMS2 OCCURS 4 TIMES.
             03 FILLER     PICTURE X(2).
             03 DPTEXCLA   PICTURE X.
             03 DPTEXCLC   PICTURE X.
             03 DPTEXCLO   PIC X(8).
          02 FILLER        PICTURE X(3).
          02 EDATEAC       PICTURE X.
          02 EDATEAO       PIC X(8).
          02 FILLER        PICTURE X(3).
          02 EDATEBC       PICTURE X.
          02 EDATEBO       PIC X(8).
          02 FILLER        PICTURE X(3).
          02 MESSFLC       PICTURE X.
          02 MESSFLO       PIC X(79).
      *-----------------------------------------------------------------
      *   'EFILM' - END OF MAP.
      *-----------------------------------------------------------------
      ******************************************************************
      *   'ELSTMAP' - END OF MAPSET.
      ******************************************************************