       01  ELSTMI.
           02  FILLER PIC X(12).
           02  TRANIDL    COMP  PIC  S9(4).
           02  TRANIDF    PICTURE X.
           02  FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  TRANIDI  PIC X(4).
           02  PAGENL    COMP  PIC  S9(4).
           02  PAGENF    PICTURE X.
           02  FILLER REDEFINES PAGENF.
             03 PAGENA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  PAGENI  PIC X(6).
           02  FLTRSL    COMP  PIC  S9(4).
           02  FLTRSF    PICTURE X.
           02  FILLER REDEFINES FLTRSF.
             03 FLTRSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  FLTRSI  PIC X(69).
           02  SELCT01L    COMP  PIC  S9(4).
           02  SELCT01F    PICTURE X.
           02  FILLER REDEFINES SELCT01F.
             03 SELCT01A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SELCT01I  PIC X(1).
           02  EMPID01L    COMP  PIC  S9(4).
           02  EMPID01F    PICTURE X.
           02  FILLER REDEFINES EMPID01F.
             03 EMPID01A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  EMPID01I  PIC X(8).
           02  PRMNM01L    COMP  PIC  S9(4).
           02  PRMNM01F    PICTURE X.
           02  FILLER REDEFINES PRMNM01F.
             03 PRMNM01A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  PRMNM01I  PIC X(29).
           02  JOBTL01L    COMP  PIC  S9(4).
           02  JOBTL01F    PICTURE X.
           02  FILLER REDEFINES JOBTL01F.
             03 JOBTL01A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  JOBTL01I  PIC X(29).
           02  DPTID01L    COMP  PIC  S9(4).
           02  DPTID01F    PICTURE X.
           02  FILLER REDEFINES DPTID01F.
             03 DPTID01A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DPTID01I  PIC X(8).
           02  SELCT02L    COMP  PIC  S9(4).
           02  SELCT02F    PICTURE X.
           02  FILLER REDEFINES SELCT02F.
             03 SELCT02A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SELCT02I  PIC X(1).
           02  EMPID02L    COMP  PIC  S9(4).
           02  EMPID02F    PICTURE X.
           02  FILLER REDEFINES EMPID02F.
             03 EMPID02A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  EMPID02I  PIC X(8).
           02  PRMNM02L    COMP  PIC  S9(4).
           02  PRMNM02F    PICTURE X.
           02  FILLER REDEFINES PRMNM02F.
             03 PRMNM02A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  PRMNM02I  PIC X(29).
           02  JOBTL02L    COMP  PIC  S9(4).
           02  JOBTL02F    PICTURE X.
           02  FILLER REDEFINES JOBTL02F.
             03 JOBTL02A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  JOBTL02I  PIC X(29).
           02  DPTID02L    COMP  PIC  S9(4).
           02  DPTID02F    PICTURE X.
           02  FILLER REDEFINES DPTID02F.
             03 DPTID02A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DPTID02I  PIC X(8).
           02  SELCT03L    COMP  PIC  S9(4).
           02  SELCT03F    PICTURE X.
           02  FILLER REDEFINES SELCT03F.
             03 SELCT03A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SELCT03I  PIC X(1).
           02  EMPID03L    COMP  PIC  S9(4).
           02  EMPID03F    PICTURE X.
           02  FILLER REDEFINES EMPID03F.
             03 EMPID03A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  EMPID03I  PIC X(8).
           02  PRMNM03L    COMP  PIC  S9(4).
           02  PRMNM03F    PICTURE X.
           02  FILLER REDEFINES PRMNM03F.
             03 PRMNM03A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  PRMNM03I  PIC X(29).
           02  JOBTL03L    COMP  PIC  S9(4).
           02  JOBTL03F    PICTURE X.
           02  FILLER REDEFINES JOBTL03F.
             03 JOBTL03A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  JOBTL03I  PIC X(29).
           02  DPTID03L    COMP  PIC  S9(4).
           02  DPTID03F    PICTURE X.
           02  FILLER REDEFINES DPTID03F.
             03 DPTID03A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DPTID03I  PIC X(8).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  MESSI  PIC X(79).
       01  ELSTMO REDEFINES ELSTMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  PAGENC    PICTURE X.
           02  PAGENO PIC ZZZZZ9.
           02  FILLER PICTURE X(3).
           02  FLTRSC    PICTURE X.
           02  FLTRSO  PIC X(69).
           02  FILLER PICTURE X(3).
           02  SELCT01C    PICTURE X.
           02  SELCT01O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  EMPID01C    PICTURE X.
           02  EMPID01O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PRMNM01C    PICTURE X.
           02  PRMNM01O  PIC X(29).
           02  FILLER PICTURE X(3).
           02  JOBTL01C    PICTURE X.
           02  JOBTL01O  PIC X(29).
           02  FILLER PICTURE X(3).
           02  DPTID01C    PICTURE X.
           02  DPTID01O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  SELCT02C    PICTURE X.
           02  SELCT02O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  EMPID02C    PICTURE X.
           02  EMPID02O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PRMNM02C    PICTURE X.
           02  PRMNM02O  PIC X(29).
           02  FILLER PICTURE X(3).
           02  JOBTL02C    PICTURE X.
           02  JOBTL02O  PIC X(29).
           02  FILLER PICTURE X(3).
           02  DPTID02C    PICTURE X.
           02  DPTID02O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  SELCT03C    PICTURE X.
           02  SELCT03O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  EMPID03C    PICTURE X.
           02  EMPID03O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PRMNM03C    PICTURE X.
           02  PRMNM03O  PIC X(29).
           02  FILLER PICTURE X(3).
           02  JOBTL03C    PICTURE X.
           02  JOBTL03O  PIC X(29).
           02  FILLER PICTURE X(3).
           02  DPTID03C    PICTURE X.
           02  DPTID03O  PIC X(8).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSO  PIC X(79).
