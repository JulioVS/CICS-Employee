       01  EMNUMI.
           02  FILLER PIC X(12).
           02  TRANIDL    COMP  PIC  S9(4).
           02  TRANIDF    PICTURE X.
           02  FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  TRANIDI  PIC X(4).
           02  LOGDINL    COMP  PIC  S9(4).
           02  LOGDINF    PICTURE X.
           02  FILLER REDEFINES LOGDINF.
             03 LOGDINA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  LOGDINI  PIC X(8).
           02  SELECTL    COMP  PIC  S9(4).
           02  SELECTF    PICTURE X.
           02  FILLER REDEFINES SELECTF.
             03 SELECTA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SELECTI  PIC X(1).
           02  ADDEMPL    COMP  PIC  S9(4).
           02  ADDEMPF    PICTURE X.
           02  FILLER REDEFINES ADDEMPF.
             03 ADDEMPA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  ADDEMPI  PIC X(50).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  MESSI  PIC X(79).
       01  EMNUMO REDEFINES EMNUMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  LOGDINC    PICTURE X.
           02  LOGDINO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  SELECTC    PICTURE X.
           02  SELECTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ADDEMPC    PICTURE X.
           02  ADDEMPO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSO  PIC X(79).
