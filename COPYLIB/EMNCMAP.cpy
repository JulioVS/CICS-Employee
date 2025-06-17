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
           02  SEL1L    COMP  PIC  S9(4).
           02  SEL1F    PICTURE X.
           02  FILLER REDEFINES SEL1F.
             03 SEL1A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SEL1I  PIC X(1).
           02  SEL2L    COMP  PIC  S9(4).
           02  SEL2F    PICTURE X.
           02  FILLER REDEFINES SEL2F.
             03 SEL2A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SEL2I  PIC X(1).
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
           02  SEL1C    PICTURE X.
           02  SEL1O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SEL2C    PICTURE X.
           02  SEL2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSO  PIC X(79).
