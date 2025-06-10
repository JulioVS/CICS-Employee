       01  ESONMI.
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
           02  USERIDL    COMP  PIC  S9(4).
           02  USERIDF    PICTURE X.
           02  FILLER REDEFINES USERIDF.
             03 USERIDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  USERIDI  PIC X(8).
           02  PASSWDL    COMP  PIC  S9(4).
           02  PASSWDF    PICTURE X.
           02  FILLER REDEFINES PASSWDF.
             03 PASSWDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  PASSWDI  PIC X(8).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  MESSI  PIC X(79).
       01  ESONMO REDEFINES ESONMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  LOGDINC    PICTURE X.
           02  LOGDINO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  USERIDC    PICTURE X.
           02  USERIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PASSWDC    PICTURE X.
           02  PASSWDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSO  PIC X(79).
