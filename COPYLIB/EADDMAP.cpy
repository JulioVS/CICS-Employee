       01  EADDMI.
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
           02  EMPLIDL    COMP  PIC  S9(4).
           02  EMPLIDF    PICTURE X.
           02  FILLER REDEFINES EMPLIDF.
             03 EMPLIDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  EMPLIDI  PIC X(8).
           02  PRNAMEL    COMP  PIC  S9(4).
           02  PRNAMEF    PICTURE X.
           02  FILLER REDEFINES PRNAMEF.
             03 PRNAMEA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  PRNAMEI  PIC X(38).
           02  HONORL    COMP  PIC  S9(4).
           02  HONORF    PICTURE X.
           02  FILLER REDEFINES HONORF.
             03 HONORA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  HONORI  PIC X(8).
           02  SHNAMEL    COMP  PIC  S9(4).
           02  SHNAMEF    PICTURE X.
           02  FILLER REDEFINES SHNAMEF.
             03 SHNAMEA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SHNAMEI  PIC X(38).
           02  FLNAMEL    COMP  PIC  S9(4).
           02  FLNAMEF    PICTURE X.
           02  FILLER REDEFINES FLNAMEF.
             03 FLNAMEA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  FLNAMEI  PIC X(79).
           02  JBTITLL    COMP  PIC  S9(4).
           02  JBTITLF    PICTURE X.
           02  FILLER REDEFINES JBTITLF.
             03 JBTITLA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  JBTITLI  PIC X(38).
           02  DEPTIDL    COMP  PIC  S9(4).
           02  DEPTIDF    PICTURE X.
           02  FILLER REDEFINES DEPTIDF.
             03 DEPTIDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DEPTIDI  PIC X(8).
           02  DEPTNML    COMP  PIC  S9(4).
           02  DEPTNMF    PICTURE X.
           02  FILLER REDEFINES DEPTNMF.
             03 DEPTNMA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DEPTNMI  PIC X(38).
           02  STDATEL    COMP  PIC  S9(4).
           02  STDATEF    PICTURE X.
           02  FILLER REDEFINES STDATEF.
             03 STDATEA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  STDATEI  PIC X(10).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  MESSI  PIC X(79).
       01  EADDMO REDEFINES EADDMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  LOGDINC    PICTURE X.
           02  LOGDINO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  EMPLIDC    PICTURE X.
           02  EMPLIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PRNAMEC    PICTURE X.
           02  PRNAMEO  PIC X(38).
           02  FILLER PICTURE X(3).
           02  HONORC    PICTURE X.
           02  HONORO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  SHNAMEC    PICTURE X.
           02  SHNAMEO  PIC X(38).
           02  FILLER PICTURE X(3).
           02  FLNAMEC    PICTURE X.
           02  FLNAMEO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  JBTITLC    PICTURE X.
           02  JBTITLO  PIC X(38).
           02  FILLER PICTURE X(3).
           02  DEPTIDC    PICTURE X.
           02  DEPTIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  DEPTNMC    PICTURE X.
           02  DEPTNMO  PIC X(38).
           02  FILLER PICTURE X(3).
           02  STDATEC    PICTURE X.
           02  STDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSO  PIC X(79).
