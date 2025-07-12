       01  EUPDMI.
           02  FILLER PIC X(12).
           02  TRANIDL    COMP  PIC  S9(4).
           02  TRANIDF    PICTURE X.
           02  FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  TRANIDI  PIC X(4).
           02  SELBYL    COMP  PIC  S9(4).
           02  SELBYF    PICTURE X.
           02  FILLER REDEFINES SELBYF.
             03 SELBYA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  SELBYI  PIC X(7).
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
           02  ENDATEL    COMP  PIC  S9(4).
           02  ENDATEF    PICTURE X.
           02  FILLER REDEFINES ENDATEF.
             03 ENDATEA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  ENDATEI  PIC X(10).
           02  APPRDTL    COMP  PIC  S9(4).
           02  APPRDTF    PICTURE X.
           02  FILLER REDEFINES APPRDTF.
             03 APPRDTA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  APPRDTI  PIC X(10).
           02  APPRRSL    COMP  PIC  S9(4).
           02  APPRRSF    PICTURE X.
           02  FILLER REDEFINES APPRRSF.
             03 APPRRSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  APPRRSI  PIC X(30).
           02  DELFLGL    COMP  PIC  S9(4).
           02  DELFLGF    PICTURE X.
           02  FILLER REDEFINES DELFLGF.
             03 DELFLGA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DELFLGI  PIC X(1).
           02  DELDSCL    COMP  PIC  S9(4).
           02  DELDSCF    PICTURE X.
           02  FILLER REDEFINES DELDSCF.
             03 DELDSCA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DELDSCI  PIC X(9).
           02  DELDTL    COMP  PIC  S9(4).
           02  DELDTF    PICTURE X.
           02  FILLER REDEFINES DELDTF.
             03 DELDTA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DELDTI  PIC X(10).
           02  MESSL    COMP  PIC  S9(4).
           02  MESSF    PICTURE X.
           02  FILLER REDEFINES MESSF.
             03 MESSA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  MESSI  PIC X(79).
           02  HLPPF4L    COMP  PIC  S9(4).
           02  HLPPF4F    PICTURE X.
           02  FILLER REDEFINES HLPPF4F.
             03 HLPPF4A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  HLPPF4I  PIC X(7).
           02  HLPPF7L    COMP  PIC  S9(4).
           02  HLPPF7F    PICTURE X.
           02  FILLER REDEFINES HLPPF7F.
             03 HLPPF7A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  HLPPF7I  PIC X(7).
           02  HLPPF8L    COMP  PIC  S9(4).
           02  HLPPF8F    PICTURE X.
           02  FILLER REDEFINES HLPPF8F.
             03 HLPPF8A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  HLPPF8I  PIC X(7).
           02  HLPPF11L    COMP  PIC  S9(4).
           02  HLPPF11F    PICTURE X.
           02  FILLER REDEFINES HLPPF11F.
             03 HLPPF11A    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  HLPPF11I  PIC X(10).
       01  EUPDMO REDEFINES EUPDMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  SELBYC    PICTURE X.
           02  SELBYO  PIC X(7).
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
           02  ENDATEC    PICTURE X.
           02  ENDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  APPRDTC    PICTURE X.
           02  APPRDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  APPRRSC    PICTURE X.
           02  APPRRSO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  DELFLGC    PICTURE X.
           02  DELFLGO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  DELDSCC    PICTURE X.
           02  DELDSCO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  DELDTC    PICTURE X.
           02  DELDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  MESSC    PICTURE X.
           02  MESSO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  HLPPF4C    PICTURE X.
           02  HLPPF4O  PIC X(7).
           02  FILLER PICTURE X(3).
           02  HLPPF7C    PICTURE X.
           02  HLPPF7O  PIC X(7).
           02  FILLER PICTURE X(3).
           02  HLPPF8C    PICTURE X.
           02  HLPPF8O  PIC X(7).
           02  FILLER PICTURE X(3).
           02  HLPPF11C    PICTURE X.
           02  HLPPF11O  PIC X(10).
       01  EDELMI.
           02  FILLER PIC X(12).
           02  DELEMPL    COMP  PIC  S9(4).
           02  DELEMPF    PICTURE X.
           02  FILLER REDEFINES DELEMPF.
             03 DELEMPA    PICTURE X.
           02  FILLER   PICTURE X(1).
           02  DELEMPI  PIC X(8).
       01  EDELMO REDEFINES EDELMI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  DELEMPC    PICTURE X.
           02  DELEMPO  PIC X(8).
