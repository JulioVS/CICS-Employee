* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - UPDATE EMPLOYEE DETAILS MAPSET.
* ---------------------------------------------------------------------
* ---------------------------------------------------------------------
*  GLOBAL SETTINGS.
* ---------------------------------------------------------------------
EUPDMAP  DFHMSD MODE=INOUT,                                            X
               CTRL=(FREEKB,FRSET),                                    X
               CURSLOC=YES,                                            X
               DSATTS=COLOR,                                           X
               MAPATTS=(COLOR,HILIGHT),                                X
               STORAGE=AUTO,                                           X
               LANG=COBOL,                                             X
               TIOAPFX=YES,                                            X
               TYPE=&SYSPARM
* ---------------------------------------------------------------------
*  UPDATE DETAILS MAP.
* ---------------------------------------------------------------------
EUPDM    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*
*  HEADING SECTION.
*
TRANID   DFHMDF POS=(1,1),LENGTH=4,ATTRB=(ASKIP,NORM)
         DFHMDF POS=(1,32),LENGTH=16,ATTRB=(ASKIP,NORM),               X
               INITIAL='Employee Details'
SELBY    DFHMDF POS=(1,72),LENGTH=7,ATTRB=(ASKIP,NORM),                X
               INITIAL='By Id  '
*
LOGDIN   DFHMDF POS=(2,69),LENGTH=8,ATTRB=(ASKIP,NORM)
*
*  DETAIL SECTION.
*
         DFHMDF POS=(3,1),LENGTH=3,ATTRB=(ASKIP,NORM),INITIAL='Id:'
EMPLID   DFHMDF POS=(3,5),LENGTH=8,ATTRB=(UNPROT,BRT,IC),              X
               HILIGHT=UNDERLINE
*
PRNAMEL  DFHMDF POS=(3,14),LENGTH=21,ATTRB=(ASKIP,NORM),               X
               INITIAL='        Primary Name:'
PRNAME   DFHMDF POS=(3,36),LENGTH=38,ATTRB=(UNPROT,BRT),               X
               HILIGHT=UNDERLINE
         DFHMDF POS=(3,75),LENGTH=0
*
HONORL   DFHMDF POS=(4,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Honorific:'
HONOR    DFHMDF POS=(4,12),LENGTH=8,ATTRB=(UNPROT,BRT),                X
               HILIGHT=UNDERLINE
*
SHNAMEL  DFHMDF POS=(4,21),LENGTH=14,ATTRB=(ASKIP,NORM),               X
               INITIAL='   Short Name:'
SHNAME   DFHMDF POS=(4,36),LENGTH=38,ATTRB=(UNPROT,BRT),               X
               HILIGHT=UNDERLINE
         DFHMDF POS=(4,75),LENGTH=0
*
FLNAMEL  DFHMDF POS=(5,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Full Name:'
FLNAME   DFHMDF POS=(6,1),LENGTH=79,ATTRB=(UNPROT,BRT),                X
               HILIGHT=UNDERLINE
*
JBTITLL  DFHMDF POS=(7,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Job Title:'
JBTITL   DFHMDF POS=(7,12),LENGTH=38,ATTRB=(UNPROT,BRT),               X
               HILIGHT=UNDERLINE
         DFHMDF POS=(7,51),LENGTH=0
*
DEPTIDL  DFHMDF POS=(9,1),LENGTH=9,ATTRB=(ASKIP,NORM),                 X
               INITIAL='Dept. Id:'
DEPTID   DFHMDF POS=(9,11),LENGTH=8,ATTRB=(UNPROT,BRT),                X
               HILIGHT=UNDERLINE
*
DEPTNML  DFHMDF POS=(9,20),LENGTH=13,ATTRB=(ASKIP,NORM),               X
               INITIAL='  Dept. Name:'
DEPTNM   DFHMDF POS=(9,34),LENGTH=38,ATTRB=(ASKIP,BRT)
*
STDATEL  DFHMDF POS=(10,1),LENGTH=11,ATTRB=(ASKIP,NORM),               X
               INITIAL='Start Date:'
STDATE   DFHMDF POS=(10,13),LENGTH=10,ATTRB=(UNPROT,BRT),              X
               HILIGHT=UNDERLINE
*
ENDATEL  DFHMDF POS=(10,24),LENGTH=9,ATTRB=(ASKIP,NORM),               X
               INITIAL='End Date:'
ENDATE   DFHMDF POS=(10,34),LENGTH=10,ATTRB=(UNPROT,BRT),              X
               HILIGHT=UNDERLINE
         DFHMDF POS=(10,45),LENGTH=0
*
APPRDTL  DFHMDF POS=(11,1),LENGTH=20,ATTRB=(ASKIP,NORM),               X
               INITIAL='Last Appraisal Date:'
APPRDT   DFHMDF POS=(11,22),LENGTH=10,ATTRB=(UNPROT,BRT),              X
               HILIGHT=UNDERLINE
*
APPRRSL  DFHMDF POS=(11,33),LENGTH=7,ATTRB=(ASKIP,NORM),               X
               INITIAL='Result:'
APPRRS   DFHMDF POS=(11,41),LENGTH=30,ATTRB=(UNPROT,BRT),              X
               HILIGHT=UNDERLINE
         DFHMDF POS=(11,72),LENGTH=0
*
DELFLGL  DFHMDF POS=(12,1),LENGTH=16,ATTRB=(ASKIP,NORM),               X
               INITIAL='Record Deletion:'
DELFLG   DFHMDF POS=(12,18),LENGTH=1,ATTRB=(UNPROT,BRT),               X
               HILIGHT=UNDERLINE
DELDSC   DFHMDF POS=(12,20),LENGTH=9,ATTRB=(ASKIP,BRT)
*
ASOFL    DFHMDF POS=(12,30),LENGTH=6,ATTRB=(ASKIP,NORM),               X
               INITIAL='as of:'
DELDT    DFHMDF POS=(12,37),LENGTH=10,ATTRB=(ASKIP,BRT)
*
*  MESSAGE SECTION.
*
MESS     DFHMDF POS=(23,1),LENGTH=79,ATTRB=(ASKIP,BRT)
*
*  AID KEY SECTION.
*
         DFHMDF POS=(24,1),LENGTH=21,ATTRB=(ASKIP,NORM),               X
               INITIAL='ENTER Val  PF3 Exit  '
HLPPF4   DFHMDF POS=(24,23),LENGTH=10,ATTRB=(ASKIP,NORM),              X
               INITIAL='PF4 Save  '
HLPPF7   DFHMDF POS=(24,34),LENGTH=9,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF7 Prev '
HLPPF8   DFHMDF POS=(24,44),LENGTH=9,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF8 Next '
         DFHMDF POS=(24,54),LENGTH=26,ATTRB=(ASKIP,NORM),              X
               INITIAL='PF10 Sign Off  PF12 Cancel'
* ---------------------------------------------------------------------
*  ENDING SECTION.
* ---------------------------------------------------------------------
         DFHMSD TYPE=FINAL
         END
