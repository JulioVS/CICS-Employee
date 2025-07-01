* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - ADD EMPLOYEE MAPSET.
* ---------------------------------------------------------------------
* ---------------------------------------------------------------------
*  GLOBAL SETTINGS.
* ---------------------------------------------------------------------
EADDMAP  DFHMSD MODE=INOUT,                                            X
               CTRL=(FREEKB,FRSET),                                    X
               CURSLOC=YES,                                            X
               DSATTS=COLOR,                                           X
               MAPATTS=(COLOR,HILIGHT),                                X
               STORAGE=AUTO,                                           X
               LANG=COBOL,                                             X
               TIOAPFX=YES,                                            X
               TYPE=&SYSPARM
* ---------------------------------------------------------------------
*  ADD EMPLOYEE MAP.
* ---------------------------------------------------------------------
EADDM    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*
*  HEADING SECTION.
*
TRANID   DFHMDF POS=(1,1),LENGTH=4,ATTRB=(ASKIP,NORM)
         DFHMDF POS=(1,34),LENGTH=12,ATTRB=(ASKIP,NORM),               X
               INITIAL='Add Employee'
*
LOGDIN   DFHMDF POS=(2,69),LENGTH=8,ATTRB=(ASKIP,NORM)
*
*  DETAIL SECTION.
*
         DFHMDF POS=(3,1),LENGTH=3,ATTRB=(ASKIP,NORM),INITIAL='Id:'
EMPLID   DFHMDF POS=(3,5),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(3,14),LENGTH=21,ATTRB=(ASKIP,NORM),               X
               INITIAL='        Primary Name:'
PRNAME   DFHMDF POS=(3,36),LENGTH=38,ATTRB=(UNPROT,BRT,IC),            X
               HILIGHT=UNDERLINE
         DFHMDF POS=(3,75),LENGTH=0
*
         DFHMDF POS=(4,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Honorific:'
HONOR    DFHMDF POS=(4,12),LENGTH=8,ATTRB=(UNPROT,BRT),                X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(4,23),LENGTH=12,ATTRB=(ASKIP,NORM),               X
               INITIAL=' Short Name:'
SHNAME   DFHMDF POS=(4,36),LENGTH=38,ATTRB=(UNPROT,BRT),               X
               HILIGHT=UNDERLINE
         DFHMDF POS=(4,75),LENGTH=0
*
         DFHMDF POS=(5,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Full Name:'
FLNAME   DFHMDF POS=(6,1),LENGTH=79,ATTRB=(UNPROT,BRT),                X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(7,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Job Title:'
JBTITL   DFHMDF POS=(7,12),LENGTH=38,ATTRB=(UNPROT,BRT),               X
               HILIGHT=UNDERLINE
         DFHMDF POS=(7,51),LENGTH=0
*
         DFHMDF POS=(9,1),LENGTH=9,ATTRB=(ASKIP,NORM),                 X
               INITIAL='Dept. Id:'
DEPTID   DFHMDF POS=(9,11),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,20),LENGTH=13,ATTRB=(ASKIP,NORM),               X
               INITIAL='  Dept. Name:'
DEPTNM   DFHMDF POS=(9,34),LENGTH=38,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,1),LENGTH=11,ATTRB=(ASKIP,NORM),               X
               INITIAL='Start Date:'
STDATE   DFHMDF POS=(10,13),LENGTH=10,ATTRB=(UNPROT,BRT),              X
               HILIGHT=UNDERLINE
         DFHMDF POS=(10,24),LENGTH=0
*
*  MESSAGE SECTION.
*
MESS     DFHMDF POS=(23,1),LENGTH=79,ATTRB=(ASKIP,BRT)
*
*  AID KEY SECTION.
*
         DFHMDF POS=(24,1),LENGTH=79,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF3 Exit  PF4 Save  PF9 Clear  PF10 Sign Off  PX
               F12 Cancel'
* ---------------------------------------------------------------------
*  ENDING SECTION.
* ---------------------------------------------------------------------
         DFHMSD TYPE=FINAL
         END
