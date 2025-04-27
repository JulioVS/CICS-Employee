* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - LIST EMPLOYESS MAPSET.
* ---------------------------------------------------------------------
ELSTMAP  DFHMSD MODE=INOUT,                                            X
               CTRL=(FREEKB,FRSET),                                    X
               CURSLOC=YES,                                            X
               DSATTS=COLOR,                                           X
               MAPATTS=(COLOR,HILIGHT),                                X
               STORAGE=AUTO,                                           X
               LANG=COBOL,                                             X
               TIOAPFX=YES,                                            X
               TYPE=&SYSPARM
* ---------------------------------------------------------------------
*  LIST EMPLOYESS MAP.
* ---------------------------------------------------------------------
*
*  HEADING SECTION.
*
ELSTM    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
TRANID   DFHMDF POS=(1,1),LENGTH=4,ATTRB=(ASKIP,NORM)
         DFHMDF POS=(1,33),LENGTH=13,ATTRB=(ASKIP,NORM),               X
               INITIAL='Employee List'
         DFHMDF POS=(1,69),LENGTH=4,ATTRB=(ASKIP,NORM),                X
               INITIAL='Page'
PAGEN    DFHMDF POS=(1,74),ATTRB=(ASKIP,NORM),PICOUT='ZZZZZ9'
*
*  FILTER SECTION.
*
         DFHMDF POS=(3,1),LENGTH=9, ATTRB=(ASKIP,NORM),                X
               INITIAL='Filters: '
FLTRS    DFHMDF POS=(3,11),LENGTH=69,ATTRB=(ASKIP,NORM)
*
*  COLUMN HEADINGS.
*
         DFHMDF POS=(5,1),LENGTH=39,ATTRB=(ASKIP,BRT),                 X
               INITIAL=' Emp ID   Primary Name                 '
         DFHMDF POS=(5,41),LENGTH=39,ATTRB=(ASKIP,BRT),                X
               INITIAL=' Job Title                      Dept ID'
*
*  DETAIL SECTION.
*
*  ----- LINE 1 -----
SELCT01  DFHMDF POS=(6,1),LENGTH=1,ATTRB=(UNPROT,IC),HILIGHT=UNDERLINE
EMPID01  DFHMDF POS=(6,3),LENGTH=8,ATTRB=(ASKIP,NORM)
PRMNM01  DFHMDF POS=(6,12),LENGTH=29,ATTRB=(ASKIP,NORM)
JOBTL01  DFHMDF POS=(6,42),LENGTH=29,ATTRB=(ASKIP,NORM)
DPTID01  DFHMDF POS=(6,72),LENGTH=8,ATTRB=(ASKIP,NORM)
*  ----- LINE 2 -----
SELCT02  DFHMDF POS=(7,1),LENGTH=1,ATTRB=(UNPROT),HILIGHT=UNDERLINE
EMPID02  DFHMDF POS=(7,3),LENGTH=8,ATTRB=(ASKIP,NORM)
PRMNM02  DFHMDF POS=(7,12),LENGTH=29,ATTRB=(ASKIP,NORM)
JOBTL02  DFHMDF POS=(7,42),LENGTH=29,ATTRB=(ASKIP,NORM)
DPTID02  DFHMDF POS=(7,72),LENGTH=8,ATTRB=(ASKIP,NORM)
*  ----- LINE 3 -----
SELCT03  DFHMDF POS=(8,1),LENGTH=1,ATTRB=(UNPROT),HILIGHT=UNDERLINE
EMPID03  DFHMDF POS=(8,3),LENGTH=8,ATTRB=(ASKIP,NORM)
PRMNM03  DFHMDF POS=(8,12),LENGTH=29,ATTRB=(ASKIP,NORM)
JOBTL03  DFHMDF POS=(8,42),LENGTH=29,ATTRB=(ASKIP,NORM)
DPTID03  DFHMDF POS=(8,72),LENGTH=8,ATTRB=(ASKIP,NORM)
*
*  MESSAGE SECTION.
*
MESS     DFHMDF POS=(23,1),LENGTH=79,ATTRB=(ASKIP,NORM)
*
*  AID KEY SECTION.
*
         DFHMDF POS=(24,1),LENGTH=79,ATTRB=(ASKIP,NORM),               X
               INITIAL='ENTER Details  PF3 Filters  PF7 Prev  PF8 Next X
               PF10 Sign Off  PF12 Cancel'
* ---------------------------------------------------------------------
*  ENDING.
* ---------------------------------------------------------------------
         DFHMSD TYPE=FINAL
         END
