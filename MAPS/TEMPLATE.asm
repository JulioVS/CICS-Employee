* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - TEMPLATE MAPSET.
* ---------------------------------------------------------------------
* ---------------------------------------------------------------------
*  GLOBAL SETTINGS.
* ---------------------------------------------------------------------
EXXXMAP  DFHMSD MODE=INOUT,                                            X
               CTRL=(FREEKB,FRSET),                                    X
               CURSLOC=YES,                                            X
               DSATTS=COLOR,                                           X
               MAPATTS=(COLOR,HILIGHT),                                X
               STORAGE=AUTO,                                           X
               LANG=COBOL,                                             X
               TIOAPFX=YES,                                            X
               TYPE=&SYSPARM
* ---------------------------------------------------------------------
*  FIRST MAP.
* ---------------------------------------------------------------------
EXXXM    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1,COLOR=TURQUOISE
*
*  HEADING SECTION.
*
TRANID   DFHMDF POS=(1,1),LENGTH=4,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(1,34),LENGTH=12,ATTRB=(ASKIP,BRT),                X
               INITIAL='Screen Title'
*
*  DETAIL SECTION.
*
*
*  MESSAGE SECTION.
*
MESS     DFHMDF POS=(23,1),LENGTH=79,ATTRB=(ASKIP,BRT)
*
*  AID KEY SECTION.
*
         DFHMDF POS=(24,1),LENGTH=18,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF3 Save and Exit'
HLPPF7   DFHMDF POS=(24,20),LENGTH=9,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF7 Prev '
HLPPF8   DFHMDF POS=(24,30),LENGTH=9,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF8 Next '
         DFHMDF POS=(24,40),LENGTH=26,ATTRB=(ASKIP,NORM),              X
               INITIAL='PF10 Sign Off  PF12 Cancel'
* ---------------------------------------------------------------------
*  ENDING SECTION.
* ---------------------------------------------------------------------
         DFHMSD TYPE=FINAL
         END
