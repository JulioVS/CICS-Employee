* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - MENU 'C' MAPSET.
*    - CURSOR POSITION VERSION.
* ---------------------------------------------------------------------
* ---------------------------------------------------------------------
*  GLOBAL SETTINGS.
* ---------------------------------------------------------------------
EMNUMAP  DFHMSD MODE=INOUT,                                            X
               CTRL=(FREEKB,FRSET),                                    X
               CURSLOC=YES,                                            X
               DSATTS=COLOR,                                           X
               MAPATTS=(COLOR,HILIGHT),                                X
               STORAGE=AUTO,                                           X
               LANG=COBOL,                                             X
               TIOAPFX=YES,                                            X
               TYPE=&SYSPARM
* ---------------------------------------------------------------------
*  MAIN MENU MAP.
* ---------------------------------------------------------------------
EMNUM    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*
*  HEADING SECTION.
*
TRANID   DFHMDF POS=(1,1),LENGTH=4,ATTRB=(ASKIP,NORM)
         DFHMDF POS=(1,29),LENGTH=20,ATTRB=(ASKIP,NORM),               X
               INITIAL='Employee Application'
*
LOGDIN   DFHMDF POS=(2,69),LENGTH=8,ATTRB=(ASKIP,NORM)
*
*  DETAIL SECTION.
*
         DFHMDF POS=(3,1),LENGTH=79,ATTRB=(ASKIP,NORM),                X
               INITIAL='Position the Cursor Next to Your Selection and X
               Press Enter'
*
SEL1     DFHMDF POS=(5,8),LENGTH=1,ATTRB=(UNPROT,BRT,IC),              X
               HILIGHT=UNDERLINE
         DFHMDF POS=(5,10),LENGTH=50,ATTRB=(ASKIP,NORM),               X
               INITIAL='List Employees'
*
SEL2     DFHMDF POS=(6,8),LENGTH=1,ATTRB=(UNPROT,BRT),                 X
               HILIGHT=UNDERLINE
         DFHMDF POS=(6,10),LENGTH=50,ATTRB=(ASKIP,NORM),               X
               INITIAL='View Employee Details'
*
*  MESSAGE SECTION.
*
MESS     DFHMDF POS=(23,1),LENGTH=79,ATTRB=(ASKIP,BRT)
*
*  AID KEY SECTION.
*
         DFHMDF POS=(24,1),LENGTH=79,ATTRB=(ASKIP,NORM),               X
               INITIAL='PF3 Exit  PF10 Sign Off'
* ---------------------------------------------------------------------
*  ENDING SECTION.
* ---------------------------------------------------------------------
         DFHMSD TYPE=FINAL
         END
