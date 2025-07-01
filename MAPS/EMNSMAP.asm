* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - MENU 'S' MAPSET.
*    - SELECTION VERSION.
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
         DFHMDF POS=(3,1),LENGTH=10,ATTRB=(ASKIP,NORM),                X
               INITIAL='Selection:'
SELECT   DFHMDF POS=(3,12),LENGTH=1,ATTRB=(UNPROT,BRT,IC),             X
               HILIGHT=UNDERLINE
         DFHMDF POS=(3,14),LENGTH=0
*
         DFHMDF POS=(5,8),LENGTH=50,ATTRB=(ASKIP,NORM),                X
               INITIAL='1 List Employees'
*
         DFHMDF POS=(6,8),LENGTH=50,ATTRB=(ASKIP,NORM),                X
               INITIAL='2 View Employee Details'
*
ADDEMP   DFHMDF POS=(7,8),LENGTH=50,ATTRB=(ASKIP,NORM),                X
               INITIAL='3 Add Employee'
*
         DFHMDF POS=(8,8),LENGTH=50,ATTRB=(ASKIP,NORM),                X
               INITIAL='4 Update Employee'
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
