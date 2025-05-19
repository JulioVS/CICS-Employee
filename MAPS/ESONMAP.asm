* ---------------------------------------------------------------------
*  CICS PLURALSIGHT 'EMPLOYEE APP'.
*    - SIGN-ON MAPSET.
* ---------------------------------------------------------------------
* ---------------------------------------------------------------------
*  GLOBAL SETTINGS.
* ---------------------------------------------------------------------
ESONMAP  DFHMSD MODE=INOUT,                                            X
               CTRL=(FREEKB,FRSET),                                    X
               CURSLOC=YES,                                            X
               DSATTS=COLOR,                                           X
               MAPATTS=(COLOR,HILIGHT),                                X
               STORAGE=AUTO,                                           X
               LANG=COBOL,                                             X
               TIOAPFX=YES,                                            X
               TYPE=&SYSPARM
* ---------------------------------------------------------------------
*  SIGN-ON MAP.
* ---------------------------------------------------------------------
ESONM    DFHMDI SIZE=(24,80),                                          X
               LINE=1,                                                 X
               COLUMN=1
*
*  HEADING SECTION.
*
TRANID   DFHMDF POS=(1,1),                                             X
               LENGTH=4,                                               X
               ATTRB=(ASKIP,NORM)
         DFHMDF POS=(1,25),                                            X
               LENGTH=28,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               INITIAL='Employee Application Sign On'
         DFHMDF POS=(3,1),                                             X
               LENGTH=66,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               INITIAL='Please enter your Employee Application User Id X
               and Password below.'
*
*  DETAIL SECTION.
*
         DFHMDF POS=(5,20),                                            X
               LENGTH=8,                                               X
               ATTRB=(ASKIP,NORM),                                     X
               INITIAL='User Id:'
USERID   DFHMDF POS=(5,29),                                            X
               LENGTH=8,                                               X
               ATTRB=(UNPROT,BRT,IC),                                  X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(5,38),                                            X
               LENGTH=13,                                              X
               ATTRB=(ASKIP,NORM),                                     X
               INITIAL='    Password:'
PASSWD   DFHMDF POS=(5,53),                                            X
               LENGTH=8,                                               X
               ATTRB=(UNPROT,DRK),                                     X
               HILIGHT=UNDERLINE
*
         DFHMDF POS=(5,62),                                            X
               LENGTH=0
*
*  MESSAGE SECTION.
*
MESS     DFHMDF POS=(7,1),                                             X
               LENGTH=79,                                              X
               ATTRB=(ASKIP,NORM)
*
*  AID KEY SECTION.
*
         DFHMDF POS=(9,1),                                             X
               LENGTH=8,                                               X
               ATTRB=(ASKIP,NORM),                                     X
               INITIAL='PF3 Exit'
* ---------------------------------------------------------------------
*  ENDING SECTION.
* ---------------------------------------------------------------------
         DFHMSD TYPE=FINAL
         END
