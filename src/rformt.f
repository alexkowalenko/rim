      SUBROUTINE RFORMT(*)

         USE Globals, only : DFLAG
         Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
         USE Lexer, only: LXSREC
         USE Message, only: WARN
         USE Text, only : BLANK
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C
C     REFORMAT AN ATTRIBUTE
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'files.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'dclar1.inc'
         INCLUDE 'rimcom.inc'
         LOGICAL NE,EQ
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C     CHECK SYNTAX
C
         IF(.NOT.EQKEYW(3,'TO')) GO TO 900
         IF((ITEMS.GT.4).AND.(.NOT.EQKEYW(5,'IN'))) GO TO 900
         IF((ITEMS.NE.4).AND.(ITEMS.NE.6)) GO TO 900
         IF( .NOT.TOKTYP(2,KXNAME) ) THEN
            CALL WARN(7,ASCREC(IDP(2)))
            GOTO 999
         ENDIF
         CALL LXSREC(2,ANAME1,ZC)
C
C     LOOK FOR RELATION
C
         CALL ZMOVE(RNAME1,BLANK)
         IFLAG = 0
         IF (EQKEYW(5,'IN')) THEN
            IFLAG = 1
            CALL LXSREC(6,RNAME1,ZC)
C  CHECK THAT RELATION EXISTS
            I = LOCREL(RNAME1)
            IF(I.NE.0) THEN
               CALL WARN(1,RNAME1,BLANK)
               GO TO 999
            ENDIF
         ENDIF
C
C     SEE IF ANAME1 EXISTS
C
         I = LOCATT(ANAME1,RNAME1)
         IF(I.NE.0) GO TO 910
         CALL ATTGET(STATUS)
         IF(STATUS.NE.0) GO TO 910
C
C     GET NEW FORMAT
C
         CALL LXFMT(4,ATTYPE,NEWFMT,FMTLEN)
         IF (NEWFMT.EQ.0) GOTO 999
C
C     REFORMAT ATTRIBUTES
C
  200    I = LOCATT(ANAME1,RNAME1)
         NUMT = 0
  210    CALL ATTGET(STATUS)
         IF(STATUS.NE.0) GO TO 300
C
C     CHECK FOR PERMISSION
C
         I = LOCREL(RELNAM)
         I = LOCPRM(RELNAM,2)
         IF(I.EQ.0) GO TO 220
         IF(IFLAG.NE.0) GO TO 930
         GO TO 210
C
  220    NUMT = NUMT + 1
         ATTFOR = NEWFMT
         CALL ATTPUT(STATUS)
         IF(IFLAG.EQ.0) GO TO 210
C
  300    IF (IFLAG.NE.0) THEN
            CALL MSG(' ',' ','+')
            CALL IMSG(NUMT,5,'+')
            CALL MSG(' ',' TABLES MODIFIED.',' ')
         ENDIF
         GOTO 999
C
C     BAD SYNTAX
C
  900    CALL WARN(4)
         GO TO 999
C
C     ANAME1 NOT THERE
C
  910    CALL WARN(3,ANAME1,RNAME1)
         GO TO 999
C
  930    CALL WARN(8)
         GO TO 999
C
C     ALL DONE
C
  999    CONTINUE
         RETURN 1
      END
