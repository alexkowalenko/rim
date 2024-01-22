      SUBROUTINE LODREL(NUMELE)
      INCLUDE 'syspar.inc'
C
C     LOAD THE RELATION DESCRIPTION
C
      INCLUDE 'ascpar.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'tokens.inc'
      INCLUDE 'buffer.inc'
      INCLUDE 'files.inc'
C
      LOGICAL EQKEYW
      INCLUDE 'dclar1.inc'
C
C  READ RELATION DATA.
C
100   CALL LODREC
      IF(ITEMS.EQ.1) GO TO 999
      IF(ITEMS.LT.3) THEN
C       SYNATX ERROR
        CALL WARN(4,0,0)
        GO TO 800
      ENDIF
C
C  CHECK FOR VALID RELATION NAME.
C
      IF(.NOT.TOKTYP(1,KXNAME)) THEN
        CALL MSG('E','THE TABLE NAME IS NOT VALID.',' ')
        GO TO 800
      ENDIF
      CALL LXSREC(1,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.EQ.0) THEN
        CALL WARN(5,RNAME,0)
        GOTO 800
      ENDIF
C
C  CHECK ATTRIBUTE NAMES.
C
      JUNK = 1
      IF(NUMELE.GT.0) JUNK = BLKLOC(10)
      CALL CHKATT(BUFFER(JUNK),NUMELE,ERROR)
      IF (ERROR.EQ.0) GO TO 100
C
C
800   CALL MSG(' ','STILL DEFINING TABLES',' ')
      GOTO 100
C
C  END RELATION PROCESSING.
C
  999 CONTINUE
      RETURN
      END
