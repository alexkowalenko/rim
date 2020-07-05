      SUBROUTINE REMLNK(*)
      INCLUDE 'syspar.d'
C
C     REMOVE A LINK FROM THE DATABASE
C
      INCLUDE 'ascpar.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'tokens.d'
      INCLUDE 'tuplel.d'
      INCLUDE 'lnktbl.d'
      INCLUDE 'flags.d'
      LOGICAL EQKEYW
      LOGICAL NE
      LOGICAL EQ
      INCLUDE 'rmatts.d'
C
      INTEGER LKNAM(Z)
C
C     CHECK FOR A DATABASE
C
      IF (.NOT.DFLAG) THEN
         CALL WARN(2,0,0)
         GOTO 999
      ENDIF
C
C     MAKE SURE THE DATABASE MAY BE MODIFIED
C
      IF(.NOT.DMFLAG) THEN
         CALL WARN(RMSTAT,DBNAME,0)
         GO TO 999
      ENDIF
C
C     ONLY THE OWNER CAN DO THIS
C
      IF (NE(OWNER,USERID)) THEN
         CALL WARN(8,0,0)
         GOTO 999
      ENDIF
C
      IF(ITEMS.NE.3) GO TO 999
      CALL LXSREC(3,LKNAM,ZC)
C
C  FIND THE LINK NAME IN THE LINK TABLE.
C
      I = LOCLNK(LKNAM)
      IF(I.NE.0) THEN
         CALL MSG('E','LINK ''','+')
         CALL AMSG(LKNAM,-ZC,'+')
         CALL MSG(' ',''' IS NOT IN THE DATABASE.',' ')
         GOTO 999
      ENDIF
C
C
C  CHANGE THE LINK TABLE.
C
      CALL LNKGET(ISTAT)
      IF (LLROW.EQ.0) GO TO 999
C
C  CHANGE THE TUPLE STATUS FLAG TO DELETED.
C
      LNKTBL(1,LLROW) = 0-LNKTBL(1,LLROW)
      LNKMOD = 1
C
C     DONE
C
800   CALL MSG(' ','LINK ''','+')
      CALL AMSG(LKNAM,-ZC,'+')
      CALL MSG(' ',''' HAS BEEN REMOVED.',' ')
      IFMOD = .TRUE.
C
999   RETURN 1
      END
