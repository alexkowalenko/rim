      SUBROUTINE RNAMER(*)
      INCLUDE 'syspar.d'
C
C     SUBROUTINE TO RENAME A RELATION
C
      INCLUDE 'ascpar.d'
      INCLUDE 'tokens.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tupler.d'
      INCLUDE 'attble.d'
      INCLUDE 'dclar1.d'
      LOGICAL EQKEYW
      LOGICAL NE,EQ
C
C
C     CHECK FOR A DATABASE
C
      IF (.NOT.DFLAG) THEN
         CALL WARN(2,0,0)
         GOTO 999
      ENDIF
C
C
      IF(ITEMS.NE.5) GO TO 900
      IF(.NOT.EQKEYW(4,'TO')) GO TO 900
      IF(.NOT.TOKTYP(3,KXNAME)) THEN
        CALL WARN(7,ASCREC(IDP(3)),0)
        GOTO 999
      ENDIF
      IF(.NOT.TOKTYP(5,KXNAME)) THEN
        CALL WARN(7,ASCREC(IDP(5)),0)
        GOTO 999
      ENDIF
      CALL LXSREC(5,RNAME1,ZC)
      I = LOCREL(RNAME1)
      IF(I.EQ.0) THEN
C  NEW NAME IS A DUPLICATE.
        CALL WARN(5,RNAME1,0)
        GO TO 999
      ENDIF
      CALL LXSREC(3,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
        CALL WARN(1,RNAME,0)
        GOTO 999
      ENDIF
      I = LOCPRM(NAME,2)
      IF(I.NE.0) THEN
C        FAILS MODIFY PERMISSION
         CALL WARN(8,0,0)
         GO TO 999
      ENDIF
C
C  CHANGE EVERYTHING NEEDED FOR THE RELATION.
C
      CALL RELGET(ISTAT)
      CALL LXSREC(5,RNAME1,ZC)
      CALL ZMOVE(NAME,RNAME1)
      CALL RELPUT
      I = LOCATT(BLANK,RNAME)
      IF(I.NE.0) GO TO 300
C
200   CALL ATTGET(ISTAT)
      IF (ISTAT.EQ.0) THEN
         CALL ZMOVE(RELNAM,RNAME1)
         CALL ATTPUT(ISTAT)
         GO TO 200
      ENDIF
C
C     ALSO RENAME IN THE LINKS TABLES
C
300   IF (LOCLNK(BLANK).NE.0) GOTO 800
400   CALL LNKGET(STATUS)
      IF (STATUS.NE.0) GOTO 800
      IF (EQ(R1NAME,RNAME )) CALL ZMOVE(R1NAME,RNAME1)
      IF (EQ(R2NAME,RNAME )) CALL ZMOVE(R2NAME,RNAME1)
      CALL LNKPUT(STATUS)
      GOTO 400
C
C
800   CALL MSG(' ','TABLE ''','+')
      CALL AMSG(RNAME,-ZC,'+')
      CALL MSG(' ',''' RENAMED TO ''','+')
      CALL AMSG(RNAME1,-ZC,'+')
      CALL MSG(' ',''',',' ')
      GOTO 999
C
C     SYNTAX ERROR
C
900   CALL WARN(4,0,0)
C
999   RETURN 1
      END
