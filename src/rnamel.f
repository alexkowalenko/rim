      SUBROUTINE RNAMEL(*)

         USE Globals, only: DFLAG, DMFLAG, DBNAME, USERID, OWNER
         USE Lexer, only: ITEMS, EQKEYW, LXSREC
         USE Message, only: WARN
         USE Rim, only : RMSTAT
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     RENAME A LINK IN THE DATABASE
C
         INCLUDE 'tuplel.inc'
         INCLUDE 'lnktbl.inc'
         LOGICAL NE
         LOGICAL EQ
         INCLUDE 'rmatts.inc'
C
         INTEGER LKNAM(Z), NLKNAM(Z)
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C     MAKE SURE THE DATABASE MAY BE MODIFIED
C
         IF(.NOT.DMFLAG) THEN
            CALL WARN(RMSTAT,DBNAME)
            GO TO 999
         ENDIF
C
C     ONLY THE OWNER CAN DO THIS
C
         IF (NE(OWNER,USERID)) THEN
            CALL WARN(8)
            GOTO 999
         ENDIF
C
         IF(ITEMS.NE.5) GO TO 999
         IF (.NOT.EQKEYW(4,'TO'))  THEN
            CALL WARN(4)
            GOTO 999
         ENDIF

         CALL LXSREC(3,LKNAM,ZC)
         CALL LXSREC(5,NLKNAM,ZC)
C
C  CHECK THAT THE NEW NAME DOES NOT EXIST
C
         I = LOCLNK(NLKNAM)
         IF(I.EQ.0) THEN
            CALL MSG('E','LINK ''','+')
            CALL AMSG(NLKNAM,-ZC,'+')
            CALL MSG(' ',''' IS ALREADY IN THE DATABASE.',' ')
            GOTO 999
         ENDIF
C
C  FIND THE OLD LINK NAME IN THE LINK TABLE.
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
         CALL ZMOVE(LNAME,NLKNAM)
         CALL LNKPUT(ISTAT)
         IF (ISTAT.EQ.0) THEN
            CALL MSG(' ','LINK ''','+')
            CALL AMSG(LKNAM,-ZC,'+')
            CALL MSG(' ',''' HAS BEEN RENAMED TO ''','+')
            CALL AMSG(NLKNAM,-ZC,'+')
            CALL MSG(' ','''',' ')
         ENDIF
C
  999    RETURN 1
      END
