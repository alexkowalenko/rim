      SUBROUTINE REMREL(*)

         USE Globals, only : DFLAG, DMFLAG, DBNAME, USERID, OWNER, IFMOD
         USE Lexer, only: KWS, ITEMS, EQKEYW, LXSREC
         USE Message, only: WARN
         USE Rim, only : RMSTAT

         INCLUDE 'syspar.inc'
C
C     REMOVE A RELATION FROM THE DATABASE
C
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'reltbl.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'files.inc'
         INCLUDE 'rmatts.inc'
C
         LOGICAL NE
         LOGICAL EQ
         INCLUDE 'dclar1.inc'
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
         R = 2
         IF (EQKEYW(2,'TABLE')) R = 3

         IF(ITEMS.NE.R) THEN
            CALL WARN(4)
            GOTO 999
         ENDIF
         CALL LXSREC(R,RNAME,ZC)
C
C  FIND THE RELATION NAME IN THE RELATION TABLE.
C
         I = LOCREL(RNAME)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME)
            GOTO 999
         ENDIF
C
C
         L = LOCPRM(RNAME,2)
         IF(L.NE.0) GO TO 999
C
C     IF ONLINE ASK FOR VERIFICATION
C
         IF (CONNI) THEN
            CALL MSG(' ','ARE YOU SURE (YES/NO) ?',' ')
            CALL PRMSET('SET','?')
            CALL LODREC
            CALL PRMSET('RESET',' ')
            IF (ITEMS.NE.1 .OR. KWS(1).NE.'YES') THEN
               CALL MSG(' ','TABLE NOT REMOVED',' ')
               GOTO 999
            ENDIF
         ENDIF
C
C  CHANGE THE RELATION TABLE.
C
         CALL RELGET(ISTAT)
         IF(LRROW.NE.0) THEN
            RELTBL(1,LRROW) = -RELTBL(1,LRROW)
            RELMOD = 1
         ENDIF
C
C  CHANGE THE ATTRIBUTE TABLE.
C
         I = LOCATT(BLANK,RNAME)
         IF(I.NE.0) GO TO 800
  200    CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 800
         CALL ATTDEL(ISTAT)
         IF(ISTAT.NE.0) GO TO 800
         GO TO 200
C
C     DONE
C
  800    CALL MSG(' ','TABLE ''','+')
         CALL AMSG(RNAME,-ZC,'+')
         CALL MSG(' ',''' HAS BEEN REMOVED FROM THE DATABASE.',' ')
         IFMOD = .TRUE.
C
  999    RETURN 1
      END
