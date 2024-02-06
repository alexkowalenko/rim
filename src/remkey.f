      SUBROUTINE REMKEY(*)

         USE Globals, only : DFLAG, DMFLAG, DBNAME, USERID, OWNER
         Use Lexer, only: KXNAME, TOKTYP, ITEMS, EQKEYW, LXSREC
         USE Message, only: WARN
         USE Rim, only : RMSTAT
         USE Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C     REMOVE A KEY (MAKE ATTRIBUTE NON-KEYED)
C
C     :  REMOVE KEY FOR ATTRIBUTE IN RELATION

         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'attble.inc'
         LOGICAL NE
         LOGICAL EQ
         INCLUDE 'dclar1.inc'
         INCLUDE 'rmatts.inc'
C
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
C     CHECK THE COMMAND SYNTAX
C
         IF(ITEMS.NE.6 .OR. .NOT.EQKEYW(3,'FOR') .OR.
     X      .NOT.EQKEYW(5,'IN') .OR.
     X      .NOT.TOKTYP(3,KXNAME) .OR. .NOT.TOKTYP(6,KXNAME)) THEN
            CALL WARN(4)
            GOTO 999
         ENDIF
C
C  FIND THE RELATION NAME IN THE RELATION TABLE.
C
         CALL LXSREC(6,RNAME,ZC)
         I = LOCREL(RNAME)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME,BLANK)
            GOTO 999
         ENDIF
C
C
         L = LOCPRM(RNAME,2)
         IF(L.NE.0) GO TO 999
C
C  CHANGE THE ATTRIBUTE TABLE.
C
         CALL LXSREC(4,ANAME,ZC)
         I = LOCATT(ANAME,RNAME)
         IF(I.NE.0) THEN
            CALL WARN(3,ANAME,RNAME)
            GOTO 999
         ENDIF
         CALL ATTGET(ISTAT)
         ATTKEY = 0
         CALL ATTPUT(ISTAT)
C
  999    RETURN 1
      END
