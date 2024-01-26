      SUBROUTINE LODPAS(ERROR)

         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     PROCESS PASSWORDS DEFINITIONS
C             PASSWORD KEYWORDS MAY BE ABBREVIATED
C
C     SYNTAX: READ PASSWORD FOR <REL> IS <PASSWORD>
C             RPW FOR <REL> IS <PASSWORD>
C             WRITE PASSWORD FOR <REL> IS <PASSWORD>
C             WPW FOR <REL> IS <PASSWORD>
C
C             <REL> CAN BE * FOR ALL RELATIONS
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'files.inc'
         INTEGER ERROR
         LOGICAL EQKEYW
         INCLUDE 'dclar1.inc'
         INCLUDE 'dclar3.inc'
C
C  READ A PASSWORD.
C
  100    CONTINUE
         CALL LODREC
         IF (ITEMS.EQ.1) GOTO 999
         PWTP = 0
         IF (EQKEYW(1,'READ')) THEN
            IF (.NOT.EQKEYW(2,'PASSWORD')) GOTO 700
            PWTP = 1
            STOK = 3
            GOTO 200
         ENDIF
         IF (EQKEYW(1,'MODIFY')) THEN
            IF (.NOT.EQKEYW(2,'PASSWORD')) GOTO 700
            PWTP = 2
            STOK = 3
            GOTO 200
         ENDIF
         IF (EQKEYW(1,'RPW')) THEN
            PWTP = 1
            STOK = 2
            GOTO 200
         ENDIF
         IF (EQKEYW(1,'MPW')) THEN
            PWTP = 2
            STOK = 2
            GOTO 200
         ENDIF
         IF(PWTP.EQ.0)  GO TO 700
C
  200    IF(.NOT.EQKEYW(STOK,'FOR')) GOTO 700
         IF(.NOT.EQKEYW(STOK+1,'*')) CALL LXSREC(STOK+1,RNAME,ZC)
         I = LOCREL(RNAME)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME,BLANK)
            GO TO 100
         ENDIF
C
         IF(.NOT.EQKEYW(STOK+2,'IS')) GO TO 700
C
C  STORE THE PASSWORD.
C
  500    CALL RELGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 100
         IF(.NOT.TOKTYP(ITEMS,KXNAME)) THEN
            CALL WARN(7,ASCREC(IDP(ITEMS)),0)
            ERROR = ERROR + 1
            GO TO 100
         ENDIF
         CALL LXSREC(STOK+3,RPW1,ZC)
         IF(PWTP.EQ.1) CALL ZMOVE(RPW,RPW1)
         IF(PWTP.EQ.2) CALL ZMOVE(MPW,RPW1)
         CALL RELPUT
C
C  LOOK FOR MORE RELATIONS.
C
         GO TO 500
C
C     SYNTAX ERROR
C
  700    CALL WARN(4,0,0)
         GOTO 100
C
C  END PASSWORD PROCESSING.
C
  999    CONTINUE
         RETURN
      END
