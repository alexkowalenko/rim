      SUBROUTINE SELPUT(VAL,TYP,FMT,START,STRING)

         USE Globals, only : KMSSVL, KMSSVT, KNAPVL, KNAPVT, KRMRMF,
     +   KRMDMF
         USE Text, only : BLANK
         USE Utils, only : NDIGIT

         INCLUDE 'syspar.inc'
C
C     THIS ROUTINE PUTS AN ACTUAL VALUE (NON-TEXT) INTO STRING.
C     VAL ---- VALUE
C     TYP ---- FROM TYPER
C     FMT ---- FORMAT TO USE
C     START -- STARTING PLACE IN STRING
C     STRING - PLACE TO PUT TEXT
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'rmatts.inc'
C
         INTEGER IV(2)
C
C
         IF (TYP.EQ.KZINT) THEN
C
C        INTEGER
C
            IF(FMT.EQ.0) FMT = NDIGIT(VAL)
            IF (VAL.EQ.ZIMISS .OR. VAL.EQ.ZINAPP) GOTO 8000
            CALL ITOA(STRING,START,FMT,VAL,IERR)
         ELSE
C
C        REAL
C
            IF (VAL.EQ.ZIMISS .OR. VAL.EQ.ZINAPP) GOTO 8000
            IF (TYP.EQ.KZDOUB) THEN
               IF (FMT.EQ.0) FMT = KRMDMF
               CALL RTOA(STRING,START,FMT,VAL,IERR)
            ELSE
               IF (FMT.EQ.0) FMT = KRMRMF
               CALL RTOD(IV,VAL)
               CALL RTOA(STRING,START,FMT,IV,IERR)
            ENDIF
         ENDIF
         GOTO 9999
C
C     MISSING VALUES
C
 8000    WIDTH = MOD(FMT,100)
         CALL STRMOV(BLANK,1,WIDTH,STRING,START)
         IF(VAL.EQ.ZIMISS)THEN
C        MISSING VALUE
            OFFSET = MAX(0,WIDTH - KMSSVL)
            CALL STRMOV(KMSSVT,1,KMSSVL,STRING,START+OFFSET)
CCC      IF(WIDTH.EQ.0)START= START + KMSSVL
         ENDIF
         IF(VAL.EQ.ZINAPP)THEN
C        NOT-APP VALUE
            OFFSET = MAX(0,WIDTH - KNAPVL)
            CALL STRMOV(KNAPVT,1,KNAPVL,STRING,START+OFFSET)
CCC      IF(WIDTH.EQ.0)START= START + KNAPVL
         ENDIF
C
 9999    CONTINUE
         RETURN
      END
