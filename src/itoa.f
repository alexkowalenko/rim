      SUBROUTINE ITOA(STRING,SC,FMT,INT,IERR)

         USE Utils, only : NDIGIT
         USE Text, only : FILCH

         INCLUDE 'syspar.inc'
C
C     THIS ROUTINE CONVERTS AN INTEGER (INT) TO ASCII-TEXT (STRING)
C     IF THE INTEGER WILL NOT FIT, STRING IS
C     STARRED AND IERR IS RETURNED NON-ZERO.
C
C     STRING....REPOSITORY FOR TEXT OF INT
C     SC .......STARTING CHARACTER POS
C     FMT ......FORMAT CODE (TOTAL WIDTH + 100*DECIMAL + 10000*REPEAT)
C     INT.......INTEGER TO CONVERT.
C     IERR......0 IF INT FITS, 1 OTHERWISE
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'lxlcom.inc'

         INTEGER, intent(out) :: STRING(*)
C
         IERR = 0
         S = SC - 1
C
         F = MOD(FMT,10000)
         N = ABS(INT)
         NC = MOD(F,100)
         CALL FILCH(STRING,SC,NC,ABLANK)
         D = F / 100
         DP = NC - D
         MINL = NDIGIT(N)
         IF (D.NE.0) MINL = MINL + 1
         IF (INT.LT.0) MINL = MINL + 1
C
         L = NC
         IF (L.LT.MINL) GOTO 800
  100    CALL PUTT(STRING,S+L,MOD(N,10)+U0)
         L = L-1
         N = N/10
         IF (L.EQ.DP) THEN
            CALL PUTT(STRING,S+L,DECIM)
            L = L-1
         ENDIF
         IF (N.GT.0 .OR. DP.LT.L) GOTO 100
C
         IF (INT.LT.0) CALL PUTT(STRING,S+L,MNSIGN)
         RETURN

C     NUMBER TOO BIG

  800    CALL FILCH(STRING,SC,NC,ASSTAR)
         IERR = 1
         RETURN
      END
