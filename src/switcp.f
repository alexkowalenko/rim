      INTEGER FUNCTION SWITCP(I1,I2,LEN)
      INCLUDE 'syspar.inc'
C
C     COMPARE TWO ARRAYS OF ASCII-TEXT (WORD LEN = LEN)
C
C     IF I1 = I2 THEN SET SWITCP ZERO
C     IF I1 > I2 THEN SET SWITCP NEG
C     IF I1 < I2 THEN SET SWITCP POS
C
      INCLUDE 'flags.inc'
C
      IF (LEN.EQ.0) GOTO 15
      NC = LEN*ZCW
      DO 10 I = 1,NC
      CALL GETT(I1,I,A1)
      CALL GETT(I2,I,A2)
      IF (CASEIG) THEN
         A1 = UPCASE(A1)
         A2 = UPCASE(A2)
      ENDIF
      IF (A1.GT.A2) GOTO 20
      IF (A1.LT.A2) GOTO 30
   10 CONTINUE
   15 SWITCP = 0
      RETURN
   20 SWITCP = -1
      RETURN
   30 SWITCP = 1
      RETURN
      END
