      INTEGER FUNCTION SWIRCP(I1,I2,LEN)
      REAL I1(LEN), I2(LEN)
      DO 10 I = 1,LEN
      IF (I1(I).GT.I2(I)) GO TO 20
      IF (I1(I).LT.I2(I)) GO TO 30
   10 CONTINUE
      SWIRCP = 0
      RETURN
   20 SWIRCP = -1
      RETURN
   30 SWIRCP = 1
      RETURN
      END
