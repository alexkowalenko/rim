      SUBROUTINE SWSHEL(M,N)
C
C     SORT AN INTEGER ARRAY OF LENGTH N
C     USING SHELL SORT ALGORITHM
C
      DIMENSION M(N)
      INC = 1
  100 CONTINUE
      IF((9*INC+4).GE.N) GO TO 200
      INC = 3*INC + 1
      GO TO 100
  200 CONTINUE
      IF(INC.LT.1) GO TO 1000
      NMMINC = N-INC
C
C     START THE SORT LOOP
C
      DO 800 IS = 1,NMMINC
      K1 = IS
      K2 = IS + INC
      IF(M(K1).LE.M(K2)) GO TO 800
      MOVE = IS
      MT = M(K2)
  400 CONTINUE
      K1 = MOVE
      K2 = K1 + INC
      M(K2) = M(K1)
      MOVE = MOVE - INC
      IF(MOVE.LT.1) GO TO 600
      IF(MT.LT.M(MOVE)) GO TO 400
  600 CONTINUE
      M(K1) = MT
  800 CONTINUE
      INC = (INC-1)/3
      GO TO 200
 1000 CONTINUE
      RETURN
      END
