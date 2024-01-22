      SUBROUTINE TOLED(K,V,N)
      INCLUDE 'syspar.inc'
C
C     THIS ROUTINE APPLIES A TOLERANCE TO A DOUBLE ROUTINE
C
C     K IS LOCBOO VALUE
C     V(N) IS DOUBLE ARRAY
C
      INCLUDE 'flags.inc'
      DOUBLE PRECISION V(N)
      DOUBLE PRECISION X
      X = TOL
      IF(K.GT.5) X = -X
      IF(PCENT) GO TO 50
      DO 20 I=1,N
      V(I) = V(I) - X
   20 CONTINUE
      RETURN
   50 CONTINUE
      DO 70 I=1,N
      V(I) = V(I)*(1.-X)
   70 CONTINUE
      RETURN
      END
