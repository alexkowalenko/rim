      SUBROUTINE RTOD(D,R)
      INCLUDE 'syspar.d'
C
C     CONVERT A REAL (R) TO DOUBLE (D)
C     BOTH ARE INTEGER VARS AND MAY OCCUPY THE SAME SPACE
C
      INTEGER D(2)

      REAL RNUM
      INTEGER IR
      EQUIVALENCE (RNUM,IR)
      DOUBLE PRECISION DNUM
      INTEGER ID(2)
      EQUIVALENCE (DNUM,ID(1))
C
      IR = R
      DNUM = RNUM
      D(1) = ID(1)
      D(2) = ID(2)
      RETURN
      END
