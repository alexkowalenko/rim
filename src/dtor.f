      INTEGER FUNCTION DTOR(D)
      INCLUDE 'syspar.d'
C
C     CONVERT A DOUBLE (D) TO REAL (DTOR) 
C     BOTH ARE INTEGER VARS
C
      INTEGER D(2)

      REAL RNUM
      INTEGER IR
      EQUIVALENCE (RNUM,IR)
      DOUBLE PRECISION DNUM
      INTEGER ID(2)
      EQUIVALENCE (DNUM,ID(1))
C
      ID(1) = D(1)
      ID(2) = D(2)
      RNUM = DNUM
      DTOR = IR
      RETURN
      END
