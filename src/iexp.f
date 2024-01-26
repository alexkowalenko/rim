      INTEGER FUNCTION IEXP(REAL)
         USE, intrinsic :: iso_fortran_env
C
C     THIS FUNCTION RETURNS THE BASE TEN EXPONENT OF A DOUBLE
C
         INCLUDE 'syspar.inc'
         REAL(real64) :: REAL,X
         IE = 1
         X = 1.0*DABS(REAL)
         IF(X.EQ.0) GO TO 999
    1    X = DLOG10(X) + .000000000000001D0
    2    IE = INT(X) + 1
         IF(X.LT.0.) IE = 1 + (INT(1000.+X)-1000)
  999    CONTINUE
         IEXP = IE
         RETURN
      END
