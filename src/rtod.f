      SUBROUTINE RTOD(D,R)
         USE, intrinsic :: iso_fortran_env

         INCLUDE 'syspar.inc'
C
C     CONVERT A REAL (R) TO DOUBLE (D)
C     BOTH ARE INTEGER VARS AND MAY OCCUPY THE SAME SPACE
C
         INTEGER D(2)

         REAL RNUM
         INTEGER IR
         EQUIVALENCE (RNUM,IR)
         REAL(real64) :: DNUM
         INTEGER ID(2)
         EQUIVALENCE (DNUM,ID(1))
C
         IR = R
         DNUM = RNUM
         D(1) = ID(1)
         D(2) = ID(2)
         RETURN
      END
