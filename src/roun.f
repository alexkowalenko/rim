      SUBROUTINE ROUN(REAL,ND,RO)
         USE, intrinsic :: iso_fortran_env
C
C     RETURN A ROUNDED VERSION OF THE REAL NUMBER
C     ACCURATE TO ND PLACES.
C
         INCLUDE 'syspar.inc'
         REAL(real64) :: REAL, V, RO
    1    RO = REAL
         IF(REAL.EQ.0.) RETURN
         V = .5
         IF(REAL.LT.0.) V = -.5
         RO = REAL + V*(10.0D0**(0-ND))
    9    RETURN
      END
