      SUBROUTINE TOLED(K,V,N)

         USE, intrinsic :: iso_fortran_env

         USE Globals, only : PCENT, TOL

         INCLUDE 'syspar.inc'
C
C     THIS ROUTINE APPLIES A TOLERANCE TO A DOUBLE ROUTINE
C
C     K IS LOCBOO VALUE
C     V(N) IS DOUBLE ARRAY
C
         REAL(real64) :: V(N)
         REAL(real64) :: X

         X = TOL
         IF(K.GT.5) X = -X
         IF(PCENT) GO TO 50
         DO 20 I=1,N
            V(I) = V(I) - X
   20    CONTINUE
         RETURN
   50    CONTINUE
         DO 70 I=1,N
            V(I) = V(I)*(1.-X)
   70    CONTINUE
         RETURN
      END
