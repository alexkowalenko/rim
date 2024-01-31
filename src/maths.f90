MODULE Maths

   USE, intrinsic :: iso_fortran_env

   implicit none
   private

   public IEXP
   public DTOR, RTOD
   public ROUND

contains

   INTEGER FUNCTION IEXP(REAL)
      !
      ! THIS FUNCTION RETURNS THE BASE TEN EXPONENT OF A DOUBLE
      !
      REAL(real64), intent(in) :: REAL

      REAL(real64) :: X
      INTEGER :: IE

      IE = 1
      X = 1.0 * DABS(REAL)
      IF (X == 0) GOTO 999
      X = DLOG10(X) + .000000000000001D0
      IE = INT(X) + 1
      IF(X < 0.) IE = 1 + (INT(1000. + X) - 1000)

999   CONTINUE
      IEXP = IE
      RETURN
   END FUNCTION IEXP


   INTEGER FUNCTION DTOR(D)
      !
      ! CONVERT A DOUBLE (D) TO REAL (DTOR)
      ! BOTH ARE INTEGER VARS
      !
      INTEGER, intent(in) ::  D(2)

      REAL :: RNUM
      REAL(real64) :: DNUM
      INTEGER :: IR

      dnum = transfer(d, dnum)
      rnum = real(dnum, real32)
      IR = transfer(rnum, IR)

      DTOR = IR
      RETURN
   END FUNCTION DTOR


   SUBROUTINE RTOD(D,R)
      !
      ! CONVERT A REAL (R) TO DOUBLE (D)
      ! BOTH ARE INTEGER VARS AND MAY OCCUPY THE SAME SPACE
      !
      INTEGER, intent(out) :: D(2)
      INTEGER, intent(in) :: R

      REAL :: RNUM
      REAL(real64) :: DNUM
      INTEGER :: ID(2)

      RNUM = transfer(R, RNUM)
      DNUM = real(RNUM, real64)
      ID = transfer(DNUM, ID)

      D(1) = ID(1)
      D(2) = ID(2)
      RETURN
   END SUBROUTINE RTOD


   SUBROUTINE ROUND(REAL,ND,RO)
      USE, intrinsic :: iso_fortran_env
      !
      ! RETURN A ROUNDED VERSION OF THE REAL NUMBER
      ! ACCURATE TO ND PLACES.
      !

      REAL(real64), intent(in) :: REAL
      INTEGER, intent(in) :: ND
      REAL(real64), intent(out) :: RO

      REAL(real64) :: V

      RO = REAL
      IF(REAL.EQ.0.) RETURN
      V = .5
      IF(REAL.LT.0.) V = -.5
      RO = REAL + V*(10.0D0**(0-ND))
      RETURN
   END SUBROUTINE ROUND

END MODULE Maths

