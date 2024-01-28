MODULE Text
   implicit none
   private

   public UPCASE
   public ASCCHR
   public ASCTXT

CONTAINS

   INTEGER FUNCTION UPCASE(ASCHR)
      !
      ! RETURN THE UPPER CASE EQUIVALENT OF ASCHR (ASCII-CHAR)
      !
      INTEGER, intent(in) :: ASCHR

      INTEGER, PARAMETER :: LA=97,LZ=122,OFF=97-65
      !
      !
      IF (ASCHR.LT.LA .OR. ASCHR.GT.LZ) THEN
         UPCASE = ASCHR
      ELSE
         UPCASE = ASCHR - OFF
      ENDIF
      RETURN
   END FUNCTION UPCASE


   INTEGER FUNCTION ASCCHR(CH)
      !
      ! ** UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
      !
      ! RETURN THE ASCII-CHAR EQUIVALENT OF CH
      !
      CHARACTER(len=1), intent(in) :: CH
      !
      ASCCHR = ICHAR(CH)
      ! Convert tab (9) to space (32)
      IF (ASCCHR.EQ.9) ASCCHR = 32
      RETURN
   END FUNCTION ASCCHR


   SUBROUTINE ASCTXT(ATXT,LTXT,CSTR)
      USE Parameters, only : Z
      !
      ! RETURN THE ASCII-TEXT EQUIVALENT OF CSTR
      !
      !    ATXT -- PLACE TO PUT ASCII-TEXT
      !    LXTX -- LENGTH OF ATXT
      !    CSTR -- CHARACTER STRING
      !
      INTEGER, intent(out) :: ATXT(*)
      INTEGER, intent(in) :: LTXT
      CHARACTER(len=*), intent(in) :: CSTR
      !
      INCLUDE 'ascpar.inc'
      INTEGER :: L, I
      !
      L = LEN(CSTR)
      DO I = 1, LTXT
         IF (I.LE.L) THEN
            CALL PUTT(ATXT,I,ASCCHR(CSTR(I:I)))
         ELSE
            CALL PUTT(ATXT,I,ABLANK)
         ENDIF
      END DO
      RETURN
   END SUBROUTINE ASCTXT

END MODULE Text
