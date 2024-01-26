MODULE Utils
   implicit none
   private

   public ZEROIT
   public NULLIT
   public ZMOVE

contains

   SUBROUTINE ZEROIT(ARRAY,NWDS)
      !
      !  PURPOSE:   ZERO OUT AN ARRAY
      !
      !  PARAMETERS:
      !     ARRAY---ARRAY TO BE ZEROED OUT
      !     NWDS----NUMBER OF WORDS IN THE ARRAY
      !
      INTEGER, intent(out) :: ARRAY(1)
      INTEGER, intent(in) :: NWDS

      INTEGER :: I

      DO I=1,NWDS
         ARRAY(I) = 0
      END DO
      RETURN
   END SUBROUTINE ZEROIT


   SUBROUTINE NULLIT(WORD1)
      INCLUDE 'syspar.inc'
      !
      !  PURPOSE:   COPIES NULL TO WORD1
      !
      !  PARAMETERS:
      !     WORD1---DESTINATION LONG WORD
      !
      INTEGER, intent(out) :: WORD1(Z)

      INTEGER :: I
      !
      DO I = 1, Z
         WORD1(I) = NULL
      END DO
      RETURN
   END SUBROUTINE NULLIT


   SUBROUTINE ZMOVE(WORD1,WORD2)
      INCLUDE 'syspar.inc'
      !
      !  PURPOSE:   COPIES WORD2 TO WORD1
      !
      !  PARAMETERS:
      !     WORD1---DESTINATION LONG WORD
      !     WORD2---SOURCE LONG WORD
      !
      INTEGER, intent(out) :: WORD1(Z)
      INTEGER, intent(in) :: WORD2(Z)

      INTEGER :: I
      !
      DO I = 1, Z
         WORD1(I) = WORD2(I)
      END DO
      RETURN
   END SUBROUTINE ZMOVE

END MODULE Utils
