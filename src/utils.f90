MODULE Utils
   implicit none
   private

   public ZEROIT
   public NULLIT
   public ZMOVE
   public NDIGIT
   public HTOI, ITOH

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
      USE RM_Parameters, only : Z, NULL
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
      USE RM_Parameters, only : Z
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


   INTEGER FUNCTION NDIGIT(INT)
      !
      ! RETURN THE NUMBER OF DIGITS IN INT
      !
      INTEGER, intent(in) :: INT

      INTEGER :: ABS

      NDIGIT = 0
      ABS = INT
      IF (INT.LT.0) THEN
         ABS = 0 - INT
         NDIGIT = 1
      ENDIF
100   NDIGIT = NDIGIT + 1
      ABS = ABS/10
      IF (ABS.GT.0) GOTO 100
      RETURN
   END FUNCTION NDIGIT


   SUBROUTINE HTOI(I,J,K)
      USE RM_Parameters, only : ZHTOI
      !
      !  PURPOSE:   PACK I AND J INTO K
      !
      !  OFFSET I BY MULTIPLYING BY ZHTOI
      !
      INTEGER, intent(in) :: I, J
      INTEGER, intent(out) :: K

      K = J + (ZHTOI * I)
      RETURN
   END SUBROUTINE HTOI


   SUBROUTINE ITOH(I,J,K)
      USE RM_Parameters, only : ZHTOI
      !
      !  PURPOSE:   UNPACK I AND J FROM K
      !
      !  I WAS MULTIPLIED BY ZHTOI
      !
      INTEGER, intent(out) :: I, J
      INTEGER, intent(in) :: K

      I = K / ZHTOI
      J = K - (ZHTOI * I)
      RETURN
   END SUBROUTINE ITOH

END MODULE Utils
