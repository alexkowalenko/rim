MODULE Macros
   implicit none
   private

   public LOCMAC

contains

   INTEGER FUNCTION LOCMAC(MAC)
      USE Parameters
      !
      ! LOCATES A MACRO BY NAME (MAC)
      !
      ! LOCMAC = POSITION OF MACRO OR 0 IF NOT FOUND
      !
      !-----------------------------------------------------
      !
      INTEGER, intent(in) :: MAC(Z)

      INTEGER :: I

      INCLUDE 'maccom.inc'

      LOGICAL :: EQ

      DO I = 1, MACNUM
         IF (EQ(MAC,MACNAM(1,I))) THEN
            LOCMAC = I
            RETURN
         ENDIF
      END DO
      LOCMAC = 0
      RETURN
   END FUNCTION LOCMAC

END MODULE Macros
