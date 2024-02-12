MODULE Extern
   implicit none
   private

   public SETIN
   public SETOUT

   INTERFACE
      MODULE SUBROUTINE SETIN(FILE)
         CHARACTER(len=*), intent(in) :: FILE
      END SUBROUTINE SETIN

      MODULE SUBROUTINE SETOUT(UN, UNF, FILE, STAT)
         INTEGER, intent(inout) :: UN
         INTEGER, intent(in) :: UNF
         CHARACTER(len=*), intent(in) :: FILE
         INTEGER, intent(out) :: STAT
      END SUBROUTINE SETOUT
   END INTERFACE

END MODULE Extern
