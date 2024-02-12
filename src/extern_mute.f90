SUBMODULE (Extern) Extern_out
   implicit none

contains

   MODULE SUBROUTINE SETIN(FILE)
      CHARACTER(len=*), intent(in) :: FILE
   END SUBROUTINE SETIN


   SUBROUTINE SETOUT(UN,UNF,FILE,STAT)
      INTEGER, intent(inout) :: UN
      INTEGER, intent(in) :: UNF
      CHARACTER(len=*), intent(in) :: FILE
      INTEGER, intent(out) :: STAT
   END SUBROUTINE SETOUT

END SUBMODULE Extern_out
