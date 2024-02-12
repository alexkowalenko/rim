MODULE Extern
   implicit none
   private

   public SETIN
   public SETOUT
   public PROMPT
   public PRMSET

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

      MODULE SUBROUTINE PROMPT(PTXT)
         INTEGER, intent(in) :: PTXT(*)
      END SUBROUTINE PROMPT

      MODULE SUBROUTINE PRMSET(MODE,PR)
         CHARACTER(len=*), intent(in) :: MODE,PR
      END SUBROUTINE PRMSET
   END INTERFACE

END MODULE Extern
