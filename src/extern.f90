MODULE Extern

   USE Parameters, only: Z, MAXCOL

   implicit none
   private

   public SETIN
   public SETOUT
   public PROMPT
   public PRMSET
   public NXTCRD
   public LOADIT
   public LOADFM

   INTEGER, PARAMETER :: ZTUPAL = 2*Z+7

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

      MODULE SUBROUTINE NXTCRD(EOF)
         INTEGER, intent(out) :: EOF
      END SUBROUTINE NXTCRD

      MODULE SUBROUTINE LOADIT(MAT,ATT)
         INTEGER, intent(out) :: MAT(1)
         INTEGER, intent(in) :: ATT(ZTUPAL,1)
      END SUBROUTINE LOADIT

      MODULE SUBROUTINE LOADFM(MAT,ATT,FOR,NFOR)
         INTEGER, intent(out) :: MAT(MAXCOL)
         INTEGER, intent(in) :: ATT(ZTUPAL,1)
         INTEGER, intent(in out) :: FOR(6,1)
         INTEGER, intent(in) :: NFOR
      END SUBROUTINE LOADFM
   END INTERFACE

END MODULE Extern
