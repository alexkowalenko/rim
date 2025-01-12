MODULE Extern

   USE RM_Parameters, only: Z, MAXCOL, ZPRINW

   implicit none
   private

   public SETIN
   public SETOUT
   public PROMPT
   public PRMSET
   public NXTCRD
   public LOADIT
   public LOADFM
   public IMSG
   public DMSG
   public AMSG
   public MSG
   public MSGCMV

   INTEGER, PARAMETER :: ZTUPAL = 2*Z+7

   !!
   !!  *** / M S G C O M / ***
   !!
   !!  CONTAINS OUTPUT RECORD AS UNPACKED ASCII-TEXT
   !!
   !! COMMON /MSGCOM/ MSGREC(ZPRINW), MSGPTR, MSUNIT
   INTEGER, public :: MSGREC(ZPRINW)
   !!         MSGREC--ARRAY OF CHARS AS ASCII-TEXT
   INTEGER, public :: MSGPTR
   !!         MSGPTR--POINTER TO CURRENT CHARACTER IN MSGREC
   INTEGER, public :: MSUNIT
   !!         MSUNIT--UNIT NUMBER FOR CURRENT MESSAGE

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

      ! Messaging routines

      MODULE SUBROUTINE IMSG(NUM,NUMC,MCONT)
         INTEGER, intent(in) :: NUM, NUMC
         CHARACTER(len=1), intent(in) :: MCONT
      END SUBROUTINE IMSG

      MODULE SUBROUTINE DMSG(JDAT,DFMT,MCONT,TYP)
         INTEGER, intent(in) :: JDAT, DFMT, TYP
         CHARACTER(len=1) :: MCONT
      END SUBROUTINE DMSG

      MODULE SUBROUTINE AMSG(MTEXT,NUMC,MCONT)
         INTEGER, intent(in) :: MTEXT(*)
         INTEGER, intent(in) :: NUMC
         CHARACTER(len=1), intent(in) :: MCONT
      END SUBROUTINE AMSG

      MODULE SUBROUTINE MSG(MTYPE,MTEXT,MCONT)
         CHARACTER(len=*), intent(in) :: MTYPE
         CHARACTER(len=*), intent(in) :: MTEXT
         CHARACTER(len=1), intent(in) :: MCONT
      END SUBROUTINE MSG

      MODULE SUBROUTINE MSGCMV(MTEXT,CTYPE)
         CHARACTER(len=*) :: MTEXT
         CHARACTER(len=*) :: CTYPE
      END SUBROUTINE MSGCMV

   END INTERFACE

   public Initialise

CONTAINS

   SUBROUTINE Initialise
      USE RM_Globals, only : NOUT
      MSUNIT = NOUT
      MSGPTR = 0
   END SUBROUTINE Initialise

END MODULE Extern
