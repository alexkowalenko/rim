MODULE Macros
   !! Routines for dealing with macros

   USE Parameters, only : Z, ZCW, ZMXMAC, ZMXMTX, ZMASC

   implicit none
   private

   !  *** / M A C C O M / ***
   !
   !  MACRO DEFINITION COMMON BLOCK
   !
   INTEGER, public :: MACNUM
   INTEGER, public :: MACNAM(Z,ZMXMAC)
   INTEGER, public :: MACNRG(ZMXMAC)
   INTEGER, public :: MACPTR(ZMXMAC)
   INTEGER, public :: MACLEN(ZMXMAC)
   INTEGER, public :: MACTXT(ZMXMTX)
   INTEGER, public :: MACNTX
   INTEGER, public :: MACWRK(ZMASC)
   INTEGER, public :: MACWPT

   INTEGER, PARAMETER, public :: MACWPZ=ZMASC*ZCW

   !
   !  VARIABLE DEFINITIONS:
   !
   !  MACNUM ----NUMBER OF DEFINED MACROS (MAX = ZMXMAC)
   !  MACNAM ----MACRO NAMES
   !  MACNRG ----NUMBER OF ARGUMENTS
   !  MACPTR ----POINTER TO MACRO REPLACEMENT TEXT IN MACTXT
   !  MACLEN ----LENGTH OF MACRO TEXT
   !  MACTXT ----MACRO REPLACEMENT TEXT BUFFER
   !  MACNTX ----NEXT AVAILABLE WORD IN MACTXT
   !  MACWRK ----MACRO WORK AREA FOR EXPANSION
   !  MACWPT ----POINTER TO CURRENT CHAR IN WORK AREA
   !  MACWPZ ----POINTER TO END OF WORK AREA

   public Initialise
   public LOCMAC

contains

   SUBROUTINE Initialise
      !! Initialise macros
      MACNUM = 0
      MACNTX = 1
      MACWPT = MACWPZ
   END SUBROUTINE Initialise


   INTEGER FUNCTION LOCMAC(MAC)
      !!
      !! LOCATES A MACRO BY NAME (MAC)
      !!
      !! LOCMAC = POSITION OF MACRO OR 0 IF NOT FOUND
      !!
      USE Parameters

      INTEGER, intent(in) :: MAC(Z)

      INTEGER :: I

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
