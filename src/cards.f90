MODULE Cards
   !!
   !!  CONTAINS INPUT RECORD AS UNPACKED ASCII-TEXT
   !!

   USE Parameters, only : ZCARDW, ZCARDN

   implicit none
   private
   !
   !  *** / C A R D S / ***
   !
   !  CONTAINS INPUT RECORD AS UNPACKED ASCII-TEXT
   !

   public Initialise

   INTEGER, public :: CRDREC(ZCARDW)
   !! CRDREC--ARRAY OF CHARS AS ASCII-TEXT
   INTEGER, public :: CRDPTR
   !!         CRDPTR--POINTER TO CURRENT CHARACTER IN CRDREC
   INTEGER, public :: CRDEND
   !!         CRDEND--POINTER TO LAST CHARACTER IN CRDREC
   INTEGER, public :: READCD
   !!         READCD--CARD STATUS FLAG
   !!                -1  -  USE EXISTING CRDREC (READING DISABLED)
   !!                 0  -  READ NEW CARD
   !!                 1  -  CONTINUE CURRENT CARD (READING ENABLED)
   !!                 2  -  END-OF-CARD REACHED
   INTEGER, public :: LXEOC
   !!         LXEOC---GT 0 IF THE LOGICAL END-OF-CARD HAS BEEN REACHED
   INTEGER, public :: CRDRLB(ZCARDW,ZCARDN)
   !!         CRDRLB--CARDREC RECALL BUFFER
   INTEGER, public :: CRDRLL(ZCARDN)
   !!         CRDRLL--LENGTHS OF CARD RECORDS IN CRDRLB
   INTEGER, public :: CRDIDX
   !!         CRDIDX--CURRENT INDEX INTO RECALL BUFFER

contains

   SUBROUTINE Initialise
      !! Intialise values
      INTEGER :: I

      READCD = 0
      CRDIDX = 0
      CRDRLL = [(0, I = 1, ZCARDN)]
      LXEOC = 0
   END SUBROUTINE Initialise
END MODULE Cards
