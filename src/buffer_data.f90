MODULE RM_BufferData
   !!
   !!  *** / R I M B U F / ***
   !!
   !!  INCORE BUFFER FOR TUPLES AND IO PAGES
   !!

   Use RM_Parameters, only: ZBUF, Z
   implicit none
   private

   !COMMON /RIMBUF/ BUFFER(ZBUF)
   INTEGER, public :: BUFFER(ZBUF)
   !!     BUFFER--ARRAY TO HOLD ALL BLOCKS

   INTEGER, public ::  IREL(Z,1)
   EQUIVALENCE (BUFFER(1),IREL(1,1))

END MODULE RM_BufferData
