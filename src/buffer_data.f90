MODULE RM_BufferData
   Use RM_Parameters, only: ZBUF, Z
   implicit none
   private

   !COMMON /RIMBUF/ BUFFER(ZBUF)
   INTEGER, public :: BUFFER(ZBUF)

   INTEGER, public ::  IREL(Z,1)
   EQUIVALENCE (BUFFER(1),IREL(1,1))


END MODULE RM_BufferData
