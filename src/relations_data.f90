MODULE RM_Relations_Data
   !!
   !!  *** / R E L T B L / ***
   !!
   !!  BUFFER TO HOLD ONE PAGE FROM THE RELTBL RELATION
   !!

   USE RM_Parameters

   implicit none
   private

   INTEGER, public :: RELBUF(ZF1)
   !!         RELBUF--BUFFER FOR ONE PAGE FROM THE RELTBL RELATION
   INTEGER, public ::CNAME(Z)
   !!         CNAME---CURRENT RELATION NAME SPECIFIED IN LOCREL
   INTEGER, public :: LRROW
   !!         LRROW---LAST ROW SENT IN TUPLER
   INTEGER, public :: NRROW
   !!         NRROW---NEXT AVAILABLE ROW FOR ADDING A TUPLE
   INTEGER, public :: RELMOD
   !!         RELMOD--MODIFICATION FLAG - O MEANS NO, 1 MEANS YES
   INTEGER, public ::  RELTBL(ZRELL,ZRELR)
   !!         RELTBL--EQUIVALENCE ARRAY FOR EASIER USE OF RELBUF
   EQUIVALENCE (RELBUF(2),RELTBL(1,1))

END MODULE RM_Relations_Data

