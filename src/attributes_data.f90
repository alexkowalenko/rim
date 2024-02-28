MODULE RM_Attributes_Data
!!  *** / A T T B L E / ***
!!
!!  BUFFER TO HOLD ONE PAGE FROM THE ATTRIBUTE RELATION
!!

   USE RM_Parameters, only: ZF1, Z, ZATTL, ZATTR

   INTEGER, public :: ATTBUF(ZF1)
   !!         ATTBUF--BUFFER FOR ONE PAGE FROM THE ATTRIBUTE RELATION
   INTEGER, public :: NAROW
   !!         NAROW---NEXT AVAILABLE ROW FOR ADDING A TUPLE
   INTEGER, public :: ATTMOD
   !!         ATTMOD--MODIFICATION FLAG - O MEANS NO, 1 MEANS YES
   INTEGER, public :: ATTBLE(ZATTL,ZATTR)
   !!         ATTBLE--EQUIVALENCE ARRAY FOR EASIER USE OF ATTBUF
   EQUIVALENCE (ATTBUF(2),ATTBLE(1,1))

END MODULE