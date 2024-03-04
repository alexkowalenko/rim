MODULE RM_BTree_Data

   USE RM_Parameters

   implicit none
   private

   !
   !  VARIABLE DEFINITIONS:
   !  BTREE NODES ARE ARRANGED IN THREE WORD GROUPS
   !     VALUE---ROW 1 - ARRAY OF BTREE KEY VALUES
   !     VALUE---ROW 2 - ARRAY OF BTREE NODE POINTERS AND TUPLE
   !                     POINTERS
   !     VALUE---ROW 3 - ARRAY OF MOT TUPLE POINTERS
   !
   !  MOT TABLES ARE ARRANGED IN TWO WORD PAIRS
   !     ROW 1 - MOT POINTER TO NEXT LINK
   !     ROW 2 - TUPLE POINTER
   !

   !COMMON /BTBUF/ CORE(ZBT)
   INTEGER, public :: CORE(ZBT)
   INTEGER, public  :: VALUE(3,1)
   EQUIVALENCE (CORE(1),VALUE(1,1))
   REAL, public ::  RVALUE(3,1)
   EQUIVALENCE (CORE(1),RVALUE(1,1))

   !
   !  *** / S T A R T / ***
   !
   !  BTREE STARTING NODE RECORD NUMBER
   !
   !COMMON /START/ START
   INTEGER, public :: START
   !
   !  VARIABLE DEFINITIONS:
   !         START---RECORD NUMBER FOR THE STARTING NODE IN A BTREE
   !


   !
   !  *** / S T A C K / ***
   !
   !  VARIABLES FOR MOVING DOWN A BTREE INDEX
   !
   !COMMON /STACK/ STACK(20),SP
   INTEGER, public :: STACK(20)
   INTEGER, public :: SP
   !
   !  VARIABLE DEFINITIONS
   !     STACK---ARRAY OF STACK POINTERS
   !     SP------INDEX TO THE STACK ARRAY
   !

END MODULE RM_BTree_Data