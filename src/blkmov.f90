SUBROUTINE BLKMOV(TO,FROM,NWORDS)
   !!
   !!  PURPOSE:   MOVE WORDS BETWEEN ARRAYS
   !!  Sometimes called with scalar arguments, so not type safe. Type safe version in Utils module
   !!
   implicit none

   INTEGER :: TO(*),FROM(*)
   INTEGER, intent(in) :: NWORDS

   INTEGER :: I, NW

   IF(NWORDS.LT.0) GO TO 200
   !
   !  MOVE FROM THE FRONT OF THE ARRAYS.
   !
   DO I=1,NWORDS
      TO(I) = FROM(I)
   END DO
   RETURN
   !
   !  MOVE FROM THE REAR OF THE ARRAYS.
   !
200 CONTINUE
   NW = -NWORDS
   DO I=1,NW
      TO(NW+1-I) = FROM(NW+1-I)
   END DO
   RETURN
END SUBROUTINE BLKMOV
