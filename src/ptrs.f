      SUBROUTINE PTRS(IP1,IP2,K,NATT3,PTABLE,LEN,ITYPE)

         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE LOCATES THE PAIRS OF POINTERS TO COMMON
C  ATTRIBUTES FOR A SUBTRACT OR INTERSECT
C
         INCLUDE 'ptbl.inc'
         INTEGER PTABLE(PTBLL,1)
C
         IF(K.GT.NATT3) GO TO 500
C
  100    CONTINUE
         I = K
         IF(PTABLE(PTBL2,I).EQ.0) GO TO 200
         IF(PTABLE(PTBL3,I).EQ.0) GO TO 200
         IP1 = PTABLE(PTBL2,I)
         IP2 = PTABLE(PTBL3,I)
         CALL ITOH(IDUM,LEN,PTABLE(PTBL5,I))
         ITYPE = PTABLE(PTBL6,I)
         K = K + 1
         GO TO 9999
  200    CONTINUE
         K = K + 1
         IF(K.GT.NATT3) GO TO 500
         GO TO 100
  500    CONTINUE
C
C  DONE GOING THROUGH THE POINTERS.
C
         K = 0
         LEN = 0
 9999    RETURN
      END
