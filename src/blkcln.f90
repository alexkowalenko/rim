SUBROUTINE BLKCLN
   !!
   !!  PURPOSE: CLEAN OUT THE ENTIRE BUFFER AREA
   !!
   !!  RM_Parameters -- NONE
   !!
   USE RM_Parameters
   USE RM_Blocks, only: BLKCLR, BLKLOC
   USE Files, only : FILE2, LENBF2, CURBLK, MODFLG
   USE RM_BufferData, only: BUFFER
   USE RM_Globals, only : RMSTAT
   USE RandomFiles, only : RIOOUT

   implicit none

   INTEGER :: I, IOS, KQ1
   !
   !  WRITE OUT ANY PAGES THAT HAVE BEEN MODIFIED
   !
   DO I=1,3
      IF(MODFLG(I).EQ.0) GO TO 90
      KQ1 = BLKLOC(I)
      CALL RIOOUT(FILE2,CURBLK(I),BUFFER(KQ1),LENBF2,IOS)
      IF(IOS.NE.0) RMSTAT = 2200 + IOS
      MODFLG(I) = 0
90    CONTINUE
      CURBLK(I) = 0
      CALL BLKCLR(I)
   END DO
   DO I=4,9
      CALL BLKCLR(I)
   END DO
   RETURN
END SUBROUTINE BLKCLN
