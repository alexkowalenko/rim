      SUBROUTINE BLKCLN

         USE Files, only : FILE2, LENBF2, CURBLK, MODFLG
         USE Globals, only : RMSTAT
         USE RandomFiles, only : RIOOUT

         INCLUDE 'syspar.inc'
C
C  PURPOSE: CLEAN OUT THE ENTIRE BUFFER AREA
C
C  PARAMETERS -- NONE
C
         INCLUDE 'incore.inc'
         INCLUDE 'buffer.inc'
C
C  WRITE OUT ANY PAGES THAT HAVE BEEN MODIFIED
C
         DO 100 I=1,3
            IF(MODFLG(I).EQ.0) GO TO 90
            KQ1 = BLKLOC(I)
            CALL RIOOUT(FILE2,CURBLK(I),BUFFER(KQ1),LENBF2,IOS)
            IF(IOS.NE.0) RMSTAT = 2200 + IOS
            MODFLG(I) = 0
   90       CONTINUE
            CURBLK(I) = 0
            CALL BLKCLR(I)
  100    CONTINUE
         DO 200 I=4,9
            CALL BLKCLR(I)
  200    CONTINUE
         RETURN
      END
