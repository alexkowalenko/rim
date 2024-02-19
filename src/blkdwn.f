      SUBROUTINE BLKDWN

         USE RM_BufferData, only: BUFFER
         USE Files, only : CURBLK, MODFLG
         USE Utils, only : ZEROIT
C
C  GO TO THE NEXT LOWER BLOCK SET.
C
         INCLUDE 'syspar.inc'
         INCLUDE 'incore.inc'
         CALL BLKCLN
         DO 107 II=10,20
  107    CALL BLKCLR(II)

         IF (NEXT .LT. 69+1) GOTO 900
C
C  GET THE OLD BLOCK SET INFO.
         IB = NEXT - 69
         IN = NEXT - 7
         CALL BLKMOV(BLOCKS,BUFFER(IB),63)
         CALL BLKMOV(CURBLK,BUFFER(IB+63),3)
         CALL BLKMOV(MODFLG,BUFFER(IB+66),3)
         NUMBL = BUFFER(IN)
         CALL ZEROIT(BUFFER(IB),69)
         NEXT = IB
  900    CONTINUE
         RETURN
      END
