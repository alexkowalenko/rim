      SUBROUTINE BLKCLR(IND)

         USE RM_Globals, only : TRACE
         USE Extern, only: IMSG
         USE Utils, only : ZEROIT

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    CLEAR A BLOCK FROM THE INCORE BUFFER
C
C     IND-----BLOCK INDEX


         INCLUDE 'incore.inc'
         INCLUDE 'buffer.inc'
C
C     TRACING
         IF (TRACE.GE.11) THEN
            CALL MSG('T','BLKCLR:','+')
            CALL IMSG(IND,5,'+')
            CALL IMSG(BLOCKS(1,IND),6,'+')
            CALL IMSG(BLOCKS(2,IND),5,'+')
            CALL IMSG(BLOCKS(3,IND),5,' ')
         ENDIF
C
C  SEE IF ANYTHING IS THERE NOW.
C
         IF(BLOCKS(1,IND).EQ.0) RETURN
         KNR = BLOCKS(2,IND)
         KNC = BLOCKS(3,IND)
         NWOLD = KNR * KNC
         KS = BLOCKS(1,IND)
C
C  ZERO OUT THE SPACE.
C
         CALL ZEROIT(BUFFER(KS),NWOLD)
C
C  COMPRESS THE REMAINING BLOCKS.
C
         MOVE = NEXT - (KS+NWOLD)
         IF(KS+NWOLD.NE.NEXT)
     X    CALL BLKMOV(BUFFER(KS),BUFFER(KS + NWOLD),MOVE)
C
C  UPDATE THE INCORE INDEX.
C
         BLOCKS(1,IND) = 0
         DO 100 I=1,NUMBL
            IF(BLOCKS(1,I).EQ.0) GO TO 100
            IF(BLOCKS(1,I).LE.KS) GO TO 100
            BLOCKS(1,I) = BLOCKS(1,I) - NWOLD
  100    CONTINUE
         NEXT = NEXT - NWOLD
         IF(IND.EQ.NUMBL) NUMBL = NUMBL - 1
         RETURN
      END
