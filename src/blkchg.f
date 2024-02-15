      SUBROUTINE BLKCHG(IND,NROWS,NCOLS)

         USE RM_Globals, only : TRACE, RMSTAT
         USE Extern, only: IMSG
         USE Utils, only : ZEROIT

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    CHANGE THE DIMENSIONS OF AN EXISTING BLOCK
C
C  RM_Parameters
C     INPUT:   IND-----BLOCK INDEX
C              NROWS---NUMBER OF ROWS
C              NCOLS---NUMBER OF COLUMNS
         INCLUDE 'incore.inc'
         INCLUDE 'buffer.inc'
C
C     TRACING
         IF (TRACE.GE.9) THEN
            CALL MSG('T','BLKCHG:','+')
            CALL IMSG(IND,5,'+')
            CALL IMSG(NROWS,5,'+')
            CALL IMSG(NCOLS,5,'+')
            CALL IMSG(BLOCKS(1,IND),6,'+')
            CALL IMSG(BLOCKS(2,IND),5,'+')
            CALL IMSG(BLOCKS(3,IND),5,' ')
         ENDIF
C
C  SEE IF THE BLOCK HAS EXISTING DATA.
C
         IF(BLOCKS(1,IND).NE.0) GO TO 100
C
C  USE BLKDEF SINCE THIS IS A NEW BLOCK.
C
         CALL BLKDEF(IND,NROWS,NCOLS)
         RETURN
C
C  EXTRACT THE EXISTING DIMENSIONS.
C
  100    CONTINUE
         KNR = BLOCKS(2,IND)
         KNC = BLOCKS(3,IND)
         NWOLD = KNR * KNC
         KS = BLOCKS(1,IND)
C
C  SEE IF WE EXPAND OR CONTRACT.
C
         NWNEW = NROWS * NCOLS
         IF(NWNEW.EQ.NWOLD) RETURN
         NWADD = NWNEW - NWOLD
         IF(NEXT + NWADD .GT. LIMIT) GO TO 7500
C
C  MAKE ROOM IN THE BUFFER.
C
         MOVE = NEXT - (KS+NWOLD)
         IF(NWADD.GT.0) MOVE = -MOVE
         IF(KS + NWOLD .LT. NEXT)
     X    CALL BLKMOV(BUFFER(KS+NWNEW),BUFFER(KS+NWOLD),MOVE)
         IF(NWADD.GT.0) CALL ZEROIT(BUFFER(KS+NWOLD),NWADD)
C
C  UPDATE THE INCORE INDEX.
C
         BLOCKS(1,IND) = KS
         BLOCKS(2,IND) = NROWS
         BLOCKS(3,IND) = NCOLS
         DO 200 I=1,NUMBL
            IF(BLOCKS(1,I).EQ.0) GO TO 200
            ITEST = BLOCKS(1,I)
            IF(ITEST.LE.KS) GO TO 200
            BLOCKS(1,I) = BLOCKS(1,I) + NWADD
  200    CONTINUE
         NEXT = NEXT + NWADD
         RETURN
C
C  NOT ENOUGH ROOM.
C
 7500    CONTINUE
         RMSTAT = 1001
         RETURN
      END
