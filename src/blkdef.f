      SUBROUTINE BLKDEF(IND,NROWS,NCOLS)

         USE RM_Globals, only : TRACE, RMSTAT
         USE Extern, only: IMSG
         USE Utils, only : ZEROIT

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    DEFINE A NEW BLOCK FOR THE INCORE BUFFER
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
            CALL MSG('T','BLKDEF:','+')
            CALL IMSG(IND,5,'+')
            CALL IMSG(NROWS,5,'+')
            CALL IMSG(NCOLS,5,' ')
         ENDIF
C
C  CLEAR ANY EXISTING BLOCK FOR THIS INDEX.
C
         IF(BLOCKS(1,IND).NE.0) CALL BLKCLR(IND)
C
C  SET UP THE NEW BLOCK.
C
         NWNEW = NROWS * NCOLS
         IF(NEXT + NWNEW .GT.LIMIT) GO TO 7500
         CALL ZEROIT(BUFFER(NEXT),NWNEW)
C
C  UPDATE THE INCORE INDEX.
C
         BLOCKS(1,IND) = NEXT
         BLOCKS(2,IND) = NROWS
         BLOCKS(3,IND) = NCOLS
         NEXT = NEXT + NWNEW
         IF(IND.GT.NUMBL) NUMBL = IND
         RETURN
C
C  NOT ENOUGH ROOM.
C
 7500    CONTINUE
         RMSTAT = 1001
         RETURN
      END
