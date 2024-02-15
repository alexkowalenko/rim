      SUBROUTINE BLKUP

         USE Files, only: CURBLK, MODFLG
         USE RM_Globals, only : RMSTAT
         USE Utils, only: ZEROIT

         INCLUDE 'syspar.inc'
C
C  MAKE A NEW BLOCK SET.
C
C
         INCLUDE 'buffer.inc'
         INCLUDE 'incore.inc'
C  ASSUME WE ARE GOING TO RUN OUT OF SPACE.
         ISTAT = RMSTAT
         RMSTAT = 1001
C
C  SEE IF THERE IS ENOUGH ROOM TO STORE THE BLOCK INFO.
         NEW = 69 + NEXT - 1
C
C  IF NOT ENOUGH ROOM, EXIT.
         IF (NEW .GE. LIMIT) then
            write(6,9997) new, limit
 9997       format(' new, limit ', i6, i6)
            GOTO 900
         endif
CCCCC CALL BLKCLN
C
C  AOK.
         RMSTAT = ISTAT
C
C  MOVE OLD BLOCK INFO INTO RIMBUF COMMON
         CALL BLKMOV(BUFFER(NEXT),BLOCKS,63)
         CALL BLKMOV(BUFFER(NEXT+63),CURBLK,3)
         CALL BLKMOV(BUFFER(NEXT+66),MODFLG,3)
         NEXT = NEXT + 69
C
C  CLEAR OUT OLD BLOCK INFO.
         CALL ZEROIT(BLOCKS,60)
         CALL ZEROIT(CURBLK,3)
         CALL ZEROIT(MODFLG,3)
         NUMBL = 0
C
  900    CONTINUE
         RETURN
      END
