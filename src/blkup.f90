SUBROUTINE BLKUP
   !!
   !!  MAKE A NEW BLOCK SET.
   !!

   USE RM_Parameters
   USE RM_Blocks, only: BLOCKS, NEXT, LIMIT, NUMBL
   USE RM_BufferData, only: BUFFER
   USE Files, only: CURBLK, MODFLG
   USE RM_Globals, only : RMSTAT
   USE Utils, only: ZEROIT

   implicit none

   INTEGER :: ISTAT, NEW

   !  ASSUME WE ARE GOING TO RUN OUT OF SPACE.
   ISTAT = RMSTAT
   RMSTAT = 1001
   !
   !  SEE IF THERE IS ENOUGH ROOM TO STORE THE BLOCK INFO.
   NEW = 69 + NEXT - 1
   !
   !  IF NOT ENOUGH ROOM, EXIT.
   IF (NEW .GE. LIMIT) then
      write(6,9997) new, limit
9997  format(' new, limit ', i6, i6)
      GOTO 900
   endif
   !CCCC CALL BLKCLN
   !
   !  AOK.
   RMSTAT = ISTAT
   !
   !  MOVE OLD BLOCK INFO INTO RIMBUF COMMON
   CALL BLKMOV(BUFFER(NEXT),BLOCKS,63)
   CALL BLKMOV(BUFFER(NEXT+63),CURBLK,3)
   CALL BLKMOV(BUFFER(NEXT+66),MODFLG,3)
   NEXT = NEXT + 69
   !
   !  CLEAR OUT OLD BLOCK INFO.
   CALL ZEROIT(BLOCKS,60)
   CALL ZEROIT(CURBLK,3)
   CALL ZEROIT(MODFLG,3)
   NUMBL = 0
   !
900 CONTINUE
   RETURN
END SUBROUTINE BLKUP
