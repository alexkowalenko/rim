      SUBROUTINE GETDAT(INDEX,ID,LOCTUP,LENGTH)

         USE RM_Globals, only : TRACE, RMSTAT
         USE Files, only : FILE2, LENBF2, LF2REC, CURBLK, MODFLG
         USE RandomFiles, only: RIOIN, RIOOUT
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  GET A TUPLE FROM THE DATA FILE
C
C  RM_Parameters:
C         INDEX---BLOCK REFERENCE NUMBER
C         ID------PACKED ID WORD WITH START,PRU
C         LOCTUP--OFFSET IN BUFFER FOR THE TUPLE
C         LENGTH---LENGTH OF THE TUPLE
         INCLUDE 'buffer.inc'
         INCLUDE 'rimptr.inc'
C
         INTEGER OFFSET
C
C  UNPAC THE ID WORD.
C
         CALL ITOH(OFFSET,IOBN,ID)
  100    CONTINUE
C
C  MAKE SURE WE HAVE A VALID ID.
C
         IF(IOBN.GT.LF2REC) GO TO 600
         IF(OFFSET.GT.LENBF2) GO TO 600
C
C  SEE IF THE NEEDED BLOCK IS CURRENTLY IN CORE.
C
         NUMBLK = 0
         DO 200 I=1,3
            IF(IOBN.EQ.CURBLK(I)) NUMBLK = I
  200    CONTINUE
         IF((NUMBLK.NE.0).AND.(NUMBLK.LE.INDEX)) GO TO 400
C
C  WE MUST DO PAGING.
C
C  SEE IF THE CURRENT BLOCK NEEDS WRITING.
C
         IF(MODFLG(INDEX).EQ.0) GO TO 300
C
C  WRITE OUT THE CURRENT BLOCK.
C
         KQ1 = BLKLOC(INDEX)
         CALL RIOOUT(FILE2,CURBLK(INDEX),BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
  300    CONTINUE
C
C  SEE IF THE BLOCK WE NEED WAS IN ANOTHER SLOT.
C
         IF(NUMBLK.EQ.0) GO TO 350
C
C  IT WAS. MOVE IT FROM NUMBLK TO INDEX.
C
         CALL BLKCHG(INDEX,LENBF2,1)
         KQ1 = BLKLOC(INDEX)
         KQ2 = BLKLOC(NUMBLK)
         CALL BLKMOV(BUFFER(KQ1),BUFFER(KQ2),LENBF2)
         CURBLK(INDEX) = CURBLK(NUMBLK)
         MODFLG(INDEX) = MODFLG(NUMBLK)
         CURBLK(NUMBLK) = 0
         MODFLG(NUMBLK) = 0
         NUMBLK = INDEX
         GO TO 400
  350    CONTINUE
         NUMBLK = INDEX
C
C  READ IN THE NEEDED BLOCK.
C
         CALL BLKCHG(NUMBLK,LENBF2,1)
         KQ1 = BLKLOC(NUMBLK)
         CALL RIOIN(FILE2,IOBN,BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
         CURBLK(NUMBLK) = IOBN
         MODFLG(NUMBLK) = 0
  400    CONTINUE
C
C  MOVE THE DESIRED DATA.
C
         KQ0 = BLKLOC(NUMBLK) - 1
         ID = BUFFER(KQ0 + OFFSET)
         IF(ID.GE.0) GO TO 500
C
C  THIS TUPLE IS NOT ACTIVE. GO TO THE NEXT ONE.
C
         ID = -ID
         CID = ID
         ISOFF = OFFSET
         CALL ITOH(OFFSET,IOBN,ID)
         IF (TRACE.GE.6) THEN
            CALL MSG('T','GETDAT (DEL) ','+')
            CALL IMSG(OFFSET,6,'+')
            CALL IMSG(IOBN,9,' ')
         ENDIF

         IF(IOBN.NE.0) GO TO 100
C
C  WE HAVE AN INACTIVE LAST TUPLE.
C
         ID = -ID
         OFFSET = ISOFF
  500    CONTINUE
         LOCTUP = KQ0 + OFFSET + 2
         LENGTH = BUFFER(LOCTUP - 1)
         IF (TRACE.GE.6) THEN
            CALL MSG('T','GETDAT (OK ) ','+')
            CALL IMSG(OFFSET,6,'+')
            CALL IMSG(IOBN,9,'+')
            CALL IMSG(INDEX,3,'+')
            CALL IMSG(KQ0,5,'+')
            CALL ITOH(N1,N2,ID)
            CALL IMSG(N1,6,'+')
            CALL IMSG(N2,9,' ')
         ENDIF
         RETURN
C
C  BAD ID VALUE.
C
  600    CONTINUE
         ID = 0
         RETURN
      END
