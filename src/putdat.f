      SUBROUTINE PUTDAT(INDEX,ID,ARRAY,LENGTH)

         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   REPLACE A TUPLE ON THE DATA FILE
C
C  PARAMETERS:
C         INDEX---BLOCK REFERENCE NUMBER
C         ID------PACKED ID WORD WITH OFFSET,IOBN
C         ARRAY---ARRAY TO RECEIVE THE TUPLE
C         LENGTH--LENGTH OF THE TUPLE
         INCLUDE 'f2com.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'flags.inc'
C
         INTEGER OFFSET
         INTEGER ARRAY(1)
C
C  UNPAC THE ID WORD.
C
         CALL ITOH(OFFSET,IOBN,ID)
C
C  SEE IF THE NEEDED BLOCK IS CURRENTLY IN CORE.
C
         NUMBLK = 0
         DO 200 I=1,3
            IF(IOBN.EQ.CURBLK(I)) NUMBLK = I
  200    CONTINUE
         IF(NUMBLK.NE.0) GO TO 400
         NUMBLK = INDEX
C
C  WE MUST DO PAGING.
C
C  SEE IF THE CURRENT BLOCK NEEDS WRITING.
C
         IF(MODFLG(NUMBLK).EQ.0) GO TO 300
C
C  WRITE OUT THE CURRENT BLOCK.
C
         KQ1 = BLKLOC(NUMBLK)
         CALL RIOOUT(FILE2,CURBLK(NUMBLK),BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
  300    CONTINUE
C
C  READ IN THE NEEDED BLOCK.
C
         CALL BLKCHG(NUMBLK,LENBF2,1)
         KQ1 = BLKLOC(NUMBLK)
         CALL RIOIN(FILE2,IOBN,BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
         CURBLK(NUMBLK) = IOBN
  400    CONTINUE
         MODFLG(NUMBLK) = 1
         IFMOD = .TRUE.
C
C  MOVE THE TUPLE TO THE PAGE.
C
         KQ0 = BLKLOC(NUMBLK) - 1
         LEN = BUFFER(KQ0 + OFFSET + 1)
         IF(LEN.NE.LENGTH) RMSTAT = 1002
         CALL BLKMOV(BUFFER(KQ0 + OFFSET + 2),ARRAY(1),LEN)
C
C  ALL DONE.
C
         RETURN
      END
