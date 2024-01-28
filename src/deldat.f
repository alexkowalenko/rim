      SUBROUTINE DELDAT(INDEX,ID)

         USE RandomFiles, only : RIOIN, RIOOUT
         USE Utils, only : HTOI, ITOH

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   DELINK A TUPLE FROM THE DATA FILE
C
C  PARAMETERS:
C         INDEX---BLOCK REFERENCE NUMBER
C         ID------PACKED ID WORD WITH OFFSET,IOBN
         INCLUDE 'f2com.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'flags.inc'
C
         INTEGER OFFSET
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
         CURBLK(NUMBLK) = IOBN
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
  400    CONTINUE
         MODFLG(NUMBLK) = 1
         IFMOD = .TRUE.
C
C  CHANGE THE ID POINTER.
C
         KQ0 = BLKLOC(NUMBLK) - 1
         BUFFER(KQ0 + OFFSET) = -BUFFER(KQ0 + OFFSET)
         IF(BUFFER(KQ0+OFFSET).EQ.100000)BUFFER(KQ0+OFFSET)=0
         MODFLG(NUMBLK) = 1
         IFMOD = .TRUE.
         IF(BUFFER(KQ0 + OFFSET).NE.0) RETURN
C
C  SPECIAL STUFF FOR DELETING THE LAST TUPLE.
C
         CALL HTOI(1,0,BUFFER(KQ0 + OFFSET))
         BUFFER(KQ0 + OFFSET) = -BUFFER(KQ0 + OFFSET)
         RETURN
      END
