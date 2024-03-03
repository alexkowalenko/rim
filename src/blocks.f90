MODULE RM_Blocks
   implicit none
   private

   !  *** / I N C O R E / ***
   !
   !  CONTROL VARIABLES FOR INCORE BUFFER MANAGEMENT
   !

   !COMMON /INCORE/ BLOCKS(3,20),NEXT,LIMIT,NUMBL
   INTEGER, public :: BLOCKS(3,20)
   !!     BLOCKS--ARRAY WITH POINTERS AND DIMENSIONS OF INCORE BLOCKS
   !!         ROW 1---STARTING POSITION
   !!         ROW 2---NUMBER OF ROWS
   !!         ROW 3---NUMBER OF COLUMNS
   INTEGER, public :: NEXT
   !!     NEXT----NEXT AVAILABLE ADDRESS IN THE BUFFER
   INTEGER, public :: LIMIT
   !!     LIMIT---LAST WORD IN THE BUFFER
   INTEGER, public :: NUMBL
   !!     NUMBL---NUMBER OF BLOCKS DEFINED

   public BLKCHG
   public BLKCLR
   public BLKDEF
   public BLKDSP
   public BLKEXT
   public BLKLOC
   public BLKNXT
   public Intialise

contains

   SUBROUTINE Intialise
      USE RM_Parameters, only: ZBUF
      USE Utils, only : ZEROIT

      CALL ZEROIT(BLOCKS(1,1),60)
      NEXT = 1
      LIMIT = ZBUF
      NUMBL = 0
   END SUBROUTINE


   SUBROUTINE BLKCHG(IND,NROWS,NCOLS)
      !!
      !!  PURPOSE:    CHANGE THE DIMENSIONS OF AN EXISTING BLOCK
      !!
      !!  RM_Parameters
      !! INPUT:   IND-----BLOCK INDEX
      !!          NROWS---NUMBER OF ROWS
      !!          NCOLS---NUMBER OF COLUMNS

      USE RM_Parameters
      USE RM_Globals, only : TRACE, RMSTAT
      USE RM_BufferData, only: BUFFER
      USE Extern, only: IMSG, MSG
      USE Utils, only : ZEROIT

      INTEGER, intent(in) :: IND
      INTEGER, intent(in) :: NROWS
      INTEGER, intent(in) :: NCOLS

      INTEGER :: KNR, KNC, NWOLD, KS, NWNEW, NWADD, MOVE, I, ITEST

      !
      ! TRACING
      IF (TRACE.GE.9) THEN
         CALL MSG('T','BLKCHG:','+')
         CALL IMSG(IND,5,'+')
         CALL IMSG(NROWS,5,'+')
         CALL IMSG(NCOLS,5,'+')
         CALL IMSG(BLOCKS(1,IND),6,'+')
         CALL IMSG(BLOCKS(2,IND),5,'+')
         CALL IMSG(BLOCKS(3,IND),5,' ')
      ENDIF
      !
      !  SEE IF THE BLOCK HAS EXISTING DATA.
      !
      IF(BLOCKS(1,IND).NE.0) GO TO 100
      !
      !  USE BLKDEF SINCE THIS IS A NEW BLOCK.
      !
      CALL BLKDEF(IND,NROWS,NCOLS)
      RETURN
      !
      !  EXTRACT THE EXISTING DIMENSIONS.
      !
100   CONTINUE
      KNR = BLOCKS(2,IND)
      KNC = BLOCKS(3,IND)
      NWOLD = KNR * KNC
      KS = BLOCKS(1,IND)
      !
      !  SEE IF WE EXPAND OR CONTRACT.
      !
      NWNEW = NROWS * NCOLS
      IF(NWNEW.EQ.NWOLD) RETURN
      NWADD = NWNEW - NWOLD
      IF(NEXT + NWADD .GT. LIMIT) GO TO 7500
      !
      !  MAKE ROOM IN THE BUFFER.
      !
      MOVE = NEXT - (KS+NWOLD)
      IF(NWADD.GT.0) MOVE = -MOVE
      IF(KS + NWOLD .LT. NEXT) &
         CALL BLKMOV(BUFFER(KS+NWNEW),BUFFER(KS+NWOLD),MOVE)
      IF(NWADD.GT.0) CALL ZEROIT(BUFFER(KS+NWOLD),NWADD)
      !
      !  UPDATE THE INCORE INDEX.
      !
      BLOCKS(1,IND) = KS
      BLOCKS(2,IND) = NROWS
      BLOCKS(3,IND) = NCOLS
      DO I=1,NUMBL
         IF(BLOCKS(1,I).EQ.0) GO TO 200
         ITEST = BLOCKS(1,I)
         IF(ITEST.LE.KS) GO TO 200
         BLOCKS(1,I) = BLOCKS(1,I) + NWADD
200      CONTINUE
      END DO
      NEXT = NEXT + NWADD
      RETURN
      !
      !  NOT ENOUGH ROOM.
      !
7500  CONTINUE
      RMSTAT = 1001
      RETURN
   END SUBROUTINE BLKCHG


   SUBROUTINE BLKCLR(IND)
      !!
      !!  PURPOSE:    CLEAR A BLOCK FROM THE INCORE BUFFER
      !!
      !! IND-----BLOCK INDEX

      USE RM_Parameters
      USE RM_Globals, only : TRACE
      USE RM_BufferData, only: BUFFER
      USE Extern, only: IMSG, MSG
      USE Utils, only : ZEROIT

      INTEGER, intent(in) :: IND

      INTEGER :: KNR, KNC, NWOLD, KS, MOVE, I

      !
      ! TRACING
      IF (TRACE.GE.11) THEN
         CALL MSG('T','BLKCLR:','+')
         CALL IMSG(IND,5,'+')
         CALL IMSG(BLOCKS(1,IND),6,'+')
         CALL IMSG(BLOCKS(2,IND),5,'+')
         CALL IMSG(BLOCKS(3,IND),5,' ')
      ENDIF
      !
      !  SEE IF ANYTHING IS THERE NOW.
      !
      IF(BLOCKS(1,IND).EQ.0) RETURN
      KNR = BLOCKS(2,IND)
      KNC = BLOCKS(3,IND)
      NWOLD = KNR * KNC
      KS = BLOCKS(1,IND)
      !
      !  ZERO OUT THE SPACE.
      !
      CALL ZEROIT(BUFFER(KS),NWOLD)
      !
      !  COMPRESS THE REMAINING BLOCKS.
      !
      MOVE = NEXT - (KS+NWOLD)
      IF(KS+NWOLD.NE.NEXT) &
         CALL BLKMOV(BUFFER(KS),BUFFER(KS + NWOLD),MOVE)
      !
      !  UPDATE THE INCORE INDEX.
      !
      BLOCKS(1,IND) = 0
      DO I=1,NUMBL
         IF(BLOCKS(1,I).EQ.0) GO TO 100
         IF(BLOCKS(1,I).LE.KS) GO TO 100
         BLOCKS(1,I) = BLOCKS(1,I) - NWOLD
100      CONTINUE
      END DO
      NEXT = NEXT - NWOLD
      IF(IND.EQ.NUMBL) NUMBL = NUMBL - 1
      RETURN
   END SUBROUTINE BLKCLR


   SUBROUTINE BLKDEF(IND,NROWS,NCOLS)
      !!
      !!  PURPOSE:    DEFINE A NEW BLOCK FOR THE INCORE BUFFER
      !!
      !!  RM_Parameters
      !! INPUT:   IND-----BLOCK INDEX
      !!          NROWS---NUMBER OF ROWS
      !!          NCOLS---NUMBER OF COLUMNS

      USE RM_Parameters
      USE RM_Globals, only : TRACE, RMSTAT
      USE RM_BufferData, only: BUFFER
      USE Extern, only: IMSG, MSG
      USE Utils, only : ZEROIT

      INTEGER, intent(in) :: IND
      INTEGER, intent(in) :: NROWS
      INTEGER, intent(in) :: NCOLS

      INTEGER :: NWNEW

      !
      ! TRACING
      IF (TRACE.GE.9) THEN
         CALL MSG('T','BLKDEF:','+')
         CALL IMSG(IND,5,'+')
         CALL IMSG(NROWS,5,'+')
         CALL IMSG(NCOLS,5,' ')
      ENDIF
      !
      !  CLEAR ANY EXISTING BLOCK FOR THIS INDEX.
      !
      IF(BLOCKS(1,IND).NE.0) CALL BLKCLR(IND)
      !
      !  SET UP THE NEW BLOCK.
      !
      NWNEW = NROWS * NCOLS
      IF(NEXT + NWNEW .GT.LIMIT) GO TO 7500
      CALL ZEROIT(BUFFER(NEXT),NWNEW)
      !
      !  UPDATE THE INCORE INDEX.
      !
      BLOCKS(1,IND) = NEXT
      BLOCKS(2,IND) = NROWS
      BLOCKS(3,IND) = NCOLS
      NEXT = NEXT + NWNEW
      IF(IND.GT.NUMBL) NUMBL = IND
      RETURN
      !
      !  NOT ENOUGH ROOM.
      !
7500  CONTINUE
      RMSTAT = 1001
      RETURN
   END SUBROUTINE BLKDEF


   SUBROUTINE BLKDSP(NAME,BLK,CODES,LABELS,LLABEL)
      !!
      !! DISPLAY A BLOCK OF DATA TO LOG
      !!

      USE RM_Parameters
      USE Extern, only : DMSG, IMSG, AMSG, MSG
      USE Utils, only : ITOH

      CHARACTER(len=*), intent(in) :: NAME, CODES
      INTEGER, intent(in) :: LLABEL
      CHARACTER(len=6), intent(in) :: LABELS(LLABEL)

      INTEGER :: BLK(1), IBLK
      LOGICAL :: LBLK
      EQUIVALENCE(IBLK,LBLK)

      INTEGER :: BPTR, H1, H2, I

      INCLUDE 'rmatts.inc'
      !
      CALL MSG(' ',NAME,' ')
      !
      BPTR = 1
      DO I = 1, LEN(CODES)
         CALL MSG(' ',' ','+')
         CALL IMSG(I,4,'+')
         CALL MSG(' ','[','+')
         CALL IMSG(BPTR,4,'+')
         CALL MSG(' ',']','+')
         IF (I.LE.LLABEL) THEN
            CALL MSG(' ','"' // LABELS(I) // '"','+')
         ELSE
            CALL MSG(' ','"      "','+')
         ENDIF
         CALL MSG(' ',' = :','+')
         IF (CODES(I:I).EQ.'Z') THEN
            CALL AMSG(BLK(BPTR),ZC,'+')
            CALL MSG(' ',':',' ')
            BPTR = BPTR + Z
         ELSE IF (CODES(I:I).EQ.'I') THEN
            CALL IMSG(BLK(BPTR),16,' ')
            BPTR = BPTR + 1
         ELSE IF (CODES(I:I).EQ.'H') THEN
            CALL ITOH(H1,H2,BLK(BPTR))
            CALL IMSG(H1,8,'+')
            CALL IMSG(H2,8,' ')
            BPTR = BPTR + 1
         ELSE IF (CODES(I:I).EQ.'L') THEN
            IBLK = BLK(BPTR)
            IF (LBLK) THEN
               CALL MSG(' ','TRUE',' ')
            ELSE
               CALL MSG(' ','FALSE',' ')
            ENDIF
            BPTR = BPTR + 1
         ELSE IF (CODES(I:I).EQ.'D') THEN
            CALL DMSG(BLK(BPTR),0,' ',KZDATE)
            BPTR = BPTR + 1
         ELSE IF (CODES(I:I).EQ.'T') THEN
            CALL DMSG(BLK(BPTR),0,' ',KZTIME)
            BPTR = BPTR + 1
         ELSE
            CALL IMSG(BLK(BPTR),16,' ')
            BPTR = BPTR + 1
         ENDIF
      END DO
      RETURN
   END SUBROUTINE BLKDSP


   SUBROUTINE BLKEXT(IND,NROWS,NCOLS)
      !!
      !!  PURPOSE:    EXTRACT THE NUMBER OF ROWS AND COLUMNS FOR A BLOCK
      !!
      !!  RM_Parameters
      !! INPUT:   IND-----BLOCK INDEX
      !! OUTPUT:  NROWS---NUMBER OF ROWS
      !!          NCOLS---NUMBER OF COLUMNS

      INTEGER, intent(in) :: IND
      INTEGER, intent(out) :: NROWS
      INTEGER, intent(out) :: NCOLS

      !
      !  EXTRACT THE DATA FROM BLOCKS.
      !
      NROWS = BLOCKS(2,IND)
      NCOLS = BLOCKS(3,IND)
      RETURN
   END SUBROUTINE BLKEXT


   INTEGER FUNCTION BLKLOC(IND)
      !!
      !!  PURPOSE:    RETURN THE STARTING ADDRESS FOR THE REQUESTED BLOCK
      !!
      !!  RM_Parameters
      !! INPUT:   IND-----BLOCK INDEX
      !! OUTPUT:  BLKLOC--ADDRESS OF 1,1 ENTRY FOR THE BLOCK
      USE RM_Globals, only : RMSTAT
      USE Extern, only: IMSG, MSG

      INTEGER, intent(in) :: IND

      INTEGER :: KS
      !
      ! TRACING
      !CC   IF (TRACE.GT.20) THEN
      !CC      CALL MSG('T','BLKLOC:','+')
      !CC      CALL IMSG(IND,5,'+')
      !CC      CALL IMSG(BLOCKS(1,IND),6,' ')
      !CC   ENDIF
      !
      KS = BLOCKS(1,IND)
      IF(KS.EQ.0) GO TO 7500
      BLKLOC = KS
      RETURN
      !
      !  UNDEFINED BLOCK.
      !
7500  CONTINUE
      RMSTAT = 1002
      BLKLOC = 0
      RETURN
   END FUNCTION BLKLOC


   SUBROUTINE BLKNXT(NXT)
      !!
      !!  CLEAR OUT THE CURRENT BLOCK SET AND RETURN A POINTER TO THE
      !!  FIRST AVAILABLE WORD IN //BUFFER.
      !!

      INTEGER, intent(out) :: NXT

      !
      CALL BLKCLN
      NXT = NEXT
      RETURN
   END SUBROUTINE BLKNXT


END MODULE RM_Blocks
