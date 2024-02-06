      SUBROUTINE ATTNEW(RNAME,NATT)
         USE Files, only : LF1REC
         INCLUDE 'syspar.inc'
C
C  PURPOSE:   ADD A NEW RELATION TO THE ATTRIBUTE RELATION
C
C  PARAMETERS:
C         RNAME---NAME OF A RELATION
C         NATT----NUMBER OF ATTRIBUTES IN THE RELATION
         INCLUDE 'rmatts.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'start.inc'
         INCLUDE 'dclar1.inc'
C
C  ADJUST NAROW IF ALL ATTRIBUTES WILL NOT FIT ON THE PAGE.
C
         MRSTRT = NAROW
         CALL ATTPAG(MRSTRT)
         I = MRSTRT
         IF((I + NATT).LE.APBUF) GO TO 100
   10    CONTINUE
         NAROW = (APBUF * LF1REC) + 1
C
C  CHECK TO SEE THAT WE WILL ACTUALLY POINT TO A NEW PAGE.
C
         IF(NAROW.GT.IABS(ATTBUF(2))) GO TO 20
C
C  THIS IS STRANGE.
C
         LF1REC = LF1REC + 1
         GO TO 10
   20    CONTINUE
         ATTBUF(1) = NAROW
         ATTMOD = 1
         MRSTRT = NAROW
         CALL ATTPAG(MRSTRT)
  100    CONTINUE
         IF(START.NE.KSFRIA) KSFRIA = START
         RETURN
      END
