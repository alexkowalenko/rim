      SUBROUTINE GTSORT(MAT,INDEX,IFLAG,LENGTH)

         USE RM_Blocks, only: BLKCHG, BLKCLR, BLKLOC
         USE RM_BufferData, only: BUFFER
         USE RM_Globals, only : MRINDX, RMSTAT

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  READ IN TUPLES FROM THE SORTED DATA FILE
C            UNIT = ZNSRT + MRINDX
C
C  RM_Parameters:
C            MAT-----ARRAY TO HOLD ONE TUPLE (IF IFLAG = 0)
C                    POINTER TO TUPLE IN BUFFER (IF IFLAG = 1)
C            INDEX---PAGE BUFFER TO USE
C            IFLAG---0 IF THE TUPLE IS RETURNED IN MAT
C                    1 IF THE BUFFER POINTER IS RETURNED IN MAT
C                   -1 OPEN THE SORT FILE AND INITIALIZE
C            LENGTH--LENGTH OF TUPLE IN WORDS
C
         INCLUDE 'srtcom.inc'
         INCLUDE 'whcom.inc'
C
         INCLUDE 'rimptr.inc'
         DIMENSION MAT(1)
C
C     USE BLOCKS 8-10 (FOR INDICES 1-3)
C     SEE PGEXEC FOR OTHER BLOCK USAGE
C     (THERE OUGHT TO BE MORE BLOCKS)
C     THIS IS WHY SORTED RETRIEVALS ARE FOR PI = 1,2,3 ONLY
C
         GTSB = INDEX + 7
         IF (GTSB.LT.8 .OR. GTSB.GT.10) GOTO 900
C
C     IDENTIFY THE PROPER UNIT
C
         SORTUN = ZNSRT + MRINDX

         IF(IFLAG.EQ.-1) THEN
C
C       FIRST CALL -- INITIALIZE
C
            REWIND SORTUN
C
C       ESTABLISH THE NEW BUFFER BLOCK
C
            CALL BLKCHG(GTSB,MAXCOL,1)
C
C       SET THE TUPLES READ COUNTED TO 0
C
            NREAD = 0
            RETURN
         ENDIF
C
C     NOT FIRST CALL
C
C     READ IN A TUPLE FROM THE SORT FILE
C
         KQ1 = BLKLOC(GTSB) - 1
         NREAD = NREAD + 1
         IF(NREAD.GT.LIMTU) GO TO 900
         IF(NREAD.GT.NSORT) GO TO 900
         IF (FIXLT) THEN
C
C  FIXED LENGTH TUPLES
C
            IF (NKSRT0.EQ.1) THEN
               LENGTH = LTUPLE - 3
               READ(SORTUN) (BUFFER(KQ1+K),K=1,LENGTH),
     X         CID,NID,IVAL
            ELSE
               LENGTH = LTUPLE
               READ(SORTUN) (BUFFER(KQ1+K),K=1,LENGTH)
            ENDIF
         ELSE
C
C  VARIABLE LENGTH TUPLES
C
            IF (NKSRT0.EQ.4) THEN
               READ(SORTUN) LENGTH, JUNK,LENGTH,JUNK,
     X         (BUFFER(KQ1+K),K=1,LENGTH),
     X         CID,NID,IVAL
            ELSE IF (NKSRT0.EQ.1) THEN
               READ(SORTUN) LENGTH, (BUFFER(KQ1+K),K=1,LENGTH-3),
     X         CID,NID,IVAL
            ELSE
               READ(SORTUN) LENGTH, (BUFFER(KQ1+K),K=1,LENGTH)
            ENDIF
         ENDIF
C
C  TUPLE READ - SET MAT AND RMSTAT
C
         RMSTAT = 0
         MAT(1) = KQ1 + 1
         IF(IFLAG.NE.0) GO TO 999
C
C  LOAD TUPLE INTO MAT
C
         DO 800 K=1,LENGTH
            MAT(K) = BUFFER(KQ1+K)
  800    CONTINUE
         GO TO 999
C
C  ALL DONE - SET RMSTAT AND CLOSE THE FILE
C
  900    RMSTAT = -1
         CALL BLKCLR(GTSB)
         CLOSE (UNIT=SORTUN,STATUS='DELETE')
C
  999    RETURN
      END
