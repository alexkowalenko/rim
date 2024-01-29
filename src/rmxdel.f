      SUBROUTINE RMXDEL(INDPTR)

         USE Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C     DELETES THE CURRENT ROW.
C
C  PARAMETERS:
C         INDPTR--INDEX TO SAVE BLOCK (RANGE OF 1 TO 9)
         INCLUDE 'keydat.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'ascpar.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'start.inc'
         INTEGER COLUMN
         RMSTAT = 0
C
C
C  CHECK THAT RMGET WAS CALLED
C
         IF ((IVAL.LE.0).OR.(IVAL.GE.ALL9S)) THEN
            RMSTAT = 60
            GO TO 9999
         ENDIF
C
C  RETRIEVE THE CURRENT ROW IN A SCRATCH TUPLE.
C
         CALL BLKCHG(9,MAXCOL,1)
         KQ1 = BLKLOC(9)
         NID = CID
         INDEX = INDPTR
         IF(INDEX.EQ.0) INDEX = 1
         IF(INDEX.GT.3) INDEX = 3
         LNS = NS
         NS = 0
         LNBOO = NBOO
         NBOO = 0
         LIMVAL = LIMVAL - 1
         IVAL = IVAL - 1
         CALL RMLOOK(BUFFER(KQ1),INDEX,0,KURLEN)
         NBOO = LNBOO
         NS = LNS
         IF (RMSTAT.NE.0) THEN
C        NO DATA AVAILABLE
            RMSTAT = 60
            GO TO 9999
         ENDIF
C
C  DELETE THE CURRENT ROW OF THE RELATION.
C
  300    CALL DELDAT(INDEX,CID)
         RDATE = DBDATE
         NTUPLE = NTUPLE - 1
         CALL RELPUT
C
C  CHANGE THE POINTERS FOR ANY KEY ELEMENTS.
C
         IF(NUMKEY.EQ.0) GO TO 9999
         I = 0
         IF(NUMKEY.LE.5) GO TO 380
         I = LOCATT(BLANK,NAME)
  380    CONTINUE
         IF(NUMKEY.GT.5) GO TO 390
         I = I + 1
         IF(I.GT.NUMKEY) GO TO 9999
         START = KEYDAT(1,I)
         COLUMN = KEYDAT(2,I)
         ATTWDS = KEYDAT(3,I)
         ATTYPE = KEYDAT(4,I)
         GO TO 395
  390    CONTINUE
         CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 9999
         IF(ATTKEY.EQ.0) GO TO 380
         START = ATTKEY
         COLUMN = ATTCOL
  395    CONTINUE
         IF(ATTWDS.NE.0) GO TO 400
         COLUMN = BUFFER(KQ1+COLUMN-1) + 2
  400    CONTINUE
         IF(BUFFER(KQ1+COLUMN-1).EQ.NULL) GO TO 380
         CALL BTREP(BUFFER(KQ1+COLUMN-1),0,CID,ATTYPE)
         GO TO 380
C
 9999    RETURN
      END
