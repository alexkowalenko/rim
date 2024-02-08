      SUBROUTINE RMXPUT(INDPTR,TUPLE)

         USE Globals, only:  DBDATE, RMSTAT
         USE Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE PUTS DATA FROM TUPLE INTO THE CURRENT ROW.
C
C  PARAMETERS:
C         INDPTR--INDEX TO SAVE BLOCK
C         TUPLE---USER ARRAY WITH REPLACEMENT TUPLE
C
         INCLUDE 'keydat.inc'
         INCLUDE 'vardat.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'start.inc'
         INCLUDE 'picom.inc'
C
         INTEGER TUPLE(1)
         RMSTAT = 0
C
C  CHECK THAT RMGET WAS CALLED
C
         IF((IVAL.LE.0).OR.(IVAL.GE.ALL9S)) THEN
            RMSTAT = 60
            GO TO 9999
         ENDIF
C
C  CONVERT THE VARIABLE ATTRIBUTE HEADERS FROM USER TO INTERNAL
C
         IF (NUMVAR.GT.0) THEN
            CALL RMVARC(1,TUPLE)
            IF(RMSTAT.NE.0) GO TO 9999
         ENDIF
C
C  RETRIEVE THE CURRENT ROW IN A SCRATCH TUPLE.
C
  290    CALL BLKCHG(9,MAXCOL,1)
         KQ1 = BLKLOC(9)
         NID = CID
         INDEX = INDPTR
         IF(INDEX.EQ.0) INDEX = 1
         IF(INDEX.GT.3) INDEX = 3
         LNBOO = NBOO
         NBOO = 0
         LNS = NS
         NS = 0
         LIMVAL = LIMVAL - 1
         IVAL = IVAL - 1
         CALL RMLOOK(BUFFER(KQ1),INDEX,0,KURLEN)
         NS = LNS
         NBOO = LNBOO
         IF (RMSTAT.NE.0) THEN
C        NO DATA AVAILABLE
            RMSTAT = 60
            GO TO 9999
         ENDIF
C
C  SEE IF THE NEW TUPLE IS LONGER THAN THE OLD ONE.
C
         NEWL = KURLEN
         IF(NUMVAR.EQ.0) GO TO 370
         I = LOCATT(BLANK,NAME)
         NEWL = 0
C
  320    CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 360
         NWORDS = ATTWDS
         IF (ATTWDS.EQ.0) THEN
C
C        VARIABLE LENGTH ATTRIBUTE.
C
            COLUMN = TUPLE(ATTCOL)
            IF((COLUMN.LE.1).OR.(COLUMN.GT.MAXCOL)) GO TO 800
            NWORDS = TUPLE(COLUMN) + 3
            IF(NWORDS.LT.3) GO TO 800
         ENDIF
         NEWL = NEWL + NWORDS
         GO TO 320
C
  360    IF(NEWL.GT.MAXCOL) GO TO 800
  370    IF(NEWL.LE.KURLEN) GO TO 500
C
C  NEW TUPLE IS LONGER THAN THE OLD ONE.
C  OLD TUPLE MUST BE DELETED AND THE CHANGED ONE ADDED.
C
         CALL DELDAT(INDEX,CID)
C
C  CHANGE THE POINTERS FOR ANY KEY ELEMENTS.
C
         IF(NUMKEY.EQ.0) GO TO 440
         I = 0
         IF(NUMKEY.LE.5) GO TO 380
         I = LOCATT(BLANK,NAME)
  380    IF(NUMKEY.GT.5) GO TO 390
         I = I + 1
         IF(I.GT.NUMKEY) GO TO 440
         START = KEYDAT(1,I)
         COLUMN = KEYDAT(2,I)
         ATTWDS = KEYDAT(3,I)
         ATTYPE = KEYDAT(4,I)
         GO TO 395
C
  390    CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 440
         IF(ATTKEY.EQ.0) GO TO 380
         START = ATTKEY
         COLUMN = ATTCOL
C
  395    IF(ATTWDS.NE.0) GO TO 400
         COLUMN = BUFFER(KQ1+COLUMN-1) + 2
C
  400    IF(BUFFER(KQ1+COLUMN-1).EQ.NULL) GO TO 380
         CALL BTREP(BUFFER(KQ1+COLUMN-1),0,CID,ATTYPE)
         GO TO 380
C
C  ADD THE NEW TUPLE.
C
  440    IF(CID.EQ.RSTART) RSTART = NID
         CALL ADDDAT(INDEX,REND,TUPLE,NEWL)
         RDATE = DBDATE
         CALL RELPUT
         STAT2=0
         CID2=REND
         NID2=0
         LEN2=NEWL
         RST2=RSTART
         REN2=REND
C
C  FIX UP THE KEYS FOR THE ADDED TUPLE.
C
         IF(NUMKEY.EQ.0) GO TO 9999
         I = 0
         IF(NUMKEY.LE.5) GO TO 460
         I = LOCATT(BLANK,NAME)
C
  460    IF(NUMKEY.GT.5) GO TO 470
         I = I + 1
         IF(I.GT.NUMKEY) GO TO 9999
         START = KEYDAT(1,I)
         COLUMN = KEYDAT(2,I)
         ATTWDS = KEYDAT(3,I)
         ATTYPE = KEYDAT(4,I)
         GO TO 475
C
  470    CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 9999
         IF(ATTKEY.EQ.0) GO TO 460
         START = ATTKEY
         KSTART = ATTKEY
         COLUMN = ATTCOL
C
  475    IF(ATTWDS.NE.0) GO TO 480
         COLUMN = TUPLE(COLUMN) + 2
C
  480    IF(TUPLE(COLUMN).EQ.NULL) GO TO 460
         CALL BTADD(TUPLE(COLUMN),REND,ATTYPE)
         IF(START.EQ.KSTART) GO TO 460
         IF(NUMKEY.LE.5) GO TO 490
         ATTKEY = START
         CALL ATTPUT(ISTAT)
         GO TO 460
C
  490    ISTAT = LOCATT(KEYDAT(5,I),NAME)
         CALL ATTGET(ISTAT)
         ATTKEY = START
         CALL ATTPUT(ISTAT)
         GO TO 460
C
C  NEW TUPLE WILL FIT IN PLACE.
C  LENGTH = KURLEN TO ALLOW POSSIBLE REUSE OF END OF TUPLE
C
  500    CALL PUTDAT(INDEX,CID,TUPLE,KURLEN)
         RDATE = DBDATE
         CALL RELPUT
         STAT2=0
         CID2=CID
         NID2=0
         LEN2=NEWL
         RST2=RSTART
         REN2=REND
C
C  CHANGE THE POINTERS FOR ANY KEY ATTRIBUTES.
C
         IF(NUMKEY.EQ.0) GO TO 9999
         I = 0
         IF(NUMKEY.LE.5) GO TO 520
         I = LOCATT(BLANK,NAME)
C
  520    IF(NUMKEY.GT.5) GO TO 530
         I = I + 1
         IF(I.GT.NUMKEY) GO TO 9999
         START = KEYDAT(1,I)
         KSTART = KEYDAT(1,I)
         IPOLD = KEYDAT(2,I)
         IPNEW = IPOLD
         ATTWDS = KEYDAT(3,I)
         ATTYPE = KEYDAT(4,I)
         GO TO 535
C
  530    CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 9999
         IF(ATTKEY.EQ.0) GO TO 520
         START = ATTKEY
         KSTART = ATTKEY
         IPOLD = ATTCOL
         IPNEW = ATTCOL
  535    CONTINUE
         IF(ATTWDS.NE.0) GO TO 540
C
C  VARIABLE LENGTH ATTRIBUTE.
C
         IPOLD = BUFFER(KQ1+IPOLD-1) + 2
         IPNEW = TUPLE(IPNEW) + 2
         IF((IPNEW.LT.1).OR.(IPNEW.GT.MAXCOL)) GO TO 800
C
  540    IF(BUFFER(KQ1+IPOLD-1).EQ.TUPLE(IPNEW)) GO TO 520
C
C  THE VALUE CHANGED.
C
         IF(BUFFER(KQ1+IPOLD-1).NE.NULL)
     +   CALL BTREP(BUFFER(KQ1+IPOLD-1),0,CID,ATTYPE)
         IF(TUPLE(IPNEW).NE.NULL)
     +   CALL BTADD(TUPLE(IPNEW),CID,ATTYPE)
         IF(START.EQ.KSTART) GO TO 520
         IF(NUMKEY.LE.5) GO TO 550
         ATTKEY = START
         CALL ATTPUT(ISTAT)
         GO TO 520
C
  550    ISTAT = LOCATT(KEYDAT(5,I),NAME)
         CALL ATTGET(ISTAT)
         ATTKEY = START
         CALL ATTPUT(ISTAT)
         GO TO 520
C
C  NEW TUPLE HAS VARIABLE LENGTH POINTERS WHICH ARE WIERD.
C
  800    RMSTAT = 100
C
 9999    CONTINUE
         RETURN
      END
