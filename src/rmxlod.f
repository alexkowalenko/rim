      SUBROUTINE RMXLOD(INDPTR,TUPLE)

         USE RM_Globals, only : DBDATE, RMSTAT
         USE RM_Attributes, only: ATTGET, ATTPUT, LOCATT
         USE RM_BTree_Data, only : START
         USE RM_BTree, only: BTADD
         USE RM_Buffer, only: ADDDAT
         USE RM_Relations, only : RELPUT
         USE RM_Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE LOADS DATA FROM TUPLE INTO THE CURRENT RELATION.
C
C  RM_Parameters:
C         INDPTR--INDEX TO SAVE BLOCK
C         TUPLE---USER ARRAY WITH REPLACEMENT TUPLE
         INTEGER TUPLE(1)

         INCLUDE 'vardat.inc'
         INCLUDE 'keydat.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INTEGER COLUMN
         INCLUDE 'picom.inc'
C
         RMSTAT = 0

C
C  SET THE INDEX POINTER
C
         INDEX = INDPTR
         IF(INDEX.EQ.0) INDEX = 1
         IF(INDEX.GT.3) INDEX = 3
C
         NEWL = NCOL
C
C  CONVERT THE VARIABLE ATTRIBUTE HEADERS FROM USER TO INTERNAL
C
         IF(NUMVAR.EQ.0) GOTO 360
         CALL RMVARC(1,TUPLE)
         IF(RMSTAT.NE.0) GO TO 9999
C
C  FIND OUT HOW LONG THE NEW TUPLE IS.
C
         I = LOCATT(BLANK,NAME)
         NEWL = 0
  320    CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 360
         NWORDS = ATTWDS
         IF(ATTWDS.NE.0) GO TO 340
C
C  VARIABLE LENGTH ATTRIBUTE.
C
         COLUMN = TUPLE(ATTCOL)
         IF((COLUMN.LE.1).OR.(COLUMN.GT.MAXCOL)) GO TO 800
         NWORDS = TUPLE(COLUMN) + 3
         IF(NWORDS.LE.3) GO TO 800
  340    NEWL = NEWL + NWORDS
         GO TO 320
  360    IF(NEWL.GT.MAXCOL) GO TO 800
C
C  ADD THE NEW TUPLE.
C
  440    CALL ADDDAT(INDEX,REND,TUPLE,NEWL)
         IF(RSTART.EQ.0) RSTART = REND
         RDATE = DBDATE
         NTUPLE = NTUPLE + 1
         CALL RELPUT
         STAT2=0
         CID2=REND
         NID2=0
         LEN2=NEWL
         RST2=RSTART
         REN2=REND
         IF(NUMKEY.EQ.0) GO TO 9999
C
C  FIX UP THE KEYS FOR THE ADDED TUPLE.
C
         I = 0
         IF(NUMKEY.GT.ZPIKAT) I = LOCATT(BLANK,NAME)
C
  460    IF(NUMKEY.LE.ZPIKAT) THEN
            I  = I + 1
            IF(I.GT.NUMKEY) GO TO 9999
            START = KEYDAT(1,I)
            KSTART = KEYDAT(1,I)
            COLUMN = KEYDAT(2,I)
            ATTWDS = KEYDAT(3,I)
            ATTYPE = KEYDAT(4,I)
         ELSE
            CALL ATTGET(ISTAT)
            IF(ISTAT.NE.0) GO TO 9999
            IF(ATTKEY.EQ.0) GO TO 460
            START = ATTKEY
            KSTART = ATTKEY
            COLUMN = ATTCOL
         ENDIF
         IF(ATTWDS.EQ.0) COLUMN = TUPLE(COLUMN) + 2
         IF(TUPLE(COLUMN).EQ.NULL) GO TO 460
         CALL BTADD(TUPLE(COLUMN),REND,ATTYPE)
         IF(START.EQ.KSTART) GO TO 460
         IF(NUMKEY.GT.ZPIKAT) THEN
            ATTKEY = START
            CALL ATTPUT(ISTAT)
            GO TO 460
         ELSE
            ISTAT = LOCATT(KEYDAT(5,I),NAME)
            CALL ATTGET(ISTAT)
            ATTKEY = START
            CALL ATTPUT(ISTAT)
            KEYDAT(1,I) = START
            GO TO 460
         ENDIF
C
C  NEW TUPLE HAS VARIABLE LENGTH POINTERS WHICH ARE WEIRD.
C
  800    CONTINUE
         RMSTAT = 100
 9999    CONTINUE
         RETURN
      END
