      SUBROUTINE BUILD(*)

         USE Globals, only : DFLAG, RMSTAT
         USE Lexer, only : KWS, ITEMS, LXSREC
         USE Message, only : WARN

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  BUILD A KEY INDEX FOR AN ATTRIBUTE IN A RELATION
C
C SYNTAX:  BUILD KEY FOR <ATTRIBUTE> IN <RELATION>
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'start.inc'
         INCLUDE 'files.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'dclar1.inc'
         INTEGER COLUMN
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C
C  SCAN THE COMMAND FOR PROPER SYNTAX.
C
         IF(KWS(2).NE.'KEY') GO TO 950
         IF(KWS(3).NE.'FOR') GO TO 950
         IF(KWS(5).NE.'IN' ) GO TO 950
         IF(ITEMS.GT.6) GO TO 950
C
C  FIND THE ATTRIBUTE IN THE SPECIFIED RELATION.
C
         CALL LXSREC(6,RNAME,ZC)
         CALL LXSREC(4,ANAME,ZC)
         IF(LOCREL(RNAME).NE.0) THEN
            CALL WARN(1,RNAME)
            GO TO 999
         ENDIF
C
C  CHECK FOR MODIFY PERMISSION.
C
         IF(LOCPRM(RNAME,2).NE.0) THEN
            CALL WARN(9,RNAME)
            GO TO 999
         ENDIF
C
C  FIND THE ATTRIBUTE IN THE RELATION.
C
         IF(LOCATT(ANAME,RNAME).NE.0) THEN
            CALL WARN(3,ANAME,RNAME)
            GO TO 999
         ENDIF
C
C  DON'T DO IF ATTRIBUTE IS ALREADY A KEY.
C
         CALL ATTGET(ISTAT)
         IF(ATTKEY.NE.0) THEN
            CALL MSG(' ','ATTRIBUTE ','+')
            CALL AMSG(ANAME,-ZC,'+')
            CALL MSG(' ',' IS ALREADY A KEY.',' ')
            GO TO 999
         ENDIF
C
C  DON'T DO IF REAL OR DOUBLE
C
         CALL TYPER(ATTYPE,SVM,TYP)
         IF(TYP.EQ.KZREAL .OR. TYP.EQ.KZDOUB) THEN
            CALL MSG(' ','REAL OR DOUBLE COLUMNS MAY NOT BE KEYED.',' ')
            GO TO 999
         ENDIF
C
C  DETERMINE THE COLUMN TO BE USED FOR THIS ATTRIBUTE.
C
         COLUMN = ATTCOL
C
C  INITIALIZE THE BTREE FOR THIS ELEMENT.
C
         CALL BTINIT(ATTKEY)
         START = ATTKEY
         CALL ATTPUT(ISTAT)
C
C  SORT THE KEY VALUES IF THERE ARE MORE THAN 100 OF THEM
C
         IF(NTUPLE.GT.100) GO TO 700
C
C   SCAN THROUGH ALL THE DATA FOR THIS RELATION.
C
  500    IF(NID.EQ.0) GO TO 900
         CID = NID
         CALL GETDAT(1,NID,ITUP,LENGTH)
         IF(NID.LT.0) GO TO 900
         IP = ITUP + COLUMN - 1
C     CHECK FOR A VARIABLE LENGTH ATTRIBUTE.
         IF(ATTWDS.EQ.0) IP = BUFFER(IP) + ITUP + 1
         IF(BUFFER(IP).EQ.NULL) GO TO 500
         CALL BTADD(BUFFER(IP),CID,ATTYPE)
         GO TO 500
C
C  SORT KEY VALUES BEFORE BUILDING THE B-TREE
C
  700    LENGTH = 2
         NSOVAR = 1
         NKSORT = 3
         LIMTU = ALL9S
         SORTYP(1) = .TRUE.
         VARPOS(1) = 1
         L = 2
         IF(ATTYPE.EQ.KZTEXT) L = 4
         IF(ATTYPE.EQ.KZINT ) L = 1
         IF(ATTYPE.EQ.KZIVEC) L = 1
         IF(ATTYPE.EQ.KZIMAT) L = 1
         VARTYP(1) = L
         VARLEN(1) = 1
         OFFSET = 0
         CALL SORT(NKSORT)
C
C  READ THE SORTED KEY VALUES AND BUILD THE BTREE
C
         CALL GTSORT(IP,1,-1,LENGTH)
C
  800    CALL GTSORT(IP,1,1,LENGTH)
         IF(RMSTAT.NE.0) GO TO 900
         IF(BUFFER(IP).EQ.NULL) GO TO 800
         CALL BTADD(BUFFER(IP),BUFFER(IP+1),ATTYPE)
         GO TO 800
C
C  ALL DONE.
C
C  RESTORE THE START TO THE BTREE TABLE.
C
  900    I = LOCATT(ANAME,RNAME)
         CALL ATTGET(ISTAT)
         ATTKEY = START
         CALL ATTPUT(ISTAT)
         CALL MSG(' ','BUILD KEY COMPLETED.',' ')
         RMSTAT = 0
         GO TO 999
C
C  SYNTAX ERROR.
C
  950    CALL WARN(4)
C
C  RETURN
C
  999    RETURN 1
      END
