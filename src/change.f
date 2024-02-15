      SUBROUTINE CHANGE(MAT,NVAL,IFLAG,NTUP)

         USE RM_Globals, only : RMSTAT
         USE Extern, only: IMSG, AMSG
         USE Formater, only : TYPER
         USE Lexer, only : KWS, IDI
         USE Message, only : WARN
         USE RM_Text, only : BLANK
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE PROCESSES A CHANGE IN RIM.
C
C  RM_Parameters:
C         MAT-----SCRATCH ARRAY FOR A TUPLE
C         NVAL----SCRATCH ARRAY FOR A TUPLE
C        IFLAG---OUTPUT 1 : NON EXPRESSION CHANGE
C                       0 : EXPRESSION CHANGE
C         NTUP----SCRATCH ARRAY FOR A TUPLE
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'start.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
C
         CHARACTER*1 OP
         DIMENSION MAT(1)
         DIMENSION NVAL(2), NTUP(1)
         LOGICAL NE, EQ
         LOGICAL SINGLE
         INCLUDE 'dclar1.inc'
C
C     DEFINITION OF SOME LOCAL VARIABLES :
C         PN----POSITION OF 'IN' AMONG THE COMMAND STATEMENT
C         PT----POSITION OF 'TO' AMONG THE COMMAND STATEMENT
C
C     PARSING DATA FOR CHANGE COMMAND
C
         PARAMETER (QKEYL=3)
         CHARACTER*(ZKEYWL) QKEYS(QKEYL)
         INTEGER QPTRS(2,QKEYL)
C
C
C
C     *********** S T A R T
C
C
         QKEYS(1) = 'TO'
         QKEYS(2) = 'IN'
         QKEYS(3) = 'WHERE'
C
C  PARSE THE COMMAND
C
         SC = PARSE(QKEYS,QKEYL,QPTRS)
         PT = QPTRS(1,1)
         PN = QPTRS(1,2)
         J = QPTRS(1,3)
C
         NC = 0
         IVAL = 0
         NOPE = 0
         CODE = 0
C
C     CHANGE
C
  100    NEWL = ATTWDS
         NROW = ATTCHA
C
C     SINGLE INDICATES VEC(I) MAT(I,J) SPECIFICATION
C
         SINGLE = KWS(3).EQ.'('
         IF(SINGLE) THEN
C
C     CHECK SINGLE SYNTAX
C
            CALL TYPER(ATTYPE,MATV,ITYPE)
            IF(ITYPE.EQ.KZTEXT) GO TO 8150
            NDIM = 1
            IF(MATV.EQ.KZMAT) NDIM = 2
            IF(KWS(4+NDIM).NE.')') GO TO 8150
            IROW = IDI(4)
            ICOL = IDI(5)
            IF(NDIM.EQ.1) ICOL = 1
            NEWL = 1
            IF(ITYPE.EQ.KZDOUB) NEWL = 2
            ID = 6 + NDIM
C
C  CHECK VALUE SYNTAX (ONLY ONE ITEM ALLOWED)
C
C       IF(PN.NE.ID+1) GO TO 8150
            CALL PARVAL(ID,NVAL,ATTYPE,NEWL,NROW,0,IERR)
            IF(IERR.NE.0) GO TO 9900
            IP = 0
            IF(ATTWDS.NE.0) THEN
               IF(NROW.EQ.0) NROW = ATTWDS
               IF(IROW.GT.NROW) GO TO 8150
               IP = NROW*(ICOL-1) + IROW
               IF(ITYPE.EQ.KZDOUB) IP = 2*IP - 1
               IP = IP + ATTCOL - 1
               IF(MATV.EQ.KZMAT .AND. IROW*ICOL.GT.ATTWDS) GO TO 8150
            ENDIF
         ELSE
            CALL PARVAL(PT+1,NVAL,ATTYPE,NEWL,NROW,0,IERR)
            IF(IERR.NE.0) GO TO 9900
         ENDIF
C
C  PROCESS THE WHERE CLAUSE.
C
         NBOO = 0
         LIMTU = ALL9S
         CALL ZMOVE(ANAME,ATTNAM)
         CALL SELWHR(J,QPTRS(2,3))
         IF(RMSTAT.NE.0) GO TO 9900
C
C  RESTORE THE TUPLEA POINTERS.
C
         J = LOCATT(ANAME,NAME)
         CALL ATTGET(ISTAT)
         FIXCOL = ATTCOL
C
C  SEQUENCE THROUGH THE DATA.
C
  500    CALL RMLOOK(MAT,1,0,LENGTH)
         IF(RMSTAT.NE.0) GO TO 9900
         IF(IVAL.GT.NTUPLE) GO TO 9900
         START = ATTKEY
         COLUMN = ATTCOL
C
C  CHANGE IT.
C
         IF (SINGLE) THEN
C
C        CHANGE A SINGLE ITEM OF A MULTI-ITEM ATTRIBUTE
C
            IVOLD = MAT(ATTCOL)
            IF (ATTWDS.EQ.0) THEN
C           USE VAR POINTERS
               IP = MAT(ATTCOL)
               NW = MAT(IP)
               NR = MAT(IP+1)
               COLUMN = IP + 2
               IVOLD = MAT(COLUMN)
               IF(NR.EQ.0) NR = NW
               INW = IROW*ICOL
               IF(ITYPE.EQ.KZDOUB) INW = INW * 2
               IF(IROW.GT.NR .AND. INW.GT.NW) THEN
C             OUT OF RANGE
                  NOPE = NOPE + 1
                  GO TO 500
               ENDIF
               IJ = NR*(ICOL-1) + IROW
               IF(ITYPE.EQ.KZDOUB) IJ = 2*IJ - 1
               IP = IP + IJ + 1
            ENDIF
C
            NEWVAL = 1
            IF(MAT(IP).EQ.NVAL(1)) NEWVAL = 0
            MAT(IP) = NVAL(1)
            IF(ITYPE.EQ.KZDOUB) MAT(IP+1) = NVAL(2)
            IF(IROW.NE.1) NEWVAL = 0
            IF(ICOL.NE.1) NEWVAL = 0


         ELSEIF (ATTWDS.NE.0) THEN
C
C        CHANGE A FIXED LENGTH ATTRIBUTE.
C
            NEWVAL = 1
            IF(MAT(COLUMN).EQ.NVAL(1)) NEWVAL = 0
            IVOLD = MAT(COLUMN)
            K = COLUMN - 1
            DO 610 L=1,ATTWDS
               MAT(K+L) = NVAL(L)
  610       CONTINUE
C

         ELSE
C
C        CHANGE IS TO A VARIABLE LENGTH ATTRIBUTE.
C
            NEWVAL = 1
C
C        FIND THE ACTUAL COLUMN FOR VARIABLE LENGTH STUFF.
C
            COLUMN = MAT(ATTCOL)
            KURLEN = MAT(COLUMN)
            IF(KURLEN.GE.NEWL) THEN
               COLUMN = COLUMN + 2
               IF(MAT(COLUMN).EQ.NVAL(1)) NEWVAL = 0
               IVOLD = MAT(COLUMN)
               K = COLUMN - 1
               DO 620 L=1,NEWL
                  MAT(K+L) = NVAL(L)
  620          CONTINUE
C          RESET THE VARIABLE LENGTH STUFF
               MAT(COLUMN-2) = NEWL
               MAT(COLUMN-1) = NROW
            ELSE
C
C          CHANGE IS TO A VARIABLE LENGTH ATTRIBUTE WITH THE NEW VALUE
C          BIGGER THAN THE OLD VALUE
C          (REBUILD IN NTUP AND CHECK NEW TUPLE SIZE)
C
               ENDCOL = NCOL
               J = LOCATT(BLANK,NAME)
               DO 700 I = 1, NATT
                  CALL ATTGET(ISTAT)
                  IF(ISTAT.NE.0) THEN
                     CALL MSG('E','INTERNAL ATTRIBUTE FAILURE',' ')
                     GOTO 500
                  ENDIF
                  IF(ATTWDS.NE.0) THEN
                     CALL BLKMOV(NTUP(ATTCOL),MAT(ATTCOL),ATTWDS)
                  ELSE
                     IF(ATTCOL.EQ.FIXCOL) THEN
                        NTUP(ENDCOL+1) = NEWL
                        NTUP(ENDCOL+2) = NROW
                        IF (ENDCOL+2+NEWL .GT. MAXCOL) GOTO 8100
                        CALL BLKMOV(NTUP(ENDCOL+3),NVAL,NEWL)
                        LEN = NEWL + 2
                     ELSE
                        COL = MAT(ATTCOL)
                        LEN = MAT(COL)+2
                        IF (ENDCOL+2+LEN  .GT. MAXCOL) GOTO 8100
                        CALL BLKMOV(NTUP(ENDCOL+1),MAT(COL),LEN)
                     ENDIF
                     NTUP(ATTCOL) = ENDCOL + 1
                     ENDCOL = ENDCOL + LEN
                  ENDIF
  700          CONTINUE
               IF (ENDCOL.GT.LENGTH) GOTO 3000
               CALL BLKMOV(MAT,NTUP,ENDCOL)
               J = LOCATT(ANAME,NAME)
               CALL ATTGET(ISTAT)
            ENDIF
         ENDIF
C
C     FIX THE KEYS
C
         IF ((START.EQ.0).OR.(NEWVAL.EQ.0)) GO TO 800
         CALL BTREP(IVOLD,0,CID,ATTYPE)
         IF (MAT(COLUMN).EQ.NULL) GO TO 800
         ATTKEY = START
         CALL BTADD(MAT(COLUMN),CID,ATTYPE)
         IF (ATTKEY.EQ.START) GO TO 800
         ATTKEY = START
         CALL ATTPUT(ISTAT)
C
  800    CALL PUTDAT(1,CID,MAT,LENGTH)
         NC = NC + 1
         GO TO 500


C----------------------------------------

C
C     NEW TUPLE IS LONGER THAN OLD
C     DELETE OLD -- ADD NEW
C
 3000    CONTINUE
C
C
C  OLD TUPLE MUST BE DELETED AND THE CHANGED ONE ADDED.
C
         CALL DELDAT(1,CID)
C
C  ADD THE NEW TUPLE.
C
         CALL BLKMOV(MAT,NTUP,ENDCOL)
         CALL ADDDAT(1,REND,NTUP,ENDCOL)
C
C  CHANGE THE POINTERS FOR ANY KEY ATTRIBUTES.
C
         I = LOCATT(BLANK,NAME)
         DO 3400 I=1,NATT
            CALL ATTGET(ISTAT)
            IF(ISTAT.NE.0) GO TO 3400
            IF(ATTKEY.EQ.0) GO TO 3400
            START = ATTKEY
            KSTART = ATTKEY
            COLUMN = ATTCOL
            IF(ATTWDS.EQ.0) COLUMN = NTUP(COLUMN) + 2
            IF(NE(ATTNAM,ANAME)) GO TO 3200
            CALL BTREP(IVOLD,0,CID,ATTYPE)
            GO TO 3400
C
 3200       IF(MAT(COLUMN).NE.NULL) GO TO 3300
            CALL BTREP(NTUP(COLUMN),0,CID,ATTYPE)
            GO TO 3400
C
 3300       CALL BTREP(NTUP(COLUMN),REND,CID,ATTYPE)
            IF(START.EQ.KSTART) GO TO 3400
            ATTKEY = START
            CALL ATTPUT(ISTAT)
 3400    CONTINUE
C
C  UPDATE THE KEY VALUE FOR THE NEW ATTRIBUTE VALUE
C
         I = LOCATT(ANAME,NAME)
         CALL ATTGET(ISTAT)
         START = ATTKEY
         IF(START.EQ.0) GO TO 4000
         IF(NTUP(COLUMN) .EQ.NULL) GO TO 4000
         CALL BTADD(NTUP(COLUMN),REND,ATTYPE)
         IF(ATTKEY.EQ.START) GO TO 4000
         ATTKEY = START
         CALL ATTPUT(ISTAT)
 4000    CONTINUE
         IF(CID.EQ.RSTART) RSTART = NID
C
C     ACTUALLY ADD THE TUPLE
C
         CALL PUTDAT(1,REND,NTUP,ENDCOL)
         NC = NC + 1
         CALL RELPUT
         GO TO 500
C
C
C     ERRORS ----------------------------------
C
C  TUPLE LENGTH EXCCEDS MAXCOL
C
 8100    CALL WARN(15)
         GO TO 9999
C
 8150    CALL MSG('E','A VECTOR OR MATRIX ELEMENT IS SPECIFIED ' //
     1      'INCORRECTLY.',' ')
         GO TO 9999
C
C  DONE
C
 9900    CALL MSG(' ',' ','+')
         CALL IMSG(NC,8,'+')
         CALL MSG(' ',' ROWS CHANGED IN TABLE ''','+')
         CALL AMSG(NAME,-ZC,'+')
         CALL MSG(' ','''.',' ')
         IF(NOPE.EQ.0) RETURN

         CALL MSG('W',' ','+')
         CALL IMSG(NOPE,5,'+')
         CALL MSG(' ',' ROWS HAD INCOMPATIBLE DIMENSIONS',' ')
C
 9999    RETURN
      END
