      SUBROUTINE LODELE(NUMELE)

         USE Lexer, only : KXINT, KXNAME
         USE Message, only : WARN
         USE Utils, only : HTOI

         INCLUDE 'syspar.inc'
C
C     LOAD THE ELEMENT DATA INTO THE SCRATCH AREA
C
C  PARAMETERS:
C         NUMELE--NUMBER OF NEWLY DEFINED ATTRIBUTES
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'files.inc'
         INCLUDE 'ascpar.inc'
C
C     TEMPORARY ATTRIBUTE STORAGE
         INCLUDE 'tmpa.inc'
C
         INCLUDE 'dclar1.inc'
         LOGICAL EQKEYW
C
C  READ AN ELEMENT RECORD.
C
  100    CALL LODREC
         IF(ITEMS.EQ.1) GOTO 900
C
C  EDIT ELEMENT INPUT.
C
  200    IATTV = 0
         IF(EQKEYW(2,'REAL')) IATTV = KZREAL
         IF(EQKEYW(2,'TEXT')) IATTV = KZTEXT
         IF(EQKEYW(2,'DATE')) IATTV = KZDATE
         IF(EQKEYW(2,'TIME')) IATTV = KZTIME
         IF(EQKEYW(2,'INTEGER')) IATTV = KZINT
         IF(EQKEYW(2,'DOUBLE')) IATTV = KZDOUB
         IF(EQKEYW(2,'RVEC')) IATTV = KZRVEC
         IF(EQKEYW(2,'IVEC')) IATTV = KZIVEC
         IF(EQKEYW(2,'DVEC')) IATTV = KZDVEC
         IF(EQKEYW(2,'RMAT')) IATTV = KZRMAT
         IF(EQKEYW(2,'IMAT')) IATTV = KZIMAT
         IF(EQKEYW(2,'DMAT')) IATTV = KZDMAT
         IF(IATTV.EQ.0) THEN
            CALL MSG('E','UNKNOWN DATA TYPE SPECIFIED.',' ')
            GO TO 800
         ENDIF
C
C  MAKE SURE THAT THE ATTRIBUTE NAME IS VALID
C
         IF(.NOT.TOKTYP(1,KXNAME)) THEN
            CALL LXSREC(1,ANAME,ZC)
            CALL WARN(7,ANAME)
            GO TO 800
         ENDIF
C
C     SCAN LINE
C
         LENGTH = 1
         IF(EQKEYW(2,'TEXT')) LENGTH = ZC
         ROWS = 1
         COLUMN = 1
         FMT = 0
         KEY = 0
         LTEMS = ITEMS
         KEYPOS = LFIND(3,ITEMS-2,'KEY')
         FMTPOS = LFIND(3,ITEMS-2,'FORMAT')
         IF (KEYPOS.NE.0) LTEMS = KEYPOS - 1
         IF (FMTPOS.NE.0) LTEMS = MIN(LTEMS,FMTPOS-1)
C
         IF(LTEMS.EQ.2) GO TO 700
         IF(LTEMS.EQ.3) GO TO 500
         IF(LTEMS.EQ.4) GO TO 600
         CALL WARN(4)
         GO TO 800
C
C  ITEMS = 3.
C
C500   IF((IDI(3).GT.0).AND.(IDI(3).LE.9999)) THEN
  500    IF(IDI(3).GT.0) THEN
            LENGTH = IDI(3)
            ROWS = LENGTH
            GO TO 700
         ENDIF
         IF(EQKEYW(3,'VAR')) THEN
            LENGTH = 0
            ROWS = 0
            COLUMN = 0
            GO TO 700
         ENDIF
         CALL MSG('E','LENGTH MUST BE A POSITIVE INTEGER.',' ')
         GOTO 800
C
C
C  ITEMS = 4.
C
  600    IF((TOKTYP(3,KXINT)).AND.(IDI(3).GT.0)) THEN
            LENGTH = IDI(3)
            ROWS = LENGTH
            IF((TOKTYP(4,KXINT)).AND.(IDI(4).GT.0)) THEN
               COLUMN = IDI(4)
               GOTO 670
            ENDIF
            IF(EQKEYW(4,'VAR')) THEN
               COLUMN = 0
               GOTO 670
            ENDIF
         ENDIF
         IF(EQKEYW(3,'VAR')) THEN
            LENGTH = 0
            ROWS = 0
            IF(EQKEYW(4,'VAR')) THEN
               COLUMN = 0
               GOTO 670
            ENDIF
         ENDIF
         CALL MSG('E','LENGTH MUST BE A POSITIVE INTEGER.',' ')
         GO TO 800
C
C
  670    IF(.NOT. (EQKEYW(2,'RMAT') .OR.
     X      EQKEYW(2,'IMAT') .OR.
     X      EQKEYW(2,'DMAT')))THEN
            CALL MSG('E',
     X       'YOU ENTERED ROWS AND COLUMNS BUT TYPE NOT MATRIX.',' ')
            GO TO 800
         ENDIF
C
C     SET KEY AND FORMAT
C
  700    IF (KEYPOS.NE.0) KEY = 1
         IF (FMTPOS.NE.0) THEN
            CALL TYPER(IATTV,VTYP,STYP)
            CALL LXFMT(FMTPOS+1,STYP,FMT,FMTLEN)
            IF (FMT.EQ.0) THEN
               GOTO 800
            ENDIF
         ENDIF
C
C  STORE THE ELEMENT IN JUNK.
C
         NUMELE = NUMELE + 1
         CALL BLKCHG(10,TMPAL,NUMELE)
         KQ1 = BLKLOC(10)
         KQ1 = KQ1 + (TMPAL*(NUMELE-1))
         CALL LXSREC(1,BUFFER(KQ1),ZC)
         BUFFER(KQ1+TMPA2-1) = IATTV
         IF(EQKEYW(2,'DOUBLE')) LENGTH = LENGTH * 2
         BUFFER(KQ1+TMPA3-1) = LENGTH
         BUFFER(KQ1+TMPA4-1) = KEY
         BUFFER(KQ1+TMPA5-1) = FMT
C
C  GET MORE DATA.
C
         IF(IATTV.EQ.KZTEXT) THEN
C       SPECIAL PACKING FOR TEXT ATTRIBUTES.
            NWORDS = ((LENGTH - 1) / ZCW) + 1
            IF(LENGTH.EQ.0) NWORDS = 0
            CALL HTOI(LENGTH,NWORDS,BUFFER(KQ1+TMPA3-1))
            GO TO 100
         ENDIF
C
         IF(IATTV.EQ.KZDATE) GO TO 100
         IF(IATTV.EQ.KZINT ) GO TO 100
         IF(IATTV.EQ.KZREAL) GO TO 100
         IF(IATTV.EQ.KZDOUB) GO TO 100
C
C  PROCESS VECTOR AND MATRIX ITEMS.
C
         IF(IATTV.EQ.KZDVEC) COLUMN = 2
         IF(IATTV.EQ.KZDMAT) COLUMN = COLUMN * 2
         CALL HTOI(ROWS,ROWS*COLUMN,BUFFER(KQ1+TMPA3-1))
         GO TO 100
C
C
  800    CALL MSG(' ','STILL DEFINING COLUMNS',' ')
         GOTO 100
C
C  DONE.
C
  900    CONTINUE
C
CC    IF (DBGATT) THEN
C        CALL MSG(' ','INTERNAL ATTRIBUTE TABLE',' ')
C        DO 950 I = BLKLOC(10),BLKLOC(10)+(TMPAL*(NUMELE-1)),TMPAL
C950     CALL BLKDSP('ATTRIBUTE',BUFFER(I),'ZIIII')
CCC   ENDFI
C
         RETURN
      END
