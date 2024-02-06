      SUBROUTINE LOADFM(MAT,ATT,FOR,NFOR)

         USE Lexer, only: KXTEXT, KXINT, KXREAL, IDT, ASCREC, IDP, IDL
         USE Lexer, only: KWS, ITEMS, IDI, IDR
         USE TextFiles, only : TIOIN
         USE Text, only : ABLANK, ATOI, ATOR, ASCAN, STRMOV
         USE Utils, only : ZEROIT

         INCLUDE 'syspar.inc'
C
C     LOAD DATA VALUES FROM A FILE USING FORMAT.
C
C  PARAMETERS:
C         MAT-----SCRATCH ARRAY FOR BUILDING TUPLES
C         ATT-----ARRAY OF TUPLEA VALUES
C         FOR-----FORMAT ARRAY
C            1) ATTCOL
C            2) LINE NUMBER
C            3) STARTING COLUMN NUMBER
C            4) FIELD LENGTH PER ITEM (FROM FORMAT SPEC)
C            5) FORMAT
C            6) ITEM POSITION (LOADFM CALCULATES THIS)
C         NFOR----NUMBER OF ITEMS IN FOR
C
         INCLUDE 'cards.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'start.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'dclar1.inc'
C
C  DECLARATION STATEMENTS
C
         INTEGER MAT(MAXCOL), ATT(ZTUPAL,1), FOR(6,1)
         LOGICAL LOADMV
         LOGICAL EQ, NE
C
C     LOOP ON FORMAT
C
   10    CLN=0
         IP=0
         ITEMS=0
C
         DO 200 N = 1, NFOR

            IRR = 0
            IF (FOR(2,N).GT.CLN) THEN
C       SKIP TO CORRECT LINE
               DO 100 II=CLN,FOR(2,N)-1
                  CALL TIOIN(NINT,CRDREC,CRDEND,EOF)
                  IF (EOF.NE.0) GO TO 700
                  CLN=CLN+1
  100          CONTINUE
            ENDIF
            IF (FOR(1,N).EQ.0) GOTO 200
C
C     IT'S AN ATTRIBUTE.  FIND IT IN ATT
C
            DO 120 N2=1,NATT
               IF(ATT(ZA4-1,N2).EQ.FOR(1,N)) THEN
                  CALL BLKMOV(ATTNAM,ATT(1,N2),ZTUPAL)
                  GO TO 400
               ENDIF
  120       CONTINUE
            CALL MSG('E','LOADFM ERROR: ATTRIBUTE NOT FOUND',' ')
            GOTO 9999

C
C  VALUES ARE READ IN ORDER THEY APPEAR IN LAYOUT.
C  STORE ITEM POINTER IN FOR
C
  400       FOR(6,N) = ITEMS + 1
            CCOL=FOR(3,N)
            CLEN=FOR(4,N)
            CALL TYPER(ATTYPE,SVM,TYP)
            NITMS = ATTWDS
            IF (ATTWDS.EQ.0) NITMS = FOR(5,N)/10000

            IF(TYP.EQ.KZINT) THEN
C
C       INTEGER ATTRIBUTE
C
               DO 150 NI=1,NITMS
                  IF (ATTWDS.EQ.0) THEN
                     ITEMS = ITEMS + 1
                     KWS(ITEMS) = '('
                  ENDIF
                  ITEMS=ITEMS+1
                  IP=IP+1
                  IDI(ITEMS)=0
                  IDP(ITEMS)=IP
                  IDT(ITEMS)=KXINT
                  IDL(ITEMS)=1

C       CHECK FOR MISSING VALUES
                  IF (.NOT.LOADMV(CCOL,CLEN)) THEN
                     IF (.NOT. ATOI(CRDREC,CCOL,CLEN,IDI(ITEMS))) THEN
                        CALL MSG('E','EXPECTED INTEGER AT ','+')
                        CALL IMSG(CCOL,6,'+')
                        CALL IMSG(CLEN,6,' ')
                        IRR = 1
                     ENDIF
                  ENDIF
                  CCOL=CCOL+CLEN
  150          CONTINUE
               IF (ATTWDS.EQ.0) THEN
                  ITEMS = ITEMS + 1
                  KWS(ITEMS) = ')'
               ENDIF
C
            ELSE IF(TYP.EQ.KZREAL .OR. TYP.EQ.KZDOUB) THEN
C
C       REAL ATTRIBUTE
C
               DO 160 NI=1,NITMS
                  IF (ATTWDS.EQ.0) THEN
                     ITEMS = ITEMS + 1
                     KWS(ITEMS) = '('
                  ENDIF
                  ITEMS=ITEMS+1
                  IP=IP+1
                  IDI(ITEMS)=0
                  IDP(ITEMS)=IP
                  IDT(ITEMS)=KXREAL
                  IDL(ITEMS)=1

C       CHECK FOR MISSING VALUES
                  IF (.NOT.LOADMV(CCOL,CLEN)) THEN
                     IF (.NOT. ATOR(CRDREC,CCOL,CLEN,IDR(ITEMS))) THEN
                        CALL MSG('E','EXPECTED REAL AT ',' ')
                        CALL IMSG(CCOL,6,'+')
                        CALL IMSG(CLEN,6,' ')
                        IRR = 1
                     ENDIF
                  ENDIF
                  CCOL=CCOL+CLEN
  160          CONTINUE
               IF (ATTWDS.EQ.0) THEN
                  ITEMS = ITEMS + 1
                  KWS(ITEMS) = ')'
               ENDIF
            ELSE
C
C       TEXT, DATE, OR TIME ATTRIBUTE
C
               IP=IP+1
               ITEMS=ITEMS+1
               IDI(ITEMS)=0
               IDP(ITEMS)=IP
               IDT(ITEMS)=KXTEXT

C       CHECK FOR MISSING VALUES
               IF (.NOT.LOADMV(CCOL,CLEN)) THEN
C         STRIP TRAILING BLANKS ON VAR LEN TEXT
                  IF (ATTWDS.EQ.0) THEN
                     CLEN = ASCAN(CRDREC,CCOL,0-CLEN,ABLANK,.FALSE.)
                     IF (CLEN.GT.0) CLEN = CLEN - CCOL + 1
                     IF (CLEN.EQ.0) CLEN = 1
                  ENDIF
                  IDL(ITEMS)=CLEN
                  CALL STRMOV(CRDREC,CCOL,CLEN,ASCREC(IP),1)
                  NWORDS=(CLEN-1)/ZCW + 1
                  IP=IP+NWORDS-1
               ENDIF
            ENDIF
  200    CONTINUE
C
C
C  WE HAVE REACHED EITHER END OF LAYOUT OR END OF DATA FILE
C  IF THE FIRST LINE READ HAD EOF THIS MUST BE AN EMPTY FILE
C
  700    CONTINUE
         IROWS = IROWS + CLN
         IF (IROWS.EQ.0) THEN
            CALL MSG('E','THE DATA FILE IS EMPTY.',' ')
            GOTO 9999
         ENDIF
         IF(IRR.EQ.1) GO TO 1600
         IF(ITEMS.EQ.0) GO TO 1600
C
C  NOW LOAD THE TUPLE WITH EITHER AN ENTERED VALUE OR WITH NULL
C
C
C  ZERO OUT THE TUPLE.
C
         CALL ZEROIT(MAT,MAXCOL)
         NUMKEY = 0
         ENDCOL = NCOL + 1
         IRR=0
         DO 900 I=1,NATT
            CALL BLKMOV(ATTNAM,ATT(1,I),ZTUPAL)
            COLUMN = ATTCOL
            IF (ATTKEY.NE.0) NUMKEY = NUMKEY+1
            DO 720 I2=1,NFOR
               IF (ATTCOL.EQ.FOR(1,I2)) THEN
                  J=FOR(6,I2)
                  ATTFOR = FOR(5,I2)
                  GO TO 750
               ENDIF
  720       CONTINUE
C     ATTRIBUTE HAS NO DATA - PUT NULL INTO MAT ARRAY
            IERR = -1
            J=1
C
C  CALL PARVAL TO PUT THE VALUE INTO MAT ARRAY
C
  750       IF (ATTWDS.NE.0) THEN
               CALL PARVAL(J,MAT(COLUMN),ATTYPE,ATTWDS,ATTCHA,0,IERR)
               IF(IERR.NE.0) IRR=1
            ELSE
               MAT(COLUMN) = ENDCOL
               NCOLT = ENDCOL + 1
               CALL PARVAL(J,MAT(ENDCOL+2),ATTYPE,ATTWDS,ATTCHA,NCOLT,
     +         IERR)
               IF(IERR.NE.0) IRR=1
               MAT(ENDCOL) = ATTWDS
               MAT(ENDCOL+1) = ATTCHA
               ENDCOL = ENDCOL + ATTWDS + 2
            ENDIF
C
  900    IF(IRR.EQ.1) GO TO 1600
         ENDCOL = ENDCOL - 1
         NTUPLE = NTUPLE + 1
         CALL ADDDAT(1,REND,MAT,ENDCOL)
         IF(RSTART.EQ.0) RSTART = REND
C
C  PROCESS ANY KEY ATTRIBUTES.
C
         IF (NUMKEY.EQ.0) GO TO 1600
         CALL PRCKEY('ADD',MAT,ATT)
C
C     IF IT'S NOT END OF DATA FILE DO ANOTHER RECORD
C
 1600    IF (EOF.EQ.0) GO TO 10
         CALL RELPUT

C     AT END CLOSE THE FILE
 9999    CALL SETIN(ZTRMIN)
         RETURN
      END
