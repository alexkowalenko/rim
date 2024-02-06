      SUBROUTINE SORT(NKSORT)

         USE Globals, only : PGFLAG, MRINDX
         USE Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  INTERFACE WITH SOCON TO SORT RIM DATA
C
C  PARAMETERS:
C              NKSORT--INDICATOR FOR THE TYPE OF SORT
C                        1=TUPLE SORT (SELECT)
C                        2=ATTRIBUTE SORT (TALLY)
C                        3=ID (POINTER) + ATTRIBUTE SORT (BUILD)
C                        4=DELETE DUPLICATES SORT ALL ATTRS
C
C
C     SORT MAY BE CALLED WITH NSOVAR=0 TO COPY THE SELECTED
C     TUPLES TO UNIT=ZNSRT (PASS-THRU SORT)
C
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'incore.inc'
C
         DIMENSION JBUF(ZMSRT)
C===============================
C     ====> S T A R T
C===============================
C
         NKSRT0=NKSORT
C
C  OPEN THE FIRST SORT FILE
C
         OPEN(ZNSRT,STATUS='SCRATCH',ACCESS='SEQUENTIAL',
     X     FORM='UNFORMATTED')
C
C  SET UP TUPLE LIMITS - SAVE USER SPECIFIED LIMIT
C
         LIMTUS = LIMTU
         LIMTU = ALL9S
C
C  BRANCH DEPENDING ON THE TYPE OF SORT REQUESTED
C
         IF(NKSORT.EQ.2) GO TO 350
         IF(NKSORT.EQ.3) GO TO 370
         IF(NKSORT.EQ.4)THEN
            FIXLT = .FALSE.
            GOTO 110
         ENDIF
C
C  TUPLE SORT - WRITE THE COMPLETE TUPLE ON THE SORT FILE
C
C  CHECK FOR VARIABLE LENGTH TUPLES IN THE RELATION
C
         FIXLT = .TRUE.
         I = LOCATT(BLANK,NAME)
         DO 100 J=1,NATT
            CALL ATTGET(ISTATX)
            IF(ISTATX.NE.0) GO TO 110
            IF(ATTWDS.EQ.0) FIXLT = .FALSE.
  100    CONTINUE
C
C  INITIALIZE THE REMAINING VARIABLES
C
  110    LTUMAX = 0
         LTUMIN = ALL9S
         NSORT = 0
         LTUPLE = 0
         IF (FIXLT) LTUPLE = NCOL + 3
C
C  READ IN THE TUPLES AND WRITE THE SORT FILE
C
  200    CALL RMLOOK(IP,1,1,LEN)
  201    IF(RMSTAT.NE.0) GO TO 400
         NSORT = NSORT + 1
         IP = IP - 1
         IF (FIXLT) THEN
C
C     FIXED LENGTH TUPLES
C
            WRITE(ZNSRT) (BUFFER(IP+K),K=1,LEN),
     X          CID,NID,IVAL
         ELSE
C
C  VARIBLE LENGTH TUPLE
C
C     'IPLUS' IS EXTRA WORDS: THESE ARE CID, NID, AND IVAL
C              (CURRENT ID, NEXT ID, AND ROW NUMBER) FOR EACH
C              ROW, FOR ALL ATTRIBUTE SORTS
C
C     FOR DELETE DUPLICATES SORT ALL ATTRIBUTES, THREE WORDS ARE
C              PREFIXED TO MAKE THE ENTIRE ROW LOOK LIKE A
C              SINGLE VARIABLE LENGTH TEXT ATTRIBUTE -- FIRST
C              WORD IS POINTER TO START OF FIRST "ATTRIBUTE"
C              THIS POINTER IS ALWAYS TWO.  SECOND WORD IS
C              LENGTH OF ATTRIBUTE (EQ LENGTH OF LINE).  THIRD
C              WORD IS ONE, ALWAYS.
C
            IPLUS = 3
            IF(NKSORT.EQ.4)IPLUS = 6
            LTUPLE = LTUPLE + LEN + IPLUS
            IF(LEN + IPLUS .GT. LTUMAX) LTUMAX = LEN + IPLUS
            IF(LEN + IPLUS .LT. LTUMIN) LTUMIN = LEN + IPLUS

            IF (NKSORT.EQ.4) THEN
               WRITE(ZNSRT) LEN+IPLUS, 2,LEN,1, (BUFFER(IP+K),K=1,LEN),
     X            CID,NID,IVAL
            ELSE
               WRITE(ZNSRT) LEN+IPLUS, (BUFFER(IP+K),K=1,LEN),
     X            CID,NID,IVAL
            ENDIF
         ENDIF
         GO TO 200
C
C     ====================================================
C
C  ATTRIBUTE SORT - WRITE ONLY THE REQUESTED ATTRIBUTE(S) ON THE SORT
C     ===========   FILE
C
  350    FIXLT = .TRUE.
         LTUMAX = 0
         LTUMIN = ALL9S
         NSORT = 0
C
         LTUPLE = 0
         DO 355 JATT = 1,NSOVAR
            LTUPLE = LTUPLE + VARLEN(JATT)
            IF (VARLEN(JATT).NE.0) GO TO 355
            FIXLT = .FALSE.
            LTUPLE = LTUPLE + 1
  355    CONTINUE
C
C  READ THE TUPLES AND WRITE THE ATTRIBUTE VALUES ON THE SORT FILE
C
  360    CALL RMLOOK(IP,1,1,LEN)
         IF(RMSTAT.NE.0) GO TO 400
         NSORT = NSORT + 1
C
C  MOVE THE DESIRED ATTRIBUTES(S) INTO A PACKED SORT ARRAY.
C
         LENV = LTUPLE
         JLEN = 1
         DO 365 JATT = 1,NSOVAR
            DO 366 J = 1,VARLEN(JATT)
               JBUF(JLEN+J-1) = BUFFER(IP-1+VARPOS(JATT)+J-1)
  366       CONTINUE
            JBASE = BUFFER(IP-1+VARPOS(JATT))
            JLEN = JLEN + VARLEN(JATT)
            IF (VARLEN(JATT).NE.0) GO TO 365
            JBUF(JLEN) = LENV+1
            LEN = BUFFER(IP-1+JBASE) + 2
            DO 367 J = 1,LEN
               JBUF(LENV+J) = BUFFER(IP-1+JBASE+J-1)
  367       CONTINUE
            LENV = LENV + LEN
            JLEN = JLEN + 1
  365    CONTINUE

         IF (FIXLT) THEN
            WRITE(ZNSRT) (JBUF(K),K=1,LENV)
         ELSE
            WRITE(ZNSRT) LENV, (JBUF(K),K=1,LENV)
         ENDIF
         GO TO 360
C=========================================================
C
C  ID + ATTRIBUTE SORT (BUILD)
C
  370    FIXLT = .TRUE.
         LTUMAX = 0
         LTUMIN = ALL9S
         NSORT = 0
         LTUPLE = 2
C
  380    IF(NID.EQ.0) GO TO 400
         CID = NID
         CALL GETDAT(1,NID,ITUP,LENGT)
         IF(NID.LT.0) GO TO 400
         IP = ITUP + ATTCOL - 1
         IF(ATTWDS.NE.0) GO TO 390
C
C  ATTRIBUTE IS A VARIABLE LENGTH ATTRIBUTE.
C
         IP = BUFFER(IP) + ITUP + 1
  390    CONTINUE
         IF(BUFFER(IP).EQ.NULL) GO TO 380
C
C WRITE THE SORT FILE
C
         NSORT = NSORT + 1
         WRITE(ZNSRT) BUFFER(IP), CID
         GO TO 380
C=========================================================
C
C  CHECK THAT SOME TUPLES WERE WRITTEN ON INFIL
C  RESET THE TUPLE LIMIT
C
  400    LIMTU = LIMTUS
         IF(NSORT.EQ.0 .AND. .NOT.PGFLAG) THEN
            CALL MSG('W',' NO ROWS AVAILABLE ',' ')
            GO TO 999
         ENDIF
C
         REWIND ZNSRT
C
C  CLEAR OUT ANY PAGE DATA LEFT IN //BUFFER AND GET THE FIRST
C  AVAILABLE WORD.
C
         CALL BLKNXT(NXT)
C
C  IF PASS-THRU SORT THEN RETURN NOW
C
         IF (NSOVAR.EQ.0) GOTO 990
C
C  FIXUP THE LENGTHS FOR THE VARIABLE LENGTH STUFF
C
         IF(FIXLT) GO TO 440
         LTUPLE = LTUPLE + NSORT
         LTUMAX = LTUMAX + 1
         LTUMIN = LTUMIN + 1
C
C  CALL SWCON TO DO THE ACTUAL SORT
C
  440    IERR = 0
C
C  IF ATTRIBUTE SORT, REDEFINE VARPOS TO BE IN PACKED ORDER
C
         IF(NKSORT .EQ. 2) THEN
            JLEN = 1
            DO 9443 JATT = 1,NSOVAR
               VARPOS(JATT) = JLEN
               JLEN = JLEN + VARLEN(JATT)
               IF (VARLEN(JATT).EQ.0) JLEN = JLEN+1
 9443       CONTINUE
         ENDIF
         LIM = LIMIT - 64 - (NXT - 1)
C
C  OPEN THE OUTPUT SORT FILE
C
         RMSTAT = 0
         OUTUN = ZNSRT + MRINDX
         IF (OUTUN.NE.ZNSRT)
     1      OPEN(OUTUN,STATUS='SCRATCH',ACCESS='SEQUENTIAL',
     2          FORM='UNFORMATTED')
         CALL BLKDEF(1,LIM,1)
         CALL SWCON(BUFFER(NXT),LIM,ZNSRT,OUTUN)
         CALL BLKCLR(1)
         IF (OUTUN.NE.ZNSRT) CLOSE (UNIT=ZNSRT,STATUS='DELETE')
         IF(RMSTAT.NE.0) THEN
            CALL MSG('E',' SORT I/O ERROR','+')
            CALL IMSG(RMSTAT,5,' ')
            NSORT = 0
            GO TO 999
         ENDIF
C
  990    RMSTAT = 0
C
  999    RETURN
      END
