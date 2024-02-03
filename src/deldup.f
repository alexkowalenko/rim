      SUBROUTINE DELDUP(*)

         USE Globals, only : DFLAG
         USE Lexer, only : ITEMS
         USE Message, only : WARN
         USE Text, only : BLANK

         INCLUDE 'syspar.inc'
C
C     DELETE DUPLICATES ROUTINE
C
C     METHOD -
C              1. SORT TUPLES ALONG ATTRIBUTES OR ALL
C              2. LOOP ON SORTED TUPLES, DELETING SUCCESSIVE
C                      DUPLICATES
C              3. WHEN DONE RESET RSTART AND NTUPLE, PRINT MESSAGE,
C                  AND RETURN
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'f2com.inc'
         INCLUDE 'start.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'whcom.inc'
C
         LOGICAL IFALL
         INCLUDE 'dclar1.inc'
         LOGICAL SELREL
C
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
C     LOCATE WORD 'FROM' OR 'IN'
C
         J = LFIND(1,ITEMS,'FROM')
         IF(J.EQ.0) J = LFIND(1,ITEMS,'IN')
         NJ = 2
         IF (J.EQ.0) NJ = 0
C
C     GET RELATION INFO
C
         IF (.NOT.SELREL(J,NJ)) GOTO 999

         IFALL = .FALSE.
         NKSORT = 1
         MAXTU = ALL9S
         LIMTU = ALL9S
         NBOO = 0
         LIMVAL = 0
         NS = 0
         IF (J.EQ.3)THEN
C
C        SET UP SORT ARRAYS FOR SORTING ON ENTIRE TUPLE
C
            NSOVAR = 1
            VARPOS(1) = 1
            VARLEN(1) = 0
            SORTYP(1) = .TRUE.
            VARTYP(1) = 1
            OFFSET = 0
            IFALL = .TRUE.
            NKSORT = 4
C        AND GO DO THE SORT
            GOTO 250
         ENDIF
C
C     SET UP FOR SPECIFIED ATTRIBUTES
C
         II = ITEMS - 2
         NSOVAR = 0
         OFFSET = 0
         DO 107 I=3,II
            CALL LXSREC(I,ANAME,ZC)
            IF(LOCATT(ANAME,NAME).NE.0)THEN
               CALL WARN(3,ANAME,NAME)
               GO TO 999
            ENDIF
            CALL ATTGET(ISTAT)
C
C         GOT ATTRIBUTE - FILL SORTVAR LIST
C
            NSOVAR = NSOVAR + 1
            IF(NSOVAR.GT.NSORTW) THEN
               CALL MSG('E','YOU HAVE SPECIFIED TOO MANY COLUMNS.',' ')
               NSOVAR = NSORTW
               GO TO 999
            ENDIF
C
C         ADD TO SORT LIST ARRAYS
C
            VARPOS(NSOVAR) = ATTCOL
            VARLEN(NSOVAR) = ATTWDS
            SORTYP(NSOVAR) = .TRUE.
            IF(ATTYPE.EQ.KZINT .OR.ATTYPE.EQ.KZIVEC.OR.ATTYPE.EQ.KZIMAT)
     +        L = 1
            IF(ATTYPE.EQ.KZREAL.OR.ATTYPE.EQ.KZRVEC.OR.ATTYPE.EQ.KZRMAT)
     +        L = 2
            IF(ATTYPE.EQ.KZDOUB.OR.ATTYPE.EQ.KZDVEC.OR.ATTYPE.EQ.KZDMAT)
     +        L = 3
            IF(ATTYPE.EQ.KZTEXT) L = 4
            VARTYP(NSOVAR) = L
  107    CONTINUE
C
C     PERFORM THE SORT
C
  250    CALL SORT(NKSORT)
C
C     COMPARE THE TUPLES
C
C     GET BUFFERS FOR SAVED TUPLE AND READ TUPLE
C
         CALL BLKDEF(6,MAXCOL,1)
         KQ1 = BLKLOC(6)
         CALL BLKDEF(7,MAXCOL,1)
         IP  = BLKLOC(7)
C
C     CHECK FOR ANY KEY ATTRIBUTES
C
         J = LOCATT(BLANK,NAME)
         NUMKEY = 0
C
  252    CALL ATTGET(ISTAT)
         IF(ISTAT.EQ.0) THEN
            IF(ATTKEY.NE.0) NUMKEY=NUMKEY + 1
            GO TO 252
         ENDIF
C
C  RETRIEVE THE SORTED TUPLES
C
C     (OPEN THE SORT FILE)
         LENGTH = NCOL
         NS = 1
         CALL GTSORT(IP,3,-1,LENGTH)
C
C     (READ THE FIRST RECORD)
C     (GTSORT RETURNS 'CID', 'NID' AND 'IVAL'
C              IN 'RIMPTR' COMMON)
C
         CALL GTSORT(BUFFER(IP),3,0,LENGTH)
         ND = 0
C
C  SAVE THE ACTIVE TUPLE IN THE BUFFER
C
  300    CALL BLKMOV(BUFFER(KQ1),BUFFER(IP),LENGTH)
C
C  GET NEXT TUPLE FROM SORT FILE
C
  400    CALL GTSORT(BUFFER(IP),3,0,LENGTH)
         IF(CID.EQ.0) GO TO 600
         IF(RMSTAT.NE.0) GO TO 600
C
C     COMPARE ATTRIBUTES
C
         IF (.NOT.IFALL)THEN
C
C     COMPARE ON SELECTED ATTRIBUTES
C
            DO 490 I = 1,NSOVAR
               L = VARPOS(I) - 1
               IF (VARLEN(I).NE.0)THEN
C
C           FIXED LENGTH ATTR COMPARE
C
                  DO 410 J = 1,VARLEN(I)
                     IF(BUFFER(IP+L).NE.BUFFER(KQ1+L)) GO TO 300
                     L = L + 1
  410             CONTINUE
                  GO TO 490
               ELSE
C
C           VARIABLE LENGTH ATTR COMPARE
C
                  JP1 = BUFFER(IP+L) + IP - 1
                  JP2 = BUFFER(KQ1+L) + KQ1 - 1
                  IF(BUFFER(JP1).NE.BUFFER(JP2)) GO TO 300
                  NW = BUFFER(JP1) + 1
                  DO 460 J = 1,NW
                     IF(BUFFER(JP1+J).NE.BUFFER(JP2+J)) GO TO 300
  460             CONTINUE
               ENDIF
  490       CONTINUE

C        (A DUP -- DELE IT)
            GO TO 550
         ELSE
C
C     COMPARE ALL ATTRIBUTES
C
            DO 520 I = 1,LENGTH
               IF(BUFFER(IP-1+I).NE.BUFFER(KQ1-1+I)) GO TO 300
  520       CONTINUE
         ENDIF
C
C
C  DELETE DUPLICATE RECORD
C
  550    CALL DELDAT (1,CID)
         IF(NUMKEY.EQ.0)GOTO 580
C
C     PROCESS ANY KEY ATTRIBUTES
C
         J = LOCATT(BLANK,NAME)
C
C     FOR EACH ATTRIBUTE
  560    CALL ATTGET(ISTAT)
         IF (ISTAT.EQ.0) THEN
            IF(ATTKEY.EQ.0) GO TO 560
C        PROCESS IF KEY
            COLUMN = ATTCOL
            IF(ATTWDS.EQ.0) COLUMN = BUFFER(IP+ATTCOL-1) + 2
            START = ATTKEY
            CALL BTREP(BUFFER(IP+COLUMN-1),0,CID,ATTYPE)
            GO TO 560
         ENDIF

  580    CONTINUE
         IF (CID .EQ. IID) IID = NID
         ND = ND + 1
         GO TO 400
C
C     UPDATE RELATION INFORMATION
C
  600    CONTINUE
         CALL RELGET(ISTAT)
         NTUPLE = NTUPLE - ND
         CALL RELPUT
C
         CALL MSG(' ',' ','+')
         CALL IMSG(ND,5,'+')
         CALL MSG(' ',' ROWS WERE DELETED.',' ')
         CALL BLKCLR(7)
         CALL BLKCLR(6)
C
  999    RETURN 1
      END
