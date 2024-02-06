      SUBROUTINE UNDATA(ALL,IRCNTR,MODE,LHASH,ATDATA)

         USE Globals, only : USERID, KRMINF, KRMRNF, KMSSVL,
     +   KMSSVT, KNAPVL, KNAPVT
         USE Text, only : BLANK, ABLANK, ASCAN, STRMOV, NONE
         USE DateTime, only : ASCDAT, KRMDTF
         USE Lexer, only : SQUOTE
         USE Rim, only : RMSTAT
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     UNLOAD THE DATA OF A DATABASE.
C
C  INPUTS:
C          ALL---------TRUE IF ALL RELATIONS ARE SPECIFIED.
C          IRCNTR------NUMBER OF RELATIONS IF SPECIFIED (ALL IS FALSE).
C          IDAY--------DAY CODE FOR HASH
C          MODE --------COMMAND SPECIFIED.
C          LHASH--------LOGICAL SWITCH FOR HASH
C          NAMDB--------NAMDB FOR DEFINE.
C
         CHARACTER*(*) MODE
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'msgcom.inc'
         INCLUDE 'dclar1.inc'
         INCLUDE 'dclar3.inc'
C
         INTEGER IREL(Z,1),ATDATA(250,7),STAT
         EQUIVALENCE (BUFFER(1),IREL(1,1))
         LOGICAL ALL,PERM,LHASH
         LOGICAL NE
         INTEGER LINE(ZCARDW)
         INTEGER FMTSTR(3)
C
C     MAX CHARACTERS PER LINE FOR UNLOAD
C
         PARAMETER (UMCPL=80, UMCPLL=UMCPL-20)
C
C
C     OUTPUT THE CURRENT SETTINGS
C
         CALL MSG('R','SET INTEGER FORMAT ''','+')
         CALL FMTDEC(KRMINF,KZINT ,FMTSTR,12)
         CALL AMSG(FMTSTR,-12,'+')
         CALL MSG(' ','''',' ')

         CALL MSG('R','SET REAL FORMAT ''','+')
         CALL FMTDEC(KRMRNF,KZREAL,FMTSTR,12)
         CALL AMSG(FMTSTR,-12,'+')
         CALL MSG(' ','''',' ')

         CALL MSG('R','SET DATE FORMAT ''','+')
         CALL FMTDEC(KRMDTF,KZDATE,FMTSTR,12)
         CALL AMSG(FMTSTR,-12,'+')
         CALL MSG(' ','''',' ')

         CALL MSG('R','SET MV ''','+')
         CALL AMSG(KMSSVT,KMSSVL,'+')
         CALL MSG(' ','''',' ')

         CALL MSG('R','SET NA ''','+')
         CALL AMSG(KNAPVT,KNAPVL,'+')
         CALL MSG(' ','''',' ')

C
         J = LOCREL(BLANK)
         I = 0
         CALL ZMOVE(MPW1,BLANK)
         I = 0
C
C  GET MODIFY PASSWORD
C
   75    IF (ALL) THEN
C
   80       CALL CHKREL (PERM,MODE,ISTAT,USERID)
            IF (ISTAT .NE. 0) GO TO 800
            IF  (.NOT. PERM) GO TO 80
         ELSE
C
C        SUBSET OF THE DATA
C
            I = I + 1
            IF (I .GT. IRCNTR) GO TO 800
            CALL ZMOVE(RNAME,IREL(1,I))
            J = LOCREL(RNAME)
         ENDIF
C
   85    IF (NE(MPW,NONE) .AND. NE(MPW,MPW1)) THEN
            CALL MSG('RU','USER ''','+')
            NUM = 16
CCCCC    IF (LHASH) NUM = 24
CCCCC    IF (LHASH) CALL HASHIN (MPW,IDAY,LINE,8)
            IF (.NOT. LHASH) CALL AMSG(MPW,-ZC,'+')
            CALL MSG(' ','''',' ')
         ENDIF
         CALL ZMOVE(MPW1,MPW)
C
C  WRITE LOAD COMMAND
C
         CALL MSG('R','LOAD ','+')
         CALL AMSG(NAME,ZC,' ')
C
C
C  GET ATTRIBUTE INFO
C
         J = LOCATT(BLANK,NAME)
         IND = 1
         ATTCNT = 0
  160    CALL ATTGET(ISTAT)
         IF (ISTAT .NE. 0) GO TO 250
         ATTCNT = ATTCNT + 1
         ATDATA (ATTCNT,1) = ATTCOL
         ATDATA (ATTCNT,2) = ATTCHA
         ATDATA (ATTCNT,3) = ATTWDS
         CALL TYPER(ATTYPE,ATDATA(ATTCNT,5),ATDATA(ATTCNT,4))
         ATDATA (ATTCNT,6) = ATTFOR
         ATDATA (ATTCNT,7) = ATTKEY
         GO TO 160
C
  250    NEXTID = RSTART
         STAT = 0
C
C  PROCESS THE TUPLES
C
         DO 600 NEXTUP = 1,ALL9S
            KK = 0
            DONE = 0
C
C  GET THE DATA -- NC IS THE NUMBER OF CHARACTERS
C
            IF (NEXTID.LE.0) GOTO 605
            CALL GETDAT(IND,NEXTID,ITUP,LEN)
            IF (NEXTID.LT.0) GOTO 605
C
C  PROCESS THE TUPLE ACCORDING TO THE NUMBER OF ATTRIBUTES
C
            DO 500 LL = 1,ATTCNT
               STEP = 1
               ICOUNT = ATDATA (LL,1)
               IF (LL .EQ. ATTCNT) DONE = 1
               LEN1 = ATDATA (LL,2)
               LEN2 = ATDATA (LL,3)
               ATTSTR = ATDATA (LL,5)
               TUPLE = ITUP + ICOUNT - 1
C
C  CHECK TO SEE IF VARYING LENGTH -- IF SO GET NEW LENGTHS
C
               IF (LEN2 .NE. 0) GO TO 265
C
C  VARYING ATTRIBUTE
C
C  CHECK TO SEE IF VARYING SCALAR--IF SO, CHANGE TO VECTOR
               IF (ATTSTR.EQ.KZSCA) ATTSTR = KZVEC
               TUPLE = BUFFER (TUPLE) + ITUP - 1
               LEN2 = BUFFER (TUPLE)
               LEN1 = BUFFER (TUPLE + 1)
               TUPLE = TUPLE + 2
C
  265          ATTYPE = ATDATA (LL,4)
               ATTFOR = ATDATA (LL,6)
               IF (ATTYPE .EQ. KZDOUB) THEN
                  LEN2 = LEN2/2
                  STEP = 2
               ENDIF
C
C
C     CHECK FOR MISSING VALUES AND ETC.
C
  270          IF(BUFFER(TUPLE).EQ.ZIMISS) THEN
                  CALL MSG('R','''','+')
                  CALL AMSG(KMSSVT,KMSSVL,'+')
                  CALL MSG(' ',''' ','+')
                  IF(DONE.EQ.1) STAT = 1
                  GO TO 500
               ENDIF
               IF(BUFFER(TUPLE).EQ.ZINAPP) THEN
                  CALL MSG('R','''','+')
                  CALL AMSG(KNAPVT,KNAPVL,'+')
                  CALL MSG(' ',''' ','+')
                  IF(DONE.EQ.1) STAT = 1
                  GO TO 500
               ENDIF
C
               IF (ATTYPE.EQ.KZDATE .OR. ATTYPE.EQ.KZTIME) THEN
C
C        DATE/TIME ITEM
C
                  FMT = ATTFOR
                  CALL ASCDAT(LINE,1,L,BUFFER(TUPLE),FMT,ATTYPE)
                  CALL MSG('R','''','+')
                  CALL AMSG(LINE,L,'+')
                  CALL MSG('R',''' ','+')
                  GOTO 500
               ENDIF
C
C
C     IF TEXT ITEM -- LEN1 IS NUMBER OF CHARACTERS
C
  280          IF (ATTYPE .NE. KZTEXT) GO TO 300
C
C  TEXT PROCESSING SECTION
C
               CALL MSG('R','''','+')
               START = 1
               NONBLK = ASCAN(BUFFER(TUPLE),1,-LEN1,ABLANK,.FALSE.)
C
C  CHECK FOR BLANK FIELD
C
               IF (NONBLK .EQ. 0) GOTO 298
CCCC  IF (NONBLK .EQ. 0) NONBLK = 1
C
  290          ICHAR = NONBLK
               QUOTE = ASCAN(BUFFER(TUPLE),START,NONBLK,SQUOTE,.TRUE.)
               IF (QUOTE .NE. 0) ICHAR = (QUOTE - START + 1)
C
C  CHECK TO SEE IF THE TEXT STRING CAN FIT ON THE LINE
C
               IF ((MSGPTR + ICHAR) .GT. UMCPL - 3) ICHAR = UMCPL - MSGPTR - 3
CCCCC IF(ICHAR.EQ.0) ICHAR = 1
C     AMSG WITH NON-ALIGNED CHARS
               CALL STRMOV(BUFFER(TUPLE),START,ICHAR,MSGREC,MSGPTR+1)
               MSGPTR = MSGPTR + ICHAR
C
C  CHECK FOR DOUBLE QUOTE
C
               IF (ICHAR .EQ. (QUOTE - START + 1))
     +             CALL MSG('R','''','+')
C
               START = START + ICHAR
               NONBLK = NONBLK - ICHAR
C
C  CHECK FOR FULL LINE
C
               IF (NONBLK.NE.0) THEN
                  IF (MSGPTR .GE. UMCPL - 3) CALL MSG('R','+',' ')
                  GOTO 290
               ENDIF
C
C  DONE WITH TEXT ITEM -- ADD TRAILING QUOTE
C
  298          CONTINUE
               IF (DONE .EQ. 1) STAT = 1
               CALL MSG('R',''' ','+')
               GO TO 500
C
C
C  PROCESS INTEGERS AND REALS
C
  300          MATLEN = 1
C
C  PROCESS REAL OR INTEGER ATTRIBUTE (MATRIX,VECTOR, OR SCALAR)
C
               IF (ATTSTR .NE. KZMAT) GO TO 315
C
C  MATRIX PROCESSING -- NEED TO SET MATLEN AND CHANGE LEN2
C  TO THE NUMBER OF COLUMNS
C
               MATLEN = LEN1
               IF (LEN1 .NE. 0) LEN2 = LEN2/LEN1
               CALL MSG('R','(','+')
C
  315          DO 350 KK = 1,LEN2
                  IF ((((LEN2 .EQ. 1)
     +                   .AND. (ATTSTR .NE. KZVEC))
     +                   .OR. (KK .GT. 1))
     +                 .AND. (ATTSTR .NE. KZMAT))
     +                GOTO 320
                  CALL MSG('R','(','+')
  320             CONTINUE
                  DO 330 J = 1,MATLEN
C
C  CHECK TO SEE IF LAST DATA IN TUPLE -- IF SO SET STAT TO 1
C
                     IF ((KK .EQ. LEN2) .AND. (J .EQ. MATLEN)
     +                     .AND. (DONE .EQ. 1)) STAT = 1
C     SET TO USE MAX FORMAT
                     FMT = 0
                     CALL SELPUT (BUFFER(TUPLE),ATTYPE,FMT,1,LINE)
                     CALL AMSG(LINE,MOD(FMT,100),'+')
                     IF (STAT.EQ.0) CALL MSG(' ',' ','+')
C
C             MAKE SURE NO DANGLING PARENS WITHOUT PLUS SIGN
C
                     IF ((STAT.EQ.1) .AND. (MSGPTR.GE.UMCPLL) .AND.
     +                ((ATTSTR .EQ. KZVEC) .OR. (ATTSTR .EQ. KZMAT)))
     +                   STAT = 0
                     IF (MSGPTR.GE.UMCPLL) CALL MSG(' ','+',' ')
                     TUPLE = TUPLE + STEP
  330             CONTINUE
C
                  IF (ATTSTR .NE. KZMAT) GO TO 350
                  CALL MSG(' ',') ','+')
  350          CONTINUE

               IF ((ATTSTR .EQ. KZSCA) .AND. (LEN2 .EQ. 1)) GO TO 360
               CALL MSG('R',') ','+')
  360          CONTINUE
  500       IF (DONE.NE.1 .AND. MSGPTR.GE.UMCPLL) CALL MSG(' ','+',' ')
C
            IF (MSGPTR .NE. 1)  CALL MSG('R',' ',' ')
            STAT = 0
  600    CONTINUE
C
C  WRITE END STATEMENT FOR RELATION
C
  605    CALL MSG('R','END',' ')
C
C     BUILD KEYS FOR KEYED ATTRIBUTES
C
         J = LOCATT(BLANK,NAME)
  700    CALL ATTGET(ISTAT)
         IF (ISTAT .NE. 0) GO TO 75
         IF (ATTKEY.EQ.0) GOTO 700
         CALL MSG('R','BUILD KEY FOR ','+')
         CALL AMSG(ATTNAM,-ZC,'+')
         CALL MSG('R',' IN ','+')
         CALL AMSG(RELNAM,-ZC,' ')
         GOTO 700
C
C
  800    RMSTAT = 0
         RETURN
      END
