      SUBROUTINE NXTTOK(EOR)
C
C --- TEXT PARSING ROUTINE
C
C     PARSE THE INPUT LINE RETRIEVING THE NEXT TOKEN.
C
C     PARAMETERS
C              EOR--- =0  INDICATES GOOD TOKEN
C              EOR--- >0  INDICATES END-OF-RECORD
C              EOR--- <0  INDICATES ERROR
C
C     TOKEN IS PUT INTO /TOKENS/ AT ITEMS+1
C
         USE Text, only : UPCASE, BLANK, ABLANK

         INCLUDE 'syspar.inc'

         INTEGER EOR
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'lxlcom.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'cards.inc'
         INCLUDE 'flags.inc'
C
         INTEGER ASCHR
         LOGICAL TXTONL,KWSOK,KNMOK
         LOGICAL CHTYPE, ATOI, ATOR, EQ
         CHARACTER*1 CHRASC
         INTEGER NULLBF(Z)
C
C     CHECK FOR CARRYOVER E-O-C
C
         IF (LXEOC.NE.0) GOTO 900
C
         TXTONL = .FALSE.
C
C     BEGIN SCAN FOR NEXT TOKEN
C
  100    ASCHR = NXTCHR(TXTONL)
         IF (LXEOC.NE.0) GOTO 900
C     IGNORE LEADING BLANKS
         IF (ASCHR.EQ.ASBLK) GOTO 100
C     CHECK THAT THERE IS ROOM IN /TOKENS/
         IF (ITEMS.GE.ZMTOK) GOTO 990
         ITEMS = ITEMS + 1
         IDT(ITEMS) = KXTEXT
         IDI(ITEMS) = 0
         IDR(ITEMS) = 0.0
         KWS(ITEMS) = ' '
C
C     COMMA IS NULL FIELD
         IF (ASCHR.EQ.ASCOM) THEN
            IDT(ITEMS) = KXTEXT + KXINT + KXREAL
            IDP(ITEMS) = ASCNXT
            ASCREC(ASCNXT) = BLANK(1)
            ASCNXT = ASCNXT + 1
            IDL(ITEMS) = 0
            GOTO 700
         ENDIF
C
C     QUOTE STARTS TEXT ONLY STRING
C
         IF (ASCHR.EQ.SQUOTE .OR. ASCHR.EQ.DQUOTE) THEN
            ASQUO = ASCHR
            IDX = 0
            TXTONL = .TRUE.
  150       ASCHR = NXTCHR(TXTONL)
            IF (LXEOC.NE.0) GOTO 970
            IF (ASCHR.EQ.ASQUO) THEN
               LOOKCH = NXTCHR(TXTONL)
               IF (LOOKCH.EQ.ASQUO) THEN
                  GOTO 160
               ELSE
                  CALL NXTCHX(LOOKCH)
               ENDIF
C           DONE WITH QUOTED TEXT
               IDP(ITEMS) = ASCNXT
               ASCNXT = ASCNXT + (IDX+ZCW-1)/ZCW
               IDL(ITEMS) = IDX
               GOTO 700
            ENDIF
C        CONTINUE ACCUMULATING QUOTED TEXT
  160       IDX = IDX + 1
            IF (ASCNXT+(IDX/ZCW).GT.ZMASC) GOTO 991
            CALL PUTT(ASCREC(ASCNXT),IDX,ASCHR)
            GOTO 150
         ENDIF
C
C     ELSE THIS IS AN UNQUOTED FIELD
C
         IDX = 0
         TXTONL = .FALSE.
         KWSOK = .TRUE.
         KNMOK = .TRUE.
  210    IDX = IDX + 1
         IF (ASCNXT+(IDX*ZCW).GT.ZMASC) GOTO 991
         CALL PUTT(ASCREC(ASCNXT),IDX,ASCHR)
C
         IF (ASCHR.EQ.ABLANK) THEN
            KNMOK = .FALSE.
            KWSOK = .FALSE.
         ENDIF
         IF (IDX.GT.ZKEYWL) KWSOK = .FALSE.
         IF (IDX.GT.ZC) KNMOK = .FALSE.
         IF (KWSOK) KWS(ITEMS)(IDX:IDX) = CHRASC(UPCASE(ASCHR))
C
C     NOTE THAT DELIMITERS ARE SINGLE CHARACTERS ONLY
         IF (CHTYPE('DELIMIT',ASCHR)) GOTO 290
         ASCHR = NXTCHR(TXTONL)
         IF (LXEOC.NE.0) GOTO 290
         IF (.NOT.CHTYPE('DELIMIT',ASCHR)) GOTO 210
C     DONE WITH TEXT PART
  280    IF (ASCHR.NE.ASBLK .AND. ASCHR.NE.ASCOM) CALL NXTCHX(ASCHR)
C
  290    IDP(ITEMS) = ASCNXT
         ASCNXT = ASCNXT + (IDX+ZCW-1)/ZCW
         IDL(ITEMS) = IDX
         IF (KNMOK) IDT(ITEMS) = IDT(ITEMS) + KXNAME
         IF (KWSOK) THEN
            IDT(ITEMS) = IDT(ITEMS) + KXKEYW
            IF (ATOI(ASCREC(IDP(ITEMS)),1,IDL(ITEMS),IDI(ITEMS)))
     1         IDT(ITEMS) = IDT(ITEMS) + KXINT
         ELSE
            KWS(ITEMS) = ' '
         ENDIF
         IF (ATOR(ASCREC(IDP(ITEMS)),1,IDL(ITEMS),IDR(ITEMS)))
     1      IDT(ITEMS) = IDT(ITEMS) + KXREAL
C
C     TOKEN IS ASSEMBLED
C
C     -- CHECK FOR NULL VALUE FLAGS
C
  700    LNB = ASCAN(ASCREC(IDP(ITEMS)),1,0-IDL(ITEMS),ABLANK,.FALSE.)
         IF (LNB.GT.ZC) GOTO 720
         CALL LXSREC(ITEMS,NULLBF,ZC)
         IF (LNB.EQ.0 .OR. LNB.EQ.KMSSVL) THEN
            IF (EQ(NULLBF,KMSSVT)) THEN
               IDI(ITEMS) = ZIMISS
               IDT(ITEMS) = KXTEXT + KXINT + KXREAL
            ENDIF
         ENDIF
         IF (LNB.EQ.0 .OR. LNB.EQ.KNAPVL) THEN
            IF (EQ(NULLBF,KNAPVT)) THEN
               IDI(ITEMS) = ZINAPP
               IDT(ITEMS) = KXTEXT + KXINT + KXREAL
            ENDIF
         ENDIF
C
C     -- REMOVE TRAILING BLANKS
C
  720    IF (ASCHR.EQ.ASBLK) THEN
            ASCHR = NXTCHR(.FALSE.)
            IF (LXEOC.NE.0) GOTO 800
            IF (ASCHR.NE.ASBLK .AND. ASCHR.NE.ASCOM) CALL NXTCHX(ASCHR)
            GOTO 720
         ENDIF
C
C     RETURN WITH GOOD TOKEN
C
  800    EOR = 0
         RETURN
C
C     END-OF-CARD REACHED
C
  900    EOR = LXEOC
         LXEOC = 0
         RETURN
C
C     OTHER ERRORS
C
  970    CALL MSG('E','A QUOTED STRING WAS UNFINISHED.',' ')
         GOTO 999
  990    CALL MSG('E','THERE ARE TOO MANY INPUT FIELDS.',' ')
         GOTO 995
  991    CALL MSG('E','THERE IS TOO MUCH INPUT TEXT.',' ')
  995    X = NXTCHR(TXTONL)
         IF (LXEOC.EQ.0) GOTO 995
C
  999    EOR = -1
         RETURN
      END
