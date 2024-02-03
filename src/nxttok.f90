      SUBROUTINE NXTTOK(EOR)
         USE Globals, only : KMSSVL, KMSSVT, KNAPVL, KNAPVT
         USE Lexer, only: KXTEXT, KXINT, KXREAL, KXKEYW, KXNAME, IDT, ASCREC, ASCOM, ASQUO, SQUOTE, DQUOTE, ASBLK, ASCNXT, IDP
         USE Lexer, only: IDL, KWS
         USE Text, only : CHRASC, ASCAN, UPCASE, BLANK, ABLANK, ATOI, ATOR
!
! --- TEXT PARSING ROUTINE
!
!     PARSE THE INPUT LINE RETRIEVING THE NEXT TOKEN.
!
!     PARAMETERS
!              EOR--- =0  INDICATES GOOD TOKEN
!              EOR--- >0  INDICATES END-OF-RECORD
!              EOR--- <0  INDICATES ERROR
!
!     TOKEN IS PUT INTO /TOKENS/ AT ITEMS+1
!
         INCLUDE 'syspar.inc'

         INTEGER EOR
!
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'cards.inc'
!
         INTEGER ASCHR
         LOGICAL TXTONL,KWSOK,KNMOK
         LOGICAL CHTYPE, EQ
         INTEGER NULLBF(Z)
!
!     CHECK FOR CARRYOVER E-O-C
!
         IF (LXEOC.NE.0) GOTO 900
!
         TXTONL = .FALSE.
!
!     BEGIN SCAN FOR NEXT TOKEN
!
  100    ASCHR = NXTCHR(TXTONL)
         IF (LXEOC.NE.0) GOTO 900
!     IGNORE LEADING BLANKS
         IF (ASCHR.EQ.ASBLK) GOTO 100
!     CHECK THAT THERE IS ROOM IN /TOKENS/
         IF (ITEMS.GE.ZMTOK) GOTO 990
         ITEMS = ITEMS + 1
         IDT(ITEMS) = KXTEXT
         IDI(ITEMS) = 0
         IDR(ITEMS) = 0.0
         KWS(ITEMS) = ' '
!
!     COMMA IS NULL FIELD
         IF (ASCHR.EQ.ASCOM) THEN
            IDT(ITEMS) = KXTEXT + KXINT + KXREAL
            IDP(ITEMS) = ASCNXT
            ASCREC(ASCNXT) = BLANK(1)
            ASCNXT = ASCNXT + 1
            IDL(ITEMS) = 0
            GOTO 700
         ENDIF
!
!     QUOTE STARTS TEXT ONLY STRING
!
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
!           DONE WITH QUOTED TEXT
               IDP(ITEMS) = ASCNXT
               ASCNXT = ASCNXT + (IDX+ZCW-1)/ZCW
               IDL(ITEMS) = IDX
               GOTO 700
            ENDIF
!        CONTINUE ACCUMULATING QUOTED TEXT
  160       IDX = IDX + 1
            IF (ASCNXT+(IDX/ZCW).GT.ZMASC) GOTO 991
            CALL PUTT(ASCREC(ASCNXT),IDX,ASCHR)
            GOTO 150
         ENDIF
!
!     ELSE THIS IS AN UNQUOTED FIELD
!
         IDX = 0
         TXTONL = .FALSE.
         KWSOK = .TRUE.
         KNMOK = .TRUE.
  210    IDX = IDX + 1
         IF (ASCNXT+(IDX*ZCW).GT.ZMASC) GOTO 991
         CALL PUTT(ASCREC(ASCNXT),IDX,ASCHR)
!
         IF (ASCHR.EQ.ABLANK) THEN
            KNMOK = .FALSE.
            KWSOK = .FALSE.
         ENDIF
         IF (IDX.GT.ZKEYWL) KWSOK = .FALSE.
         IF (IDX.GT.ZC) KNMOK = .FALSE.
         IF (KWSOK) KWS(ITEMS)(IDX:IDX) = CHRASC(UPCASE(ASCHR))
!
!     NOTE THAT DELIMITERS ARE SINGLE CHARACTERS ONLY
         IF (CHTYPE('DELIMIT',ASCHR)) GOTO 290
         ASCHR = NXTCHR(TXTONL)
         IF (LXEOC.NE.0) GOTO 290
         IF (.NOT.CHTYPE('DELIMIT',ASCHR)) GOTO 210
!     DONE WITH TEXT PART
  280    IF (ASCHR.NE.ASBLK .AND. ASCHR.NE.ASCOM) CALL NXTCHX(ASCHR)
!
  290    IDP(ITEMS) = ASCNXT
         ASCNXT = ASCNXT + (IDX+ZCW-1)/ZCW
         IDL(ITEMS) = IDX
         IF (KNMOK) IDT(ITEMS) = IDT(ITEMS) + KXNAME
         IF (KWSOK) THEN
            IDT(ITEMS) = IDT(ITEMS) + KXKEYW
            IF (ATOI(ASCREC(IDP(ITEMS)),1,IDL(ITEMS),IDI(ITEMS))) IDT(ITEMS) = IDT(ITEMS) + KXINT
         ELSE
            KWS(ITEMS) = ' '
         ENDIF
         IF (ATOR(ASCREC(IDP(ITEMS)),1,IDL(ITEMS),IDR(ITEMS))) IDT(ITEMS) = IDT(ITEMS) + KXREAL
!
!     TOKEN IS ASSEMBLED
!
!     -- CHECK FOR NULL VALUE FLAGS
!
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
!
!     -- REMOVE TRAILING BLANKS
!
  720    IF (ASCHR.EQ.ASBLK) THEN
            ASCHR = NXTCHR(.FALSE.)
            IF (LXEOC.NE.0) GOTO 800
            IF (ASCHR.NE.ASBLK .AND. ASCHR.NE.ASCOM) CALL NXTCHX(ASCHR)
            GOTO 720
         ENDIF
!
!     RETURN WITH GOOD TOKEN
!
  800    EOR = 0
         RETURN
!
!     END-OF-CARD REACHED
!
  900    EOR = LXEOC
         LXEOC = 0
         RETURN
!
!     OTHER ERRORS
!
  970    CALL MSG('E','A QUOTED STRING WAS UNFINISHED.',' ')
         GOTO 999
  990    CALL MSG('E','THERE ARE TOO MANY INPUT FIELDS.',' ')
         GOTO 995
  991    CALL MSG('E','THERE IS TOO MUCH INPUT TEXT.',' ')
  995    X = NXTCHR(TXTONL)
         IF (LXEOC.EQ.0) GOTO 995
!
  999    EOR = -1
         RETURN
      END
