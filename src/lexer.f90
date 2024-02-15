MODULE Lexer

   USE, intrinsic :: iso_fortran_env

   USE RM_Parameters, only : ZMTOK, ZMASC, ZKEYWL, ZCW, Z, ZC
   implicit none
   private

   INTEGER, PARAMETER, public :: ASRCLL=82,ASLXEI=60,ASLXEE=62
   INTEGER, PARAMETER, public :: ASLXEX=33,ASLXES=35


!
!         ASRCLL--RECALL LINE CHARACTER (R)
!         ASRCLC--RECALL COMMAND CHARACTER (RC)
!         ASLXEI--RECALL EDIT - BEGIN INSERT (>)
!         ASLXEE--RECALL EDIT - END INSERT (<)
!         ASLXEX--RECALL EDIT - END LINE (!)
!         ASLXES--RECALL EDIT - SKIP CHAR (#)


!  *** / T O K E N S / ***
!
!  CONTAINS INPUT RECORD AS TOKENS
!
!     EACH TOKEN MAY BE ONE OR MORE OF THE FOLLOWING
!
!          KXNULL - NULL (TYPE = 0)
!          KXTEXT - TEXT
!          KXINT  - INTEGER
!          KXREAL - REAL
!          KXKEYW - KEYWORD
!          KXNAME - NAME
!
   INTEGER, PARAMETER, public :: KXNULL=0 ! Not used
   INTEGER, PARAMETER, public  :: KXTEXT=1
   INTEGER, PARAMETER, public  :: KXINT=2
   INTEGER, PARAMETER, public  :: KXREAL=4
   INTEGER, PARAMETER, public  :: KXKEYW=8
   INTEGER, PARAMETER, public  :: KXNAME=16

   !
   !  *** / T O K E N S / ***
   !
   !  CONTAINS INPUT RECORD AS TOKENS
   !
   INTEGER, public :: ASCREC(ZMASC)
   INTEGER, public :: ASCNXT

   INTEGER, public :: IDP(ZMTOK)
   INTEGER, public :: IDL(ZMTOK)
   INTEGER, public :: IDT(ZMTOK)
   INTEGER, public :: IDI(ZMTOK)
   REAL(real64), public :: IDR(ZMTOK)

   CHARACTER(len=ZKEYWL), public :: KWS(ZMTOK)
   INTEGER, public :: ITEMS

   !
   !  VARIABLE DEFINITIONS:
   !         ASCREC--ARRAY OF TOKENS AS ASCII-TEXT
   !         IDT-----ARRAY OF TYPE IDENTIFIERS FOR EACH TOKEN
   !         IDP-----ARRAY OF POINTERS TO REC
   !         IDL-----ARRAY OF LENGTHS FOR CHARACTER DATA
   !         IDI-----ARRAY OF TOKENS AS INTEGERS (OR ASCII-CHAR SYMBOLS)
   !         IDR-----ARRAY OF TOKENS AS REALS
   !         ASCNXT--NEXT AVAILABLE SPOT IN ASCREC
   !         ITEMS---NUMBER OF TOKENS
   !         KWS-----ARRAY OF TOKENS AS KEYWORDS (UPPER CASE CHAR)
   !
   !     TOKTYP CHECKS A TOKEN FOR A TYPE MATCH
   ! LOGICAL TOKTYP


   public TOKTYP !     TOKTYP CHECKS A TOKEN FOR A TYPE MATCH
   public EQTOK
   public EQKEYW
   public LFIND
   public LXLENW
   public LXSREC
   public NXTTOK
   public LXCMNT
   public TOKDSP

contains

   LOGICAL FUNCTION TOKTYP(I, TYPE)
      !
      ! THIS FUNCTION CHECKS IF THE I'TH TOKEN IS OF TYPE 'TYPE'
      !
      ! INPUT - I........ITEM NUMBER
      !         TYPE.....TYPE TO MATCH
      ! OUTPUT- TOKTYP ...TRUE IF THE ITEM CAN BE OF TYPE 'TYPE'
      !
      INTEGER, intent(in) :: I, TYPE

      !
      ! FOR SUN FORTRAN I REPLACED IAND WINT AND
      IF (AND(IDT(I),TYPE).NE.0) THEN
         TOKTYP = .TRUE.
      ELSE
         TOKTYP = .FALSE.
      ENDIF
      RETURN
   END FUNCTION TOKTYP


   LOGICAL FUNCTION EQTOK(I, ASCHR)
      !
      ! THIS FUNCTION COMPARES ASCHR WITH ITEM I OF THE
      ! COMMAND TOKEN LIST
      !
      ! INPUT - I........ITEM NUMBER
      !         ASCHR....ASCII-CHAR
      ! OUTPUT- EQTOK ....TRUE. IFF
      !               A. ITEM I IS TEXT OF LENGTH 1
      !               B. ITEM I EQUALS ASCHR
      !
      INTEGER, intent(in) :: I, ASCHR

      INTEGER :: A

      EQTOK = .FALSE.
      IF (IDL(I).NE.1) RETURN
      CALL GETT(ASCREC(IDP(I)),1,A)
      IF (A.NE.ASCHR) RETURN
      EQTOK = .TRUE.
      RETURN
   END FUNCTION EQTOK


   LOGICAL FUNCTION EQKEYW(I, KEYW)
      !
      ! THIS FUNCTION COMPARES KEYW WITH ITEM I OF THE
      ! COMMAND TOKEN LIST
      !
      ! INPUT - I........ITEM NUMBER
      !         KEYW.....STRING WITH KEYWORD IN IT
      ! OUTPUT- EQKEYW....TRUE. IFF
      !                         A. ITEM I IS TEXT
      !                     AND B. NUMBER OF CHARACTERS IN ITEM I
      !                            IS GE MIN(3,LEN) AND LE LEN.
      !                     AND C. ITEM IT MATCHES KEYWORD TO MINIMUM
      !                            OF 8 AND THE NUMBER OF CHARACTERS
      !                            IN ITEM I.
      !
      INTEGER, intent(in) :: I
      CHARACTER(len=*), intent(in) :: KEYW

      INTEGER :: L, N, MIN
      INTRINSIC LEN

      L = LEN(KEYW)
      EQKEYW = .FALSE.
      IF(I.GT.ITEMS) GO TO 1000
      IF(.NOT.TOKTYP(I,KXKEYW)) GO TO 1000
      N = IDL(I)
      MIN = 3
      IF(L.LT.MIN) MIN = L
      IF(N.LT.MIN) N = MIN
      IF(N.GT.L) GO TO 1000
      IF (KWS(I)(1:N).EQ.KEYW(1:N)) EQKEYW = .TRUE.
1000  RETURN
   END FUNCTION EQKEYW


   INTEGER FUNCTION LFIND(ITEM1,NUM,KEY)
      !
      ! THIS ROUTINE LOOKS FOR A KEYWORD IN THE COMMAND TOKEN
      ! RECORD.  IT RETURNS 0 IF NOT FOUND AND THE ITEM
      ! NUMBER IF FOUND.
      !
      ! ITEM1---FIRST TOKEN TO CHECK
      ! NUM-----NUMBER OF TOKENS TO CHECK
      ! KEY ----KEYWORD TO LOOK FOR
      !
      !
      INTEGER, intent(in) :: ITEM1, NUM
      CHARACTER(len=*), intent(in) :: KEY

      INTEGER :: NEND, J

      NEND = ITEM1 + NUM - 1
      IF (NEND.GT.ITEMS) NEND = ITEMS
      DO J=ITEM1,NEND
         IF(EQKEYW(J,KEY)) GO TO 20
      END DO
      J = 0
20    CONTINUE
      LFIND = J
      RETURN
   END FUNCTION LFIND


   INTEGER FUNCTION LXLENW(I)
      !
      !  RETURN THE LENGTH IN WORDS FOR THE ITH ITEM.
      !
      INTEGER, intent(in) :: I

      LXLENW = 1
      IF (TOKTYP(I,KXTEXT)) LXLENW = (IDL(I) - 1) / ZCW + 1
      RETURN
   END FUNCTION LXLENW


   SUBROUTINE LXSREC(I,STRING,NUMC)
      !
      !
      !  MOVE NUMC ASCII-CHARS FROM THE ITH ITEM.                   .
      !
      USE RM_Text, only : BLANK, ABLANK, STRMOV
      USE Utils, only : ZMOVE

      INTEGER, intent(in) :: I, NUMC
      INTEGER :: STRING(Z)

      INTEGER :: J, K, NUM

      !
      !  BLANK FILL FIRST.
      !
      IF (NUMC.EQ.ZC) THEN
         CALL ZMOVE(STRING,BLANK)
      ELSE
         DO J=1,NUMC
            CALL PUTT(STRING,J,ABLANK)
         END DO
      ENDIF
      !
      !  MOVE THE TEXT.
      !
      IF(TOKTYP(I,KXTEXT)) THEN
         K = IDP(I)
         NUM = IDL(I)
         IF(NUM.LE.0) RETURN
         IF(NUMC.LT.NUM) NUM = NUMC
         CALL STRMOV(ASCREC(K),1,NUM,STRING,1)
      ENDIF
      RETURN
   END SUBROUTINE LXSREC


   INTEGER FUNCTION NXTCHR(TXTONL)
      !
      ! --- TEXT PARSING ROUTINE
      !
      ! PARSE THE INPUT LINE RETRIEVING THE NEXT CHARACTER
      !
      ! PARAMETERS
      !          TXTONL--TRUE IF PARSING QUOTED TEXT STRING
      !
      ! CHARACTER IS RETURNED AS THE FUNCTION VALUE (ASCII-CHAR)
      !
      ! IF MACWPT<MACWPZ THEN THE CHARACTER IS TAKEN FROM MACWRK
      !
      USE RM_Parameters
      USE RM_Globals, only : INEOF
      USE Cards, only : READCD, CRDPTR, CRDEND, CRDREC, LXEOC
      USE Extern, only : NXTCRD
      USE Macros, only : MACWRK, MACWPT, MACWPZ
      USE RM_Text, only : ABLANK, ASPLUS, ASSEMI

      LOGICAL, intent(in) :: TXTONL

      INTEGER :: NXT, LASTCH

      !
      ! CHECK FOR ACTIVE MACRO TEXT
      !
      IF (MACWPT.LT.MACWPZ) THEN
         CALL GETT(MACWRK(1),MACWPT,NXT)
         MACWPT = MACWPT + 1
         IF (NXT.EQ.0) GOTO 810
         GOTO 400
      ENDIF
      !
      ! CHECK STATUS OF CARD RECORD
      !
100   IF (READCD.EQ.0) THEN
         CALL NXTCRD(INEOF)
         IF (INEOF.NE.0) GOTO 800
         READCD = 1
      ENDIF
      IF (READCD.EQ.2) GOTO 800
      !
      NXT = 0
      CRDPTR = CRDPTR + 1
      IF (CRDPTR.GT.CRDEND) THEN
         IF (ASPLUS.EQ.NULL) THEN
            READCD = 0
            NXT = ABLANK
            GOTO 600
         ELSE
            GOTO 800
         ENDIF
      ENDIF

      CALL GETT(CRDREC,CRDPTR,NXT)
      !
      ! PLUS AT END OF LINE IS CONTINUATION UNLESS NOT READING
      !
      IF (NXT.EQ.ASPLUS .AND. CRDPTR.EQ.CRDEND) THEN
         IF (READCD.GE.0) THEN
            READCD = 0
            GOTO 100
         ENDIF
         IF (READCD.LT.0) GOTO 800
      ENDIF
      !
      ! SEMICOLON IS LOGICAL END-OF-CARD UNLESS QUOTED
      !
400   IF (.NOT.TXTONL .AND. NXT.EQ.ASSEMI) GOTO 810
      !
      ! RETURN THE CHARACTER
      !
600   NXTCHR = NXT
      LASTCH = NXT
      LXEOC = 0
      GOTO 900
      !
      ! END-OF-CARD REACHED
      !
800   IF (READCD.GT.0) READCD = 0
810   NXTCHR = 0
      LASTCH = 0
      LXEOC = 1
      !
900   CONTINUE
      RETURN
   END FUNCTION NXTCHR


   SUBROUTINE NXTCHX(CH)
      !
      ! --- TEXT PARSING ROUTINE
      !
      ! PUT A CHARACTER AT THE FRONT OF THE INPUT STREAM
      ! (INVERSE OF NXTCHR)
      !
      ! PARAMETERS
      !          CH  =  CHARACTER TO REPLACE
      !
      USE RM_Parameters
      USE Cards, only : CRDPTR, CRDREC
      USE Macros, only : MACWPT, MACWPZ, MACWRK

      INTEGER, intent(in) :: CH

      !
      ! CH MAY BE REPLACED IN THE MACRO BUFFER OR IN THE CARD BUFFER
      !
      IF (MACWPT.LT.MACWPZ) THEN
         MACWPT = MACWPT - 1
         IF (MACWPT.LT.1) GOTO 8000
         CALL PUTT(MACWRK,MACWPT,CH)
      ELSE
         IF (CRDPTR.LT.1) GOTO 8000
         CALL PUTT(CRDREC,CRDPTR,CH)
         CRDPTR = CRDPTR - 1
      ENDIF
      RETURN

8000  PRINT *,'NXTCHX ERROR: ',MACWPT, CRDPTR
      RETURN
   END SUBROUTINE NXTCHX


   SUBROUTINE NXTTOK(EOR)
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
      USE RM_Parameters
      USE Cards, only : LXEOC
      USE RM_Globals, only : KMSSVL, KMSSVT, KNAPVL, KNAPVT
      USE RM_Text, only : CHRASC, ASCAN, UPCASE, BLANK, ABLANK, ATOI, ATOR, SQUOTE, DQUOTE, ASBLK, ASCOM, ASQUO, CHTYPE

      INTEGER, intent(out) :: EOR

      INTEGER :: IDX, LOOKCH, LNB, X, ASCHR
      LOGICAL :: TXTONL, KWSOK, KNMOK
      INTEGER :: NULLBF(Z)

      LOGICAL EQ
!
!     CHECK FOR CARRYOVER E-O-C
!
      IF (LXEOC.NE.0) GOTO 900
!
      TXTONL = .FALSE.
!
!     BEGIN SCAN FOR NEXT TOKEN
!
100   ASCHR = NXTCHR(TXTONL)
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
150      ASCHR = NXTCHR(TXTONL)
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
160      IDX = IDX + 1
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
210   IDX = IDX + 1
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
      IF (ASCHR.NE.ASBLK .AND. ASCHR.NE.ASCOM) CALL NXTCHX(ASCHR)
!
290   IDP(ITEMS) = ASCNXT
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
700   LNB = ASCAN(ASCREC(IDP(ITEMS)),1,0-IDL(ITEMS),ABLANK,.FALSE.)
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
720   IF (ASCHR.EQ.ASBLK) THEN
         ASCHR = NXTCHR(.FALSE.)
         IF (LXEOC.NE.0) GOTO 800
         IF (ASCHR.NE.ASBLK .AND. ASCHR.NE.ASCOM) CALL NXTCHX(ASCHR)
         GOTO 720
      ENDIF
!
!     RETURN WITH GOOD TOKEN
!
800   EOR = 0
      RETURN
!
!     END-OF-CARD REACHED
!
900   EOR = LXEOC
      LXEOC = 0
      RETURN
!
!     OTHER ERRORS
!
970   CALL MSG('E','A QUOTED STRING WAS UNFINISHED.',' ')
      GOTO 999
990   CALL MSG('E','THERE ARE TOO MANY INPUT FIELDS.',' ')
      GOTO 995
991   CALL MSG('E','THERE IS TOO MUCH INPUT TEXT.',' ')
995   X = NXTCHR(TXTONL)
      IF (LXEOC.EQ.0) GOTO 995
!
999   EOR = -1
      RETURN
   END


   SUBROUTINE LXCMNT(EOR)
      USE RM_Parameters, only : NULL
      USE RM_Globals, only: BATCH, INEOF
      USE RM_Text, only: ASBLK, ASCOM, ASPLUS, ASSEMI
      !
      ! REMOVE A COMMENT FROM THE INPUT TOKEN STREAM
      ! THE COMMENT MAY HAVE A SET COMMAND WITHIN
      !
      ! ON ENTRY:   TOKEN(ITEMS-1) = *
      !             TOKEN(ITEMS)   = (
      !
      ! ON EXIT:    ITEMS = ITEMS - 2
      !
      INTEGER, intent(out) :: EOR

      INTEGER :: SAVITM, SAVNXT, SET, ASCHR
      CHARACTER*(ZKEYWL) OPT

      SAVITM = ITEMS - 2
      SAVNXT = ASCNXT - 2
      SET = 1
      !
100   ITEMS = SAVITM
      ASCNXT = SAVNXT
      CALL NXTTOK(EOR)
      !
      ! EOF IS ERROR END OF COMMENT IF BATCH
      !
      IF (INEOF.NE.0) THEN
         IF (.NOT.BATCH) GOTO 100
         EOR = -1
         RETURN
      ENDIF
      !
      ! END OF LINE IS IGNORED
      !
      IF (EOR.NE.0) GOTO 100
      !
      ! ')' IS END OF COMMENT
      !
      IF (KWS(ITEMS).EQ.')') THEN
         ITEMS = SAVITM
         ASCNXT = SAVNXT
         RETURN
      ENDIF
      !
      ! CHECK FOR SET COMMAND IN PROGRESS
      !
      GOTO (310,320,330,340)  SET
      ! ELSE IGNORE THIS FIELD
      GOTO 100
      !
      !   LOOK FOR 'SET'
310   IF (KWS(ITEMS).EQ.'SET') THEN
         SET = 2
      ELSE
         SET = 0
      ENDIF
      GOTO 100
      !
      !   GET OPTION TO SET
320   IF (TOKTYP(ITEMS,KXKEYW)) THEN
         OPT = KWS(ITEMS)
         SET = 3
      ELSE
         SET = 0
      ENDIF
      GOTO 100
      !
      !   LOOK FOR =
330   IF (KWS(ITEMS).EQ.'=') THEN
         SET = 4
      ELSE
         SET = 0
      ENDIF
      GOTO 100
      !
      !   COPY TOKEN AND DO THE SET
340   IF (KWS(ITEMS).EQ.'NULL') THEN
         ASCHR = NULL
      ELSE
         CALL GETT(ASCREC(IDP(ITEMS)),1,ASCHR)
      ENDIF
390   IF (OPT.EQ.'BLANK')  ASBLK  = ASCHR
      IF (OPT.EQ.'COMMA')  ASCOM  = ASCHR
      IF (OPT.EQ.'PLUS')   ASPLUS = ASCHR
      IF (OPT.EQ.'SEMI')   ASSEMI = ASCHR
      ! PROPER VERSIONS
      IF (OPT.EQ.'DEL')  ASCOM  = ASCHR
      IF (OPT.EQ.'CON')  ASPLUS = ASCHR
      IF (OPT.EQ.'END')  ASSEMI = ASCHR
      SET = 0
      GOTO 100
      !
   END SUBROUTINE LXCMNT


   SUBROUTINE TOKDSP
      !!
      !! DISPLAY /TOKENS/ COMMON
      !!
      USE RM_Parameters
      USE Maths, only : IEXP
      USE RM_Text, only : FILCH, ABLANK, RTOA

      INTEGER :: RTXT(ZPRINW), I, RF, N, ERR
      !
      CALL MSG(' ',' /TOKENS/ ',' ')
      DO I = 1, ITEMS
         CALL MSG(' ','ITEM: ','+')
         CALL IMSG(I,3,'+')
         CALL MSG(' ',' TYPE=','+')
         CALL IMSG(IDT(I),4,'+')
         CALL MSG(' ',' LENGTH=','+')
         CALL IMSG(IDL(I),4,'+')
         CALL MSG(' ',' KWS=','+')
         CALL MSG(' U',' :' // KWS(I) // ':',' ')
         CALL MSG(' ',' INT=','+')
         CALL IMSG(IDI(I),10,' ')
         IF (IDR(I).EQ.0) THEN
            RF = 103
         ELSE
            N = IEXP(IDR(I))
            IF (IDR(I).LT.0) N = N + 1
            RF = (IDL(I)-N)*100  + IDL(I)+6
         ENDIF
         CALL MSG(' ',' REAL(','+')
         CALL IMSG(RF,4,'+')
         CALL MSG(' ',')=','+')
         CALL FILCH(RTXT,1,ZPRINL,ABLANK)
         CALL RTOA(RTXT,1, RF, IDR(I), ERR)
         CALL RTOA(RTXT,40,-RF, IDR(I), ERR)
         CALL AMSG(RTXT,-79,' ')
         CALL MSG(' ','[','+')
         CALL AMSG(ASCREC(IDP(I)),IDL(I),'+')
         CALL MSG(' ',']',' ')
      END DO
      RETURN
   END SUBROUTINE TOKDSP

END MODULE Lexer
