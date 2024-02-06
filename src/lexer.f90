MODULE Lexer

   USE, intrinsic :: iso_fortran_env

   USE Parameters, only : ZMTOK, ZMASC, ZKEYWL, ZCW, Z, ZC
   implicit none
   private

   INTEGER, PARAMETER, public :: ASPACE=32
   INTEGER, PARAMETER, public :: LA=97,LZ=122,UA=65,UZ=90,USCOR=95
   INTEGER, PARAMETER, public :: UECH=69,LECH=101
   INTEGER, PARAMETER, public :: U0=48,U9=57,PLSIGN=43,MNSIGN=45,DECIM=46
   INTEGER, PARAMETER, public :: TMSIGN=42,DVSIGN=47
   INTEGER, PARAMETER, public :: ATSIGN=64,PCSIGN=37
   INTEGER, PARAMETER, public :: EQSIGN=61,GTSIGN=62,LTSIGN=60,SQUOTE=39
   INTEGER, PARAMETER, public :: DQUOTE=34
   INTEGER, PARAMETER, public :: ASSTAR=42,ASPCNT=37
   INTEGER, PARAMETER, public :: ASCOLN=58
   INTEGER, PARAMETER, public :: ASRCLL=82,ASLXEI=60,ASLXEE=62
   INTEGER, PARAMETER, public :: ASLXEX=33,ASLXES=35

   INTEGER, public :: ASBLK,ASCOM,ASSEMI,ASPLUS,ASQUO
   INTEGER, public  :: ASLPAR, ASRPAR

!  VARIABLE DEFINITIONS
!         ASBLK---FIELD SEPERATOR (MANY = 1)
!         ASCOM---FIELD SEPERATOR (MANY = MANY)
!         ASSEMI--RECORD DELIMITER
!-----C   ASDOL---RECORD DELIMITER (ARCHAIC)
!         ASPLUS--LINE CONTINUATION CHARACTER
!         ASQUO---STRING DELIMITER  (' OR ")
!         ASLPAR--LEFT PARENTHESIS
!         ASRPAR--RIGHT PARENTHESIS
!         ASPCNT--PERCENT SIGN
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


   public Initialise
   public TOKTYP !     TOKTYP CHECKS A TOKEN FOR A TYPE MATCH
   public EQTOK
   public EQKEYW
   public LFIND
   public LXLENW
   public LXSREC

contains

   SUBROUTINE Initialise
      !
      !     INITIALIZE THE LXLCOM VARIABLES
      !
      INTRINSIC ICHAR

      ASBLK  = ICHAR(' ')
      ASCOM  = ICHAR(',')
      ASPLUS = ICHAR('+')
      ASQUO  = SQUOTE
      ASLPAR = ICHAR('(')
      ASRPAR = ICHAR(')')
      ASSEMI = ICHAR(';')
      RETURN
   END


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
      USE Text, only : BLANK, ABLANK, STRMOV
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

END MODULE Lexer
