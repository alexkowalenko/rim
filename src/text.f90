MODULE Text
   !! Text string processing routines

   USE, intrinsic :: iso_fortran_env

   USE Parameters, only : Z

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

   public Initialise
   public UPCASE, LOCASE
   public ASCCHR, CHRASC
   public ASCTXT
   public FILCH
   public STRASC
   public CHTYPE
   public ATOI, ITOA
   public ATOR, RTOA
   public RITOA
   public ASCAN
   public STRMOV
   public LSTRNG
   public LKSTR

   INTEGER, public :: ABLANK, BLANK(Z)
   !     ABLANK --- A ASCII-CHAR BLANK
   !     BLANK ---- ASCII-TEXT (Z) BLANKS
   INTEGER, public :: NONE(Z)
   !     NONE ----- PASSWORD FOR 'NO PASSWORD'

CONTAINS

   SUBROUTINE Initialise()
      USE Parameters, only : ZC
      INTEGER :: I
      INTRINSIC ICHAR

      ASBLK  = ICHAR(' ')
      ASCOM  = ICHAR(',')
      ASPLUS = ICHAR('+')
      ASQUO  = SQUOTE
      ASLPAR = ICHAR('(')
      ASRPAR = ICHAR(')')
      ASSEMI = ICHAR(';')

      ABLANK = ASCCHR(' ')
      DO I = 1, ZC
         CALL PUTT(BLANK,I,ABLANK)
      END DO
      CALL ASCTXT(NONE,ZC,' ')
   END SUBROUTINE Initialise


   INTEGER FUNCTION UPCASE(ASCHR)
      !
      ! RETURN THE UPPER CASE EQUIVALENT OF ASCHR (ASCII-CHAR)
      !
      INTEGER, intent(in) :: ASCHR

      INTEGER, PARAMETER :: LA=97,LZ=122,OFF=97-65
      !
      !
      IF (ASCHR.LT.LA .OR. ASCHR.GT.LZ) THEN
         UPCASE = ASCHR
      ELSE
         UPCASE = ASCHR - OFF
      ENDIF
      RETURN
   END FUNCTION UPCASE


   INTEGER FUNCTION LOCASE(ASCHR)
      !
      ! RETURN THE LOWER CASE EQUIVALENT OF ASCHR (ASCII-CHAR)
      !
      INTEGER, intent(in) :: ASCHR
      INTEGER, PARAMETER :: UA=65,UZ=90,OFF=97-65
      !
      !
      IF (ASCHR.LT.UA .OR. ASCHR.GT.UZ) THEN
         LOCASE = ASCHR
      ELSE
         LOCASE = ASCHR + OFF
      ENDIF
      RETURN
   END FUNCTION LOCASE


   INTEGER FUNCTION ASCCHR(CH)
      !
      ! ** UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
      !
      ! RETURN THE ASCII-CHAR EQUIVALENT OF CH
      !
      CHARACTER(len=1), intent(in) :: CH
      INTRINSIC ICHAR

      ASCCHR = ICHAR(CH)
      ! Convert tab (9) to space (32)
      IF (ASCCHR.EQ.9) ASCCHR = 32
      RETURN
   END FUNCTION ASCCHR


   FUNCTION CHRASC(ASCHR)
      !
      !     **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
      !
      !     RETURN THE CHARACTER EQUIVALENT OF ASCHR (ASCII-CHAR)
      !
      CHARACTER(len=1) CHRASC
      INTEGER ASCHR
      INTRINSIC CHAR

      CHRASC = CHAR(ASCHR)
      RETURN
   END


   SUBROUTINE ASCTXT(ATXT,LTXT,CSTR)
      USE Parameters, only : Z
      !
      ! RETURN THE ASCII-TEXT EQUIVALENT OF CSTR
      !
      !    ATXT -- PLACE TO PUT ASCII-TEXT
      !    LXTX -- LENGTH OF ATXT
      !    CSTR -- CHARACTER STRING
      !
      INTEGER, intent(out) :: ATXT(*)
      INTEGER, intent(in) :: LTXT
      CHARACTER(len=*), intent(in) :: CSTR
      !
      INTEGER :: L, I
      !
      L = LEN(CSTR)
      DO I = 1, LTXT
         IF (I.LE.L) THEN
            CALL PUTT(ATXT,I,ASCCHR(CSTR(I:I)))
         ELSE
            CALL PUTT(ATXT,I,ABLANK)
         ENDIF
      END DO
      RETURN
   END SUBROUTINE ASCTXT


   SUBROUTINE FILCH(ASC,ST,NC,CH)
      INTEGER, intent(out) :: ASC(*)
      INTEGER, intent(in) :: ST, NC, CH
      !
      ! FILL ASC (ASCII-TEXT) WITH CH (ASCII-CHAR)
      ! ST IS START IN ASC, NC IS NUMBER OF CHARS
      !
      INTEGER :: I

      DO I = ST,ST+NC-1
         CALL PUTT(ASC,I,CH)
      END DO
      RETURN
   END SUBROUTINE FILCH


   LOGICAL FUNCTION CHTYPE(TYPE,ASCHR)
      !
      ! RETURN TRUE IF ASCHR IS OF TYPE 'TYPE'
      ! TYPE MAY BE
      !
      !     'LETTER' - (UPPER OR LOWER CASE, UNDERSCORE)
      !     'DIGIT'  - (0-9)
      !     'XDIGIT' - (0-9, PLUS, MINUS, DECIMAL, 'E')
      !     'DELIMIT'- (DELIMITERS)
      !
      CHARACTER(len=*), intent(in) :: TYPE
      INTEGER, intent(in) :: ASCHR
      !
      !
      IF (TYPE.EQ.'LETTER') THEN
         IF ( (ASCHR.GE.LA .AND. ASCHR.LE.LZ) .OR. &
            (ASCHR.GE.UA .AND. ASCHR.LE.UZ) .OR. &
            (ASCHR.EQ.USCOR) ) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
      !
      IF (TYPE.EQ.'DIGIT') THEN
         IF (ASCHR.GE.U0 .AND. ASCHR.LE.U9) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
      !
      IF (TYPE.EQ.'XDIGIT') THEN
         IF ( (ASCHR.GE.U0 .AND. ASCHR.LE.U9) .OR. &
            (ASCHR.EQ.PLSIGN) .OR. (ASCHR.EQ.MNSIGN) .OR. &
            (ASCHR.EQ.UECH) .OR. (ASCHR.EQ.LECH) .OR. &
            (ASCHR.EQ.DECIM) ) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
      !
      ! DELIMITERS ARE: BLANK , ( ) < > = ' " : @ %
      ! NOTE: = - * / ARE NOT DELIMITERS
      !         (- IS ALSO UNIARY, * IS WILD-CARD, / OCCURS OFTEN IN DATES
      !
      IF (TYPE.EQ.'DELIMIT') THEN
         IF ( (ASCHR.EQ.ASBLK)  .OR. (ASCHR.EQ.ASCOM) .OR. &
            (ASCHR.EQ.ASLPAR) .OR. (ASCHR.EQ.ASRPAR).OR. &
            (ASCHR.EQ.SQUOTE) .OR. (ASCHR.EQ.DQUOTE).OR. &
            (ASCHR.EQ.LTSIGN) .OR. (ASCHR.EQ.GTSIGN).OR. &
            (ASCHR.EQ.ATSIGN) .OR. (ASCHR.EQ.PCSIGN).OR. &
            (ASCHR.EQ.EQSIGN) .OR. (ASCHR.EQ.ASCOLN) ) THEN
            CHTYPE = .TRUE.
         ELSE
            CHTYPE = .FALSE.
         ENDIF
         RETURN
      ENDIF
      !
      ! UNRECOGNISED TYPE CODE
      !
      CHTYPE = .FALSE.
      RETURN
   END FUNCTION CHTYPE


   SUBROUTINE STRASC(STR,ASC,NC)
      !
      ! RETURN THE STRING EQUIVALENT OF ASC (ASCII-TEXT, LENGTH NC)

      ! UNIX version does not uppercase the result.  This routine
      ! is used to make filenames and system commands.  These are
      ! uniformly uppercase on many machines, but are mixed case
      ! on UNIX.

      !
      CHARACTER(len=*), intent(out) :: STR
      INTEGER, intent(in) :: ASC(*)
      INTEGER, intent(in) :: NC
      !
      INTEGER :: I, CH

      STR = ' '
      DO I = 1, MIN(NC,LEN(STR))
         CALL GETT(ASC,I,CH)
         STR(I:I) = CHRASC(CH)
      END DO
      RETURN
   END SUBROUTINE STRASC


   LOGICAL FUNCTION ATOI(ASTR,SC,NC,VAL)
      !
      ! CONVERT ASCII-TEXT TO INTEGER AND RETURN TRUE IF OK
      !
      ! SC -- STARTING CHAR IN ASTR
      ! NC -- NUMBER OF CHARS
      INTEGER, intent(in) :: ASTR(*), SC, NC
      INTEGER, intent(out) :: VAL

      INTEGER :: SGN, I, A
      !
      VAL = 0
      SGN = 1
      DO I = 1, NC
         CALL GETT(ASTR,SC+I-1,A)
         IF (A.EQ.ASPACE) THEN
            GOTO 100
         ELSE IF (A.EQ.MNSIGN) THEN
            SGN = -1
         ELSE IF (A.GE.U0 .AND. A.LE.U9) THEN
            VAL = VAL*10 + A - U0
         ELSE
            ATOI = .FALSE.
            VAL = 0
            RETURN
         ENDIF
100      CONTINUE
      END DO
      VAL = VAL * SGN
      ATOI = .TRUE.
      RETURN
   END FUNCTION ATOI


   SUBROUTINE ITOA(STRING,SC,FMT,INT,IERR)
      USE Parameters, only : Z
      USE Utils, only : NDIGIT
      !
      ! THIS ROUTINE CONVERTS AN INTEGER (INT) TO ASCII-TEXT (STRING)
      ! IF THE INTEGER WILL NOT FIT, STRING IS
      ! STARRED AND IERR IS RETURNED NON-ZERO.
      !
      ! STRING....REPOSITORY FOR TEXT OF INT
      ! SC .......STARTING CHARACTER POS
      ! FMT ......FORMAT CODE (TOTAL WIDTH + 100*DECIMAL + 10000*REPEAT)
      ! INT.......INTEGER TO CONVERT.
      ! IERR......0 IF INT FITS, 1 OTHERWISE
      !

      INTEGER, intent(out) :: STRING(*), IERR
      INTEGER, intent(in) :: SC, FMT, INT

      INTEGER :: S, F, N, NC, D, DP, MINL, L

      !
      IERR = 0
      S = SC - 1
      !
      F = MOD(FMT,10000)
      N = ABS(INT)
      NC = MOD(F,100)
      CALL FILCH(STRING,SC,NC,ABLANK)
      D = F / 100
      DP = NC - D
      MINL = NDIGIT(N)
      IF (D.NE.0) MINL = MINL + 1
      IF (INT.LT.0) MINL = MINL + 1
      !
      L = NC
      IF (L.LT.MINL) GOTO 800
100   CALL PUTT(STRING,S+L,MOD(N,10)+U0)
      L = L-1
      N = N/10
      IF (L.EQ.DP) THEN
         CALL PUTT(STRING,S+L,DECIM)
         L = L-1
      ENDIF
      IF (N.GT.0 .OR. DP.LT.L) GOTO 100
      !
      IF (INT.LT.0) CALL PUTT(STRING,S+L,MNSIGN)
      RETURN

      ! NUMBER TOO BIG

800   CALL FILCH(STRING,SC,NC,ASSTAR)
      IERR = 1
      RETURN
   END SUBROUTINE ITOA


   LOGICAL FUNCTION ATOR(ASTR,SC,NC,VAL)
      !
      ! CONVERT ASCII-TEXT TO DP AND RETURN TRUE IF OK
      !
      ! SC -- STARTING CHAR IN ASTR
      ! NC -- NUMBER OF CHARS
      !

      INTEGER, intent(in) :: ASTR(*), SC, NC
      REAL(real64), intent(out) :: VAL

      REAL :: DFAC, EXP
      REAL(real64) :: V
      INTEGER :: A, SVAL, SEXP, IFAC, MODE, I
      !
      ATOR = .FALSE.
      VAL = 0.0
      EXP = 0.0
      SVAL = 1
      SEXP = 1
      IFAC = 10
      DFAC = 1.0
      !
      MODE = 0
      DO I = 1, NC
         CALL GETT(ASTR,SC+I-1,A)
         IF (A.EQ.ASPACE) GOTO 100
         !
         !
         IF (MODE.EQ.0) THEN
            ! VALUE PART OF THE NUMBER
            IF (A.EQ.PLSIGN) THEN
               SVAL = 1
            ELSE IF (A.EQ.MNSIGN) THEN
               SVAL = -1
            ELSE IF (A.EQ.DECIM) THEN
               IFAC = 1
               DFAC = 10.0
            ELSE IF (A.EQ.UECH .OR. A.EQ.LECH) THEN
               MODE = 1
            ELSE IF (A.GE.U0 .AND. A.LE.U9) THEN
               V   = FLOAT(A-U0)
               IF (IFAC.NE.1) THEN
                  VAL = VAL*IFAC
               ELSE
                  V   = V / DFAC
                  DFAC = DFAC * 10.0
               ENDIF
               VAL = VAL + V
            ELSE
               VAL = 0.0
               RETURN
            ENDIF
         ELSE
            ! EXPONENT
            IF (A.EQ.PLSIGN) THEN
               SEXP = 1
            ELSE IF (A.EQ.MNSIGN) THEN
               SEXP = -1
            ELSE IF (A.GE.U0 .AND. A.LE.U9) THEN
               V   = FLOAT(A-U0)
               EXP = EXP*10 + V
            ELSE
               VAL = 0.0
               RETURN
            ENDIF
         ENDIF
100      CONTINUE
      END DO
      VAL = VAL * SVAL
      IF (EXP.NE.0.0) VAL = VAL * (10.0**(EXP*SEXP))
      ATOR = .TRUE.
      RETURN
   END FUNCTION ATOR


   SUBROUTINE RITOA(STRING,SC,LEN,RINT,REM,IERR)
      USE Maths, only : IEXP
      !
      ! CONVERT THE INTEGER PARTOF A DOUBLE (RINT) TO ASCII-TEXT (STRING)
      ! IF IT WILL NOT FIT RETURN IERR > 0
      !
      ! STRING....REPOSITORY FOR TEXT OF INT
      ! SC .......STARTING CHARACTER POS
      ! LEN ......LENGHT OF STRING
      ! RINT......REAL TO CONVERT (MAY BE LARGER THAN MAX INT)
      ! REM ......DECIMAL PART OF RINT
      ! IERR......0 IF RINT FITS, 1 OTHERWISE
      !

      INTEGER, intent(out) :: STRING(*)
      INTEGER, intent(in) :: SC, LEN
      REAL(real64), intent(in) :: RINT
      REAL(real64), intent(out) :: REM
      INTEGER, intent(out) :: IERR

      REAL(real64) :: R
      INTEGER :: I, DG, S, E, IN
      !
      IERR = 0
      R = DABS(RINT)
      DG = IEXP(R)
      IF (DG.GT.LEN) GOTO 800
      S = SC + LEN - DG - 1
      IF (RINT.LT.0) THEN
         IF (S.LT.SC) GOTO 800
         CALL PUTT(STRING,S,MNSIGN)
      ENDIF
      !
      DO I = 1, DG
         E = DG - I
         IN = R / (10.0D0**E)
         IF (IN.GT.9) IN = 9
         CALL PUTT(STRING,S+I,IN+U0)
         R = R - IN*(10.0D0**E)
      END DO
      REM = R
      RETURN

      ! NUMBER TOO BIG
800   IERR = 1
      RETURN
   END SUBROUTINE RITOA


   SUBROUTINE RTOA(STRING,SC,FMT,RNUM,IERR)

      USE Maths, only : IEXP, ROUND

      !
      ! CONVERT A REAL (RNUM) TO ASCII-TEXT (STRING)
      ! IF THE REAL WILL NOT FIT, STRING IS
      ! BLANKED OUT AND IERR IS RETURNED NON-ZERO.
      !
      ! STRING....REPOSITORY FOR TEXT OF RNUM
      ! SC .......STARTING CHARACTER POS
      ! FMT ......FORMAT (LENGTH + 100*DECIMAL PLACES + 10000*REPEAT)
      ! RNUM......VALUE TO CONVERT.
      ! IERR......0 IF VAL FITS, 1 OTHERWISE
      !
      INTEGER, intent(out) :: STRING(*)
      INTEGER, intent(in) :: SC
      INTEGER, intent(in) :: FMT
      REAL(real64), intent(in) :: RNUM
      INTEGER, intent(out) :: IERR

      LOGICAL :: FFMT
      REAL(real64) :: R,RR,REAL,POINT,RREM
      INTEGER :: S, F, L, IL, D, ILDP, IE, ESGN, NUME, FD
      !

      IERR = 0
      S = SC - 1
      F = MOD(FMT,10000)
      FFMT = .TRUE.
      IF (FMT.LT.0) THEN
         F = 0 - F
         FFMT = .FALSE.
      ENDIF
      L = MOD(F,100)
      CALL FILCH(STRING,SC,L,ABLANK)
      D = F/100
      !
      IF (FFMT) THEN
         !
         ! F FORMAT
         !
         CALL ROUND(RNUM,D,REAL)
         R = REAL
         IL = L - D - 1
         CALL RITOA(STRING,SC,IL,R,POINT,IERR)
         IF(IERR.NE.0) GO TO 900
         CALL PUTT(STRING,SC+IL,DECIM)
         IF(D.GT.0) THEN
            POINT = POINT * (10.0D0**D)
            CALL FILCH(STRING,SC+IL+1,D,U0)
            CALL RITOA(STRING,SC+IL+1,D,POINT,RREM,IERR)
            IF(IERR.NE.0) GO TO 900
         ENDIF
      ELSE
         !
         !    E - FORMAT
         !
         !    INCREASE D FOR SINGLE DIGIT LEFT OF DECIMAL
         D = D + 1
         IL = L - D - 5
         IF(IL.LT.0) GO TO 900
         IF(RNUM.LT.0.) THEN
            IF (IL.LT.1) GOTO 900
            CALL PUTT(STRING,S+IL,MNSIGN)
         ENDIF
         ! REMEMBER START OF DIGITS
         ILDP = IL
         IL = IL + 1
         !
         !    FIND THE INTEGER AND THE EXPONENT
         !
         IE = IEXP(RNUM)
         RR = ABS(RNUM)/(10.0D0**IE)
         CALL ROUND(RR,D,RR)
         IE = IE - 1
         ESGN = PLSIGN
         IF (IE.LT.0) THEN
            IE = 0 - IE
            ESGN = MNSIGN
         ENDIF
         !
         NUME = 2
         IF(IE.GE.100) NUME = 3
         R = RR*(10.0D0**D)
         CALL FILCH(STRING,SC+IL,D,U0)
         CALL RITOA(STRING,SC+IL,D,R,RREM,IERR)
         IF(IERR.NE.0) GO TO 900
         IL = IL + D
         IF (NUME.LT.3) THEN
            CALL PUTT(STRING,SC+IL,UECH)
            IL = IL + 1
         ENDIF
         CALL PUTT(STRING,SC+IL,ESGN)
         IL = IL + 1
         CALL ITOA(STRING,SC+IL,NUME,IE,IERR)
         IF(IERR.NE.0) GO TO 900
         IF (IE.LT.10) CALL PUTT(STRING,SC+IL,U0)
         !
         !    REVERSE DECIMAL AND FIRST DIGIT FOR X.XXX FORMAT
         !
         CALL GETT(STRING,SC+ILDP+1,FD)
         CALL PUTT(STRING,SC+ILDP,FD)
         CALL PUTT(STRING,SC+ILDP+1,DECIM)
      ENDIF
      !
      GO TO 9999
      !
      ! ERROR
      !
900   CALL FILCH(STRING,SC,L,ASSTAR)
      IERR = 1
9999  CONTINUE
      RETURN
   END SUBROUTINE RTOA


   INTEGER FUNCTION ASCAN(STRING,SC,NC,ACHR,COMP)
      !
      ! LOOKS FOR ACHR IN STRING FROM SC FOR NC CHARACTERS
      ! STRING IS ASCII-TEXT
      ! NC<0 MEANS SCAN RIGHT TO LEFT
      !
      ! STRING....ASCII-TEXT TO SCAN
      ! SC .......LEFTMOST CHARACTER TO CHECK
      ! NC .......NUMBER OF CHARACTERS
      ! ACHR......ASCII-CHAR TO LOOK FOR
      ! COMP......TRUE TO LOOK FOR CHAR EQUAL
      !           FALSE TO LOOK FOR CHAR NOT EQUAL
      !
      INTEGER, intent(in) :: STRING(*)
      INTEGER, intent(in) :: SC, NC, ACHR
      LOGICAL, intent(in) :: COMP

      INTEGER ST, EN, IC, I, TCHR

      IF (NC.EQ.0) GOTO 110
      IF (NC.GT.0) THEN
         ST = SC
         EN = ST + NC - 1
         IC = 1
      ELSE
         ST = SC - NC - 1
         EN = SC
         IC = -1
      ENDIF
      !
      DO I = ST,EN,IC
         CALL GETT(STRING,I,TCHR)
         ASCAN = I
         IF (COMP .AND. TCHR.EQ.ACHR) GOTO 200
         IF ((.NOT.COMP) .AND. TCHR.NE.ACHR) GOTO 200
      END DO
110   ASCAN = 0
200   RETURN
   END FUNCTION ASCAN


   SUBROUTINE STRMOV(FTXT,FPOS,NUMC,TTXT,TPOS)
      INTEGER, intent(in) :: FTXT(*), FPOS
      INTEGER, intent(out) :: TTXT(*)
      INTEGER, intent(in) :: TPOS, NUMC

      INTEGER :: I, A
      !
      !
      !  MOVE NUMC ASCII-CHARS FROM FTXT(FPOS) -> TTXT(TPOS)               .
      !
      ! IF (FPOS < 0 .OR. NUMC <= 0 .OR. TPOS < 0) THEN
      !    write(*,*) "Return"
      !    RETURN
      ! END IF
      DO I = 1, NUMC
         CALL GETT(FTXT,FPOS+I-1,A)
         CALL PUTT(TTXT,TPOS+I-1,A)
      END DO
      RETURN
   END SUBROUTINE STRMOV


   INTEGER FUNCTION LSTRNG(S1,I1,N1,S2,I2,N2)
      !
      !    SCANS FOR A CHARACTER STRING IN  S1 THAT MATCHES THE
      !    CHARACTER STRING  S2
      ! (BOTH STRINGS ARE ASCII-TEXT)
      !
      !    S1 - STRING TO SEARCH
      !    I1 - FIRST CHAR OF S1 TO CHECK
      !    N1 - NUMBER OF CHARACTERS IN S1 TO CHECK
      !    S2 - STRING TO MATCH
      !    I2 - FIRST CHAR OF S2 TO MATCH
      !    N2 - NUMBER OF CHARACTERS IN S2 TO MATCH
      !
      !    LSTRNG = CHARACTER POS OF STRING MATCH
      !    LSTRNG = 0 IF THE STRING WAS NOT FOUND
      !
      !
      ! THE ARBITRARY CHAR (ARBCHS) MATCHES ANY CHARACTER
      ! CASE IGNORE (CASEIG) MATCHES UPPER AND LOWER CASE
      !
      USE Globals, only : CASEIG, ARBCHS

      INTEGER, intent(in) :: S1(*), S2(*)
      INTEGER, intent(in) :: I1, I2, N1, N2

      INTEGER :: A21, A1, I, J, A2X, A1X

      IF (N1.LT.N2) GOTO 999
      !
      CALL GETT(S2,I2,A21)
      DO I = I1, I1 + N1 - N2
         IF (A21.NE.ARBCHS) THEN
            CALL GETT(S1,I,A1)
            IF (CASEIG) THEN
               A1 = UPCASE(A1)
               A21 = UPCASE(A21)
            ENDIF
            IF (A1.NE.A21) GOTO 200
         ENDIF
         ! FIRST CHAR MATCH FOUND
         DO J = 1, N2 - 1
            CALL GETT(S2,I2+J,A2X)
            IF (A2X.NE.ARBCHS) THEN
               CALL GETT(S1,I+J,A1X)
               IF (CASEIG) THEN
                  A1X = UPCASE(A1X)
                  A2X = UPCASE(A2X)
               ENDIF
               IF (A1X.NE.A2X) GOTO 200
            ENDIF
         END DO
         ! FOUND
         LSTRNG = I
         RETURN
200      CONTINUE
      END DO
      ! NOT FOUND
999   LSTRNG = 0
      RETURN
   END FUNCTION LSTRNG


   LOGICAL FUNCTION LKSTR(DS,DL,MS,ML)
      !
      ! SCANS FOR A CHARACTER STRING IN DS THAT IS 'LIKE' THE
      ! CHARACTER STRING MS. MS MAY CONTAIN 'WILD-CARD' CHARACTERS.
      ! (BOTH STRINGS ARE ASCII-TEXT)
      !
      !    DS - DESTINATION STRING
      !    DL - THE NUMBER OF CHARACTERS IN DS
      !    MS - STRING TO MATCH (CONTAINS WILD-CARD CHARS)
      !    ML - THE NUMBER OF CHARACTERS IN MS
      !
      !    LSTRNG = TRUE IF MATCH FOUND
      !

      USE Globals, only : ARBCHM

      INTEGER, intent(in) :: DS(*), MS(*)
      INTEGER, intent(in) :: DL, ML
      !
      ! WILD-CARD CHARS: ARBCHS - MATCH SINGLE ARBITRARY CHARACTER
      !                  ARBCHM - MATCH MULTIPLE ARBITRARY CHARACTERS
      !
      !
      ! LOOK FOR EACH SUBSTRING OF MS IN DS
      !

      INTEGER :: SMP, SML, SDL, SDP, A

      INTEGER :: DP
      INTEGER :: MP
      !
      DP = 1
      MP = 1
100   SMP = MP + 1
110   MP = MP + 1
      IF (MP.GT.ML) GOTO 200
      CALL GETT(MS,MP,A)
      IF (A.EQ.ARBCHM) GOTO 200
      GOTO 110
      !
      ! SUBSTRING OF MS FOUND (START=SMP, END=MP-1)
      !
200   SML = MP - SMP
      IF (SML.EQ.0) GOTO 300
      !
      ! CHECK FOR STARTING AND ENDING STRINGS (SMP=1 OR MP>ML)
      !
      IF (SMP.EQ.1) THEN
         IF (MP.GT.ML .AND. DL.NE.ML) GOTO 800
         SDL = SML
      ELSE
         IF (MP.GT.ML) DP = DL - SML + 1
         SDL = DL - DP + 1
      ENDIF
      IF (DP.LE.0) GOTO 800
      IF (DP.GT.DL) GOTO 800
      IF (DP+SDL-1.GT.DL) GOTO 800
      !
      ! LOOK FOR THE SUBSTRING
      !
      SDP = LSTRNG(DS,DP,SDL,MS,SMP,SML)
      IF (SDP.LE.0) GOTO 800
      !
      ! SUBSTRING FOUND IN DS
      !
      DP = SDP + SML
300   IF (MP.LE.ML) GOTO 100
      !
      ! MATCH
      !
      LKSTR = .TRUE.
      RETURN
      !
      ! NO MATCH
      !
800   LKSTR = .FALSE.
      RETURN
   END FUNCTION LKSTR

END MODULE Text
