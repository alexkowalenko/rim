MODULE Text

   USE, intrinsic :: iso_fortran_env

   USE Parameters, only : Z

   implicit none
   private

   public Text_Initialise
   public UPCASE, LOCASE
   public ASCCHR, CHRASC
   public ASCTXT
   public FILCH
   public STRASC
   public ATOI, ITOA
   public ATOR, RTOA
   public RITOA

   INTEGER, public :: ABLANK, BLANK(Z)
   !     ABLANK --- A ASCII-CHAR BLANK
   !     BLANK ---- ASCII-TEXT (Z) BLANKS

CONTAINS

   SUBROUTINE Text_Initialise()
      USE Parameters, only : ZC
      INTEGER :: I

      ABLANK = ASCCHR(' ')
      DO I = 1, ZC
         CALL PUTT(BLANK,I,ABLANK)
      END DO
   END SUBROUTINE Text_Initialise


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
      !
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
      !
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
      INCLUDE 'ascpar.inc'
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
      implicit none
      !
      ! CONVERT ASCII-TEXT TO INTEGER AND RETURN TRUE IF OK
      !
      ! SC -- STARTING CHAR IN ASTR
      ! NC -- NUMBER OF CHARS
      INTEGER, intent(in) :: ASTR(*), SC, NC
      INTEGER, intent(out) :: VAL
      !
      INCLUDE 'lxlcom.inc'

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

      implicit none
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
      INCLUDE 'lxlcom.inc'

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

      USE, intrinsic :: iso_fortran_env

      implicit none
      !
      ! CONVERT ASCII-TEXT TO DP AND RETURN TRUE IF OK
      !
      ! SC -- STARTING CHAR IN ASTR
      ! NC -- NUMBER OF CHARS
      !
      INCLUDE 'lxlcom.inc'

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

      INCLUDE 'lxlcom.inc'

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

      USE Maths, only : IEXP

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
      !
      INCLUDE 'ascpar.inc'
      INCLUDE 'lxlcom.inc'

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
         CALL ROUN(RNUM,D,REAL)
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
         CALL ROUN(RR,D,RR)
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

END MODULE Text
