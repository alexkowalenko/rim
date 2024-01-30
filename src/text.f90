MODULE Text

   USE Parameters, only : Z

   implicit none
   private

   public Text_Initialise
   public UPCASE, LOCASE
   public ASCCHR
   public ASCTXT
   public FILCH
   public ATOI, ITOA
   public ATOR

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


END MODULE Text
