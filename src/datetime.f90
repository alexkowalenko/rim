MODULE DateTime
   implicit none
   private

   public DateTime_Initialise
   public RMTIME
   public RMDATE
   public JULDAT
   public DATJUL
   public ASCDAT
   public DTFENC
   public DTFSPL

   INTEGER, public :: ASMTXT(12)
   !
   !     ASMTXT  -- ASCII-TEXT OF MONTH NAMES (3-CHARS)
   !
   INTEGER, public :: KRMDTF, KRMTMF
   !         KRMDTF--DEFAULT DATE FORMAT
   !         KRMDTF--DEFAULT TIME FORMAT

contains

   SUBROUTINE DateTime_Initialise()
      USE Parameters
      USE Text, only : ASCTXT
      CHARACTER(len=3) :: MONTHS(12) = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']
      INTEGER :: I

      INCLUDE 'rmatts.inc'

      DO I = 1, 12
         ASMTXT(I) = 0
         CALL ASCTXT(ASMTXT(I),3,MONTHS(I))
      END DO

      CALL DTFENC(KZDATE, KRMDTF, 'DD/MM/YYYY')
      CALL DTFENC(KZTIME, KRMTMF, 'HH:MM:SS')
   END SUBROUTINE DateTime_Initialise


   INTEGER FUNCTION RMTIME()
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      !  PURPOSE:   RETURN THE CURRENT TIME AS INTEGER (SEC FROM MIDNIGHT)
      !
      !  PARAMETERS:
      !     TIM-----THE CURRENT TIME
      !
      integer :: t(8)

      ! Fortran 90 Standard
      CALL date_and_time(VALUES=t)
      RMTIME = t(5)*3600 + t(6)*60 + t(7)
      RETURN
   END FUNCTION RMTIME


   INTEGER FUNCTION RMDATE()
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      !  PURPOSE:   RETURN THE CURRENT DATE AS INTEGER
      !
      !  PARAMETERS:
      !     DAT-----THE CURRENT DATE
      !
      integer :: d(8)
      LOGICAL X

      ! Fortran 90 Standard
      CALL date_and_time(VALUES=d)
      X = JULDAT(d(3),d(2),d(1),RMDATE)
      RETURN
   END FUNCTION RMDATE


   LOGICAL FUNCTION JULDAT(DD,MM,YY,JUL)
      INTEGER, intent(in) :: DD, MM, YY
      INTEGER, intent(out) :: JUL
      !
      ! CONVERT D,M,Y TO JULIAN
      ! RETURN <TRUE> IF D,M,Y ARE VALID
      !
      ! FROM ACM-CA 199, BY R. TANTZEN
      !

      INTEGER :: D, M, Y, C, YA
      D = DD
      M = MM
      Y = YY
      IF (M.GT.2) THEN
         M = M - 3
      ELSE
         M = M + 9
         Y = Y - 1
      ENDIF
      C = Y/100
      YA = Y - 100*C
      JUL = (146097*C)/4 + (1461*YA)/4 + &
         (153*M + 2)/5 + D + 1721119

      ! TEST ARGS FOR VALIDITY

      CALL DATJUL(D,M,Y,JUL)
      IF (D.EQ.DD .AND. M.EQ.MM .AND. Y.EQ.YY) THEN
         JULDAT = .TRUE.
      ELSE
         JULDAT = .FALSE.
      ENDIF
      RETURN
   END FUNCTION JULDAT


   SUBROUTINE DATJUL(DD,MM,YY,JUL)
      INTEGER, intent(out) :: DD, MM, YY
      INTEGER, intent(in) :: JUL
      !
      ! CONVERT JULIAN TO D,M,Y
      !
      ! FROM ACM-CA 199, BY R. TANTZEN
      !

      INTEGER J, Y, M, D

      J = JUL

      J = J - 1721119

      Y = (4*J-1)/146097
      J = 4*J-1 - 146097*Y
      D = J/4

      J = (4*D+3)/1461
      D = 4*D+3 - 1461*J
      D = (D+4)/4

      M = (5*D-3)/153
      D = 5*D-3 - 153*M
      D = (D+5)/5

      Y = 100*Y + J

      IF (M.LT.10) THEN
         M = M + 3
      ELSE
         M = M - 9
         Y = Y + 1
      ENDIF

      DD = D
      MM = M
      YY = Y
      RETURN
   END SUBROUTINE DATJUL

   SUBROUTINE ASCDAT(STR,SC,L,JDAT,DFMT,TYP)

      USE Parameters
      USE Globals, only : KMSSVL, KMSSVT, KNAPVT, KNAPVL
      USE Text, only : FILCH, ABLANK, ITOA, STRMOV

      !
      ! CONVERT A DATE INTEGER TO ASCII-TEXT
      !
      !  PARAMETERS
      !
      !     STR ---DESTINATION STRING (ASCII-TEXT)
      !     SC ----START POS IN STR
      !     L -----LENGTH OF STR
      !     JDAT---JULIAN DATE INTEGER
      !     DFMT----DATE-FORMAT INTEGER
      !     TYP----TYPE (KZDATE / KZTIME)
      !
      INTEGER, intent(out) :: STR(1)
      INTEGER, intent(in) :: SC, L, JDAT, DFMT, TYP
      !
      INCLUDE 'rmatts.inc'
      !
      INTEGER, PARAMETER :: U0=48,U9=57,PLUS=43,MINUS=45,DECIM=46,COLON=58
      INTEGER :: S, FMT, ASC, DP, ML, MP, YL, YP, DD, MM , YY, ERR
      INTEGER :: SP, HP, XX, HH, SS
      !
      S = SC - 1
      IF (TYP.EQ.KZTIME) GOTO 500
      !
      ! IS A DATE
      !
      FMT = DFMT
      IF (FMT.EQ.0) FMT = KRMDTF
      CALL DTFSPL(L,DP,MP,ML,YP,YL,ASC,FMT)
      CALL FILCH(STR,SC,L,ABLANK)
      !
      IF (JDAT.EQ.ZIMISS) GOTO 700
      IF (JDAT.EQ.ZINAPP) GOTO 710
      !
      CALL DATJUL(DD,MM,YY,JDAT)
      CALL FILCH(STR,SC,L,ASC)
      !
      IF (DP.NE.0) THEN
         CALL ITOA(STR,S+DP,2,DD,ERR)
         IF (DD.LT.10) CALL PUTT(STR,S+DP,U0)
      ENDIF
      !
      IF (ML.EQ.3) THEN
         CALL STRMOV(ASMTXT(MM),1,3,STR,S+MP)
      ELSE
         CALL ITOA(STR,S+MP,2,MM,ERR)
         IF (MM.LT.10) CALL PUTT(STR,S+MP,U0)
      ENDIF
      !
      IF (YL.EQ.2) YY = YY - 1900
      CALL ITOA(STR,S+YP,YL,YY,ERR)
      IF (YY.LT.10) CALL PUTT(STR,S+YP,U0)
      GOTO 800
      !
      ! TIME
      !
500   FMT = DFMT
      IF (FMT.EQ.0) FMT = KRMTMF
      CALL DTFSPL(L,SP,MP,XX,HP,XX,ASC,FMT)
      CALL FILCH(STR,SC,L,ABLANK)
      !
      IF (JDAT.EQ.ZIMISS) GOTO 700
      IF (JDAT.EQ.ZINAPP) GOTO 710
      !
      HH = JDAT / 3600
      MM = MOD(JDAT,3600)/60
      SS = MOD(JDAT,60)
      CALL FILCH(STR,SC,L,ASC)
      !
      IF (SP.NE.0) THEN
         CALL ITOA(STR,S+SP,2,SS,ERR)
         IF (SS.LT.10) CALL PUTT(STR,S+SP,U0)
      ENDIF
      !
      CALL ITOA(STR,S+MP,2,MM,ERR)
      IF (MM.LT.10) CALL PUTT(STR,S+MP,U0)
      !
      CALL ITOA(STR,S+HP,2,HH,ERR)
      IF (HH.LT.10) CALL PUTT(STR,S+HP,U0)
      GOTO 800
      !
      ! MISSING VALUE
      !
700   CALL STRMOV(KMSSVT,1,MIN(KMSSVL,L),STR,SC)
      GOTO 800
      !
      ! NOT APPL
      !
710   CALL STRMOV(KNAPVT,1,MIN(KNAPVL,L),STR,SC)
      !
800   RETURN
   END SUBROUTINE ASCDAT


   SUBROUTINE DTFENC(TYP,DTFINT,DTFCHR)
      USE Parameters
      USE Text, only : ASCCHR
      !
      ! ENCODE A DATE/TIME FORMAT STRING INTO AN INTEGER
      !
      ! MAX STRING LENGTH IS 12 CHARS
      ! DATE DAY FIELD AND TIME SECONDS FIELD ARE OPTIONAL
      !
      INTEGER, intent(in) :: TYP
      INTEGER, intent(out) :: DTFINT
      CHARACTER(len=*), intent(in) :: DTFCHR
      !
      INCLUDE 'rmatts.inc'
      !
      ! OFFSETS FOR DATA PACKING
      !
      INTEGER, PARAMETER :: T1=12, T2=12**2, T3=12**3, T4=12**4, T5=12**5
      INTEGER, PARAMETER :: TA=128
      CHARACTER(len=1) :: SC
      INTEGER :: DP, ML, MP, YL, YP, I, L, SP, HP
      !
      IF (TYP.EQ.KZTIME) GOTO 500
      !
      ! DATE FORMAT
      !
      ! DAY
      DP = INDEX(DTFCHR,'DD')
      !
      ! MONTH
      ML = 3
      MP = INDEX(DTFCHR,'MMM')
      IF (MP.EQ.0) THEN
         ML = 2
         MP = INDEX(DTFCHR,'MM')
      ENDIF
      !
      ! YEAR
      YL = 4
      YP = INDEX(DTFCHR,'YYYY')
      IF (YP.EQ.0) THEN
         YL = 2
         YP = INDEX(DTFCHR,'YY')
      ENDIF
      !
      ! LOOK FOR SEPERATION CHAR
      !
      DO I = 1, LEN(DTFCHR)
         SC = DTFCHR(I:I)
         IF (SC.NE.'Y' .AND. SC.NE.'M' .AND. SC.NE.'D') GOTO 110
      END DO
      SC = ' '
110   CONTINUE
      !
      ! CHECK VALIDITY
      !
      DTFINT = 0
      IF (MP.EQ.0 .OR. YP.EQ.0) RETURN
      L = LEN(DTFCHR)
      DTFINT = (L*T5 + DP*T4 + MP*T3 + ML*T2 + YP*T1 + YL)*TA &
         + ASCCHR(SC)
      GOTO 900
      !
      ! TIME FORMAT
      !
      ! SECOND
500   SP = INDEX(DTFCHR,'SS')
      !
      ! MINUTE
      MP = INDEX(DTFCHR,'MM')
      !
      ! HOUR
      HP = INDEX(DTFCHR,'HH')
      !
      ! LOOK FOR SEPERATION CHAR
      !
      DO I = 1, LEN(DTFCHR)
         SC = DTFCHR(I:I)
         IF (SC.NE.'H' .AND. SC.NE.'M' .AND. SC.NE.'S') GOTO 610
      END DO
      SC = ' '
610   CONTINUE
      !
      ! CHECK VALIDITY
      !
      DTFINT = 0
      IF (MP.EQ.0 .OR. HP.EQ.0) RETURN
      L = LEN(DTFCHR)
      DTFINT = (L*T5 + SP*T4 + MP*T3 + HP*T1)*TA &
         + ASCCHR(SC)
      !
900   RETURN
   END SUBROUTINE DTFENC


   SUBROUTINE DTFSPL(L,DP,MP,ML,YP,YL,SC,DTFINT)
      !
      ! SPLIT A DATE-TIME FORMAT INTEGER INTO COMPONENTS
      !
      !
      ! OFFSETS FOR DATA PACKING
      !

      INTEGER :: L,DP,MP,ML,YP,YL,SC,DTFINT
      INTEGER, PARAMETER :: T1=12, T2=12**2, T3=12**3, T4=12**4, T5=12**5
      INTEGER, PARAMETER :: TA=128

      INTEGER :: I
      !
      ! USE DEFAULT DATE SPECIFICATION IF NONE GIVEN
      !
      I = DTFINT
      IF (I.EQ.0) I = KRMDTF
      !
      I = I/TA
      !
      ! LENGTH
      L = I/T5
      I = I - L*T5
      !
      ! DAY
      DP = I/T4
      I = I - DP*T4
      !
      ! MONTH
      MP = I/T3
      I = I - MP*T3
      ML = I/T2
      I = I - ML*T2
      !
      ! YEAR
      YP = I/T1
      I = I - YP*T1
      YL = I
      !
      ! SEPERATION CHAR
      SC = MOD(DTFINT,TA)
      !
      RETURN
   END SUBROUTINE DTFSPL

END MODULE DateTime
