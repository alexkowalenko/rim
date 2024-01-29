MODULE DateTime
   implicit none
   private

   public DateTime_Initialise
   public RMTIME
   public RMDATE
   public JULDAT
   public DATJUL
   public ASCDAT

   INTEGER, public :: ASMTXT(12)
   !
   !     ASMTXT  -- ASCII-TEXT OF MONTH NAMES (3-CHARS)
   !

contains

   SUBROUTINE DateTime_Initialise()
      USE Text, only : ASCTXT
      CHARACTER*3 :: MONTHS(12)
      INTEGER :: I

      MONTHS = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']

      DO I = 1, 12
         ASMTXT(I) = 0
         CALL ASCTXT(ASMTXT(I),3,MONTHS(I))
      END DO
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
      USE Text, only : FILCH, ABLANK

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
      INCLUDE 'ascpar.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'flags.inc'
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


END MODULE DateTime
