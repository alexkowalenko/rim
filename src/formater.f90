MODULE Formater
   !! Formatting values
   implicit none
   private

   public TYPER
   public LXFMT
   public FMTDEC
   public LXDATE

contains

   SUBROUTINE TYPER(ATYPE,VECMAT,TYPE)
      !!
      !! THIS ROUTINE TURNS RIM TYPES SUCH AS IVEC
      !! INTO TWO USEFUL TYPES.
      !!
      !! ATYPE...RIM TYPE (WITH VECTOR AND MATRIX INFO)
      !! VECMAT..KZSCA,KZVEC,KZMAT
      !! TYPE....KZINT,KZREAL,KZDOUB,KZTEXT,KZDATE,...
      !!
      USE RM_Parameters

      INTEGER, intent(in) :: ATYPE
      INTEGER, intent(out) :: VECMAT, TYPE

      INCLUDE 'rmatts.inc'
      !
      TYPE = 0
      !
      ! VECTORS
      !
      VECMAT = KZVEC
      IF(ATYPE.EQ.KZIVEC) TYPE = KZINT
      IF(ATYPE.EQ.KZRVEC) TYPE = KZREAL
      IF(ATYPE.EQ.KZDVEC) TYPE = KZDOUB
      IF (TYPE.NE.0) RETURN
      !
      ! MATRICES
      !
      VECMAT = KZMAT
      IF(ATYPE.EQ.KZIMAT) TYPE = KZINT
      IF(ATYPE.EQ.KZRMAT) TYPE = KZREAL
      IF(ATYPE.EQ.KZDMAT) TYPE = KZDOUB
      IF (TYPE.NE.0) RETURN
      !
      ! ALL ELSE ARE SCALER
      !
      VECMAT = KZSCA
      TYPE = ATYPE
      !
      RETURN
   END SUBROUTINE TYPER


   SUBROUTINE LXFMT(I,TYP,FMT,LEN)
      !!
      !! ENCODE A FORMAT STRING INTO AN INTEGER
      !!
      !! I IS THE LX ITEM NUMBER
      !! TYP IS THE DATA TYPE
      !! FMT IS THE ENCODED FORMAT
      !! LEN IS THE LENGTH OF THE DATA AS FORMATTED
      !!

      USE RM_Parameters
      USE Extern, only: AMSG
      USE Lexer, only: ASCREC, IDP, IDL
      USE DateTime, only: DTFENC
      USE RM_Text, only : UPCASE, ATOI, CHRASC

      INTEGER, intent(in) :: I, TYP
      INTEGER, intent(out) :: FMT, LEN

      INCLUDE 'rmatts.inc'
      !
      INTEGER :: A, D, DP, EP, J, K, L, M, N, NP, STYP, VTYP
      CHARACTER(len=1) :: FC
      CHARACTER(len=12) :: CFMT
      LOGICAL :: T
      !
      FMT = 0
      ! ALLOW FORMAT TO BE TEXT-ONLY STRING
      CFMT = ' '
      L = IDL(I)
      DO J = 1, L
         CALL GETT(ASCREC(IDP(I)),J,A)
         CFMT(J:J) = CHRASC(UPCASE(A))
      END DO
      IF (TYP.EQ.KZDATE .OR. TYP.EQ.KZTIME) THEN
         !
         ! DATE/TIME FORMAT
         !
         CALL DTFENC(TYP,FMT,CFMT(1:L))
         LEN = L
         GOTO 900
      ENDIF
      !
      ! OTHER TYPE (MXNN.DD)
      !
500   DO 510 K = 1, 12
         NP = K
         FC = CFMT(K:K)
510   IF (FC.LT.'0' .OR. FC.GT.'9') GOTO 520
      GOTO 900
520   M = 0
      IF (NP.GT.1) THEN
         T = ATOI(ASCREC(IDP(I)),1,NP-1,M)
         IF (.NOT.T) GOTO 900
      ENDIF
      !
      FC = CFMT(NP:NP)
      IF (FC.EQ.'T') FC = 'A'
      IF (FC.EQ.'R') FC = 'F'
      CALL TYPER(TYP,VTYP,STYP)
      IF (STYP.EQ.KZTEXT .AND. FC.NE.'A') GOTO 900
      IF (STYP.EQ.KZINT  .AND. FC.NE.'I') GOTO 900
      IF ((STYP.EQ.KZREAL .OR. STYP.EQ.KZDOUB) .AND. &
         (FC.NE.'F' .AND. FC.NE.'E')) GOTO 900
      NP = NP + 1
      !
      ! GET NUMBERS
      !
      DP = 0
      EP = 12
      DO J = NP, 12
         EP = J-1
         IF (CFMT(J:J).EQ.' ') GOTO 540
         IF (CFMT(J:J).EQ.'.') DP = J
      END DO
      EP = 12
540   IF (DP.EQ.0) DP = EP+1
      N = 0
      D = 0
      IF (DP.GT.NP) T = ATOI(ASCREC(IDP(I)),NP,DP-NP,N)
      IF (.NOT.T) GOTO 900
      IF (DP.LT.EP) T = ATOI(ASCREC(IDP(I)),DP+1,EP-DP,D)
      IF (.NOT.T) GOTO 900
      LEN = N
      FMT = M*10000 + D*100 + N
      IF (FC.EQ.'E') FMT = 0 - FMT
      !
900   IF (FMT.EQ.0) THEN
         CALL MSG('E',' ''','+')
         CALL AMSG(ASCREC(IDP(I)),IDL(I),'+')
         CALL MSG(' ',''' IS NOT A VALID FORMAT FOR THIS TYPE.',' ')
      ENDIF
      RETURN
   END SUBROUTINE LXFMT


   SUBROUTINE FMTDEC(DTFINT,TYPE,DTFASC,DTFASL)
      !!
      !! DECODE A FORMAT INTEGER INTO A STRING (ASCII-TEXT)
      !!
      !! DTFINT = DATE FORMAT INTEGER
      !! TYPE   = DATA TYPE (KZDATE OR OTHER)
      !! DTFASC = DECODED STRING
      !! DTFASL = LENGTH OF DTFASC
      !!
      USE RM_Parameters
      USE DateTime, only : DTFSPL
      USE RM_Text, only : ABLANK, ITOA, DECIM
      USE Utils, only : NDIGIT

      INTEGER, intent(in) :: DTFINT, TYPE, DTFASL
      INTEGER, intent(out) :: DTFASC(1)
      !
      INCLUDE 'rmatts.inc'
      !
      ! OFFSETS FOR DATA PACKING FOR DATE TYPES
      !
      INTEGER, PARAMETER :: T1=12, T2=12**2, T3=12**3, T4=12**4
      INTEGER, PARAMETER :: TA=128
      !
      ! SOME ASCII-CHARS FOR INT AND REAL FORMATS
      !
      INTEGER, PARAMETER :: DCHR=100,MCHR=109,YCHR=121
      INTEGER, PARAMETER :: SCHR=115,HCHR=104
      INTEGER, PARAMETER :: ICHR=105,ACHR=97,FCHR=102,ECHR=101

      INTEGER :: ASC, CH, DE, DEC, DP, HE, HP, I, LEN, ME, ML, ERR, K, MP, S, SE, SP, T, L, MULT, P, YE, YL, YP, XX

      DO I = 1, DTFASL
         CALL PUTT(DTFASC,I,ABLANK)
      END DO
      IF (DTFINT.EQ.0) RETURN
      !
      IF (TYPE.NE.KZDATE) GOTO 200
      !
      ! DATE FORMAT
      !
      CALL DTFSPL(L,DP,MP,ML,YP,YL,ASC,DTFINT)
      !
      ! BUILD STRING
      !
      DE = DP+1
      ME = MP+ML-1
      YE = YP+YL-1
      IF (L.GT.DTFASL) L = DTFASL
      DO I = 1, L
         CH = ASC
         IF (I.GE.DP .AND. I.LE.DE) CH = DCHR
         IF (I.GE.MP .AND. I.LE.ME) CH = MCHR
         IF (I.GE.YP .AND. I.LE.YE) CH = YCHR
         CALL PUTT(DTFASC,I,CH)
      END DO
      GOTO 900
      !
200   IF (TYPE.NE.KZTIME) GOTO 300
      !
      ! TIME FORMAT
      !
      CALL DTFSPL(L,SP,MP,XX,HP,XX,ASC,DTFINT)
      !
      ! BUILD STRING
      !
      SE = SP+1
      ME = MP+1
      HE = HP+1
      IF (L.GT.DTFASL) L = DTFASL
      DO I = 1, L
         CH = ASC
         IF (I.GE.SP .AND. I.LE.SE) CH = SCHR
         IF (I.GE.MP .AND. I.LE.ME) CH = MCHR
         IF (I.GE.HP .AND. I.LE.HE) CH = HCHR
         CALL PUTT(DTFASC,I,CH)
      END DO
      GOTO 900
      !
      !
      ! INTEGER OR REAL TYPE
      !
300   CALL TYPER(TYPE,S,T)
      IF (T.EQ.KZINT) THEN
         CH = ICHR
      ELSE IF (T.EQ.KZTEXT) THEN
         CH = ACHR
      ELSE
         IF (DTFINT.GT.0) THEN
            CH = FCHR
         ELSE
            CH = ECHR
         ENDIF
      ENDIF
      I = IABS(DTFINT)
      MULT = I/10000
      I = I - MULT*10000
      DEC = I/100
      I = I - DEC*100
      LEN = I
      !
      L = NDIGIT(MULT)
      IF (MULT.EQ.0) THEN
         L = 0
      ELSE
         CALL ITOA(DTFASC,1,L,MULT,ERR)
      ENDIF
      CALL PUTT(DTFASC,1+L,CH)
      P = L + 2
      L = NDIGIT(LEN)
      CALL ITOA(DTFASC,P,L,LEN,ERR)
      IF (DEC.NE.0) THEN
         CALL PUTT(DTFASC,P+L,DECIM)
         P = P + L + 1
         L = NDIGIT(DEC)
         CALL ITOA(DTFASC,P,L,DEC,ERR)
      ENDIF
900   RETURN
   END SUBROUTINE FMTDEC


   LOGICAL FUNCTION LXDATE(I,JDAT,FMT,TYP)
      !!
      !!  RETURN THE DATE FROM THE ITH ITEM.
      !!
      !! JDAT IS JULIAN
      !! FMT IS DATE-FORMAT INTEGER
      !! LXDATE IS TRUE IF ALL OK
      !! TYP IS DATA TYPE
      !!
      USE RM_Parameters
      USE Lexer, only : ASCREC, IDP, IDI
      USE DateTime, only : JULDAT, ASMTXT, DTFSPL, KRMTMF
      USE RM_Text, only : ATOI, STRMOV

      INTEGER, intent(in) :: I, FMT, TYP
      INTEGER, intent(out) :: JDAT

      INCLUDE 'rmatts.inc'

      INTEGER :: AM(1)
      INTEGER :: DD, DP, HH, HP, J, L, ML, MM, MP, SC, SP, XX, YL, YP, YY, SS, TFMT
      !
      LXDATE = .FALSE.
      !
      IF (IDI(I).EQ.ZIMISS .OR. IDI(I).EQ.ZINAPP) THEN
         JDAT = IDI(I)
         LXDATE = .TRUE.
         GOTO 900
      ENDIF

      IF (TYP.EQ.KZTIME) GOTO 500
      !
      ! DATE
      !
      CALL DTFSPL(L,DP,MP,ML,YP,YL,SC,FMT)
      IF (DP.EQ.0) THEN
         DD = 01
      ELSE
         IF (.NOT.ATOI(ASCREC(IDP(I)),DP,2,DD)) RETURN
      ENDIF
      IF (ML.EQ.3) THEN
         AM(1) = 0
         CALL STRMOV(ASCREC(IDP(I)),MP,3,AM,1)
         DO J = 1, 12
            MM = J
            IF (AM(1).EQ.ASMTXT(J)) GOTO 210
         END DO
         RETURN
      ELSE
         IF(.NOT.ATOI(ASCREC(IDP(I)),MP,ML,MM)) RETURN
      ENDIF
      !
210   IF(.NOT.ATOI(ASCREC(IDP(I)),YP,YL,YY)) RETURN
      IF (YL.EQ.2) YY = YY + 1900
      IF (.NOT.JULDAT(DD,MM,YY,JDAT)) RETURN
      LXDATE = .TRUE.
      GOTO 900
      !
      ! TIME
      !
500   TFMT = FMT
      IF (TFMT.EQ.0) TFMT = KRMTMF
      CALL DTFSPL(L,SP,MP,XX,HP,XX,SC,TFMT)
      IF (MP.EQ.0) RETURN
      !
      IF (SP.EQ.0) THEN
         SS = 0
      ELSE
         IF(.NOT.ATOI(ASCREC(IDP(I)),SP,2,SS)) RETURN
         IF (SS.LT.0 .OR. SS.GT.59) RETURN
      ENDIF
      IF(.NOT.ATOI(ASCREC(IDP(I)),MP,2,MM)) RETURN
      IF (MM.LT.0 .OR. MM.GT.59) RETURN
      IF(.NOT.ATOI(ASCREC(IDP(I)),HP,2,HH)) RETURN
      IF (HH.LT.0 .OR. HH.GT.24) RETURN
      JDAT = HH*3600 + MM*60 + SS
      LXDATE = .TRUE.
      !
900   RETURN
   END FUNCTION LXDATE


END MODULE Formater

