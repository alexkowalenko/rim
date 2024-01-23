      SUBROUTINE ASCDAT(STR,SC,L,JDAT,DFMT,TYP)

      USE DateTime, only : DATJUL

      INCLUDE 'syspar.inc'
C
C     CONVERT A DATE INTEGER TO ASCII-TEXT
C
C  PARAMETERS
C
C         STR ---DESTINATION STRING (ASCII-TEXT)
C         SC ----START POS IN STR
C         L -----LENGTH OF STR
C         JDAT---JULIAN DATE INTEGER
C         DFMT----DATE-FORMAT INTEGER
C         TYP----TYPE (KZDATE / KZTIME)
C
      INTEGER STR(1)
C
      INCLUDE 'ascpar.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'flags.inc'
C
      PARAMETER (U0=48,U9=57,PLUS=43,MINUS=45,DECIM=46,COLON=58)
C
      S = SC - 1
      IF (TYP.EQ.KZTIME) GOTO 500
C
C     IS A DATE
C
      FMT = DFMT
      IF (FMT.EQ.0) FMT = KRMDTF
      CALL DTFSPL(L,DP,MP,ML,YP,YL,ASC,FMT)
      CALL FILCH(STR,SC,L,ABLANK)
C
      IF (JDAT.EQ.ZIMISS) GOTO 700
      IF (JDAT.EQ.ZINAPP) GOTO 710
C
      CALL DATJUL(DD,MM,YY,JDAT)
      CALL FILCH(STR,SC,L,ASC)
C
      IF (DP.NE.0) THEN
         CALL ITOA(STR,S+DP,2,DD,ERR)
         IF (DD.LT.10) CALL PUTT(STR,S+DP,U0)
      ENDIF
C
      IF (ML.EQ.3) THEN
         CALL STRMOV(ASMTXT(MM),1,3,STR,S+MP)
      ELSE
         CALL ITOA(STR,S+MP,2,MM,ERR)
         IF (MM.LT.10) CALL PUTT(STR,S+MP,U0)
      ENDIF
C
      IF (YL.EQ.2) YY = YY - 1900
      CALL ITOA(STR,S+YP,YL,YY,ERR)
      IF (YY.LT.10) CALL PUTT(STR,S+YP,U0)
      GOTO 800
C
C     TIME
C
500   FMT = DFMT
      IF (FMT.EQ.0) FMT = KRMTMF
      CALL DTFSPL(L,SP,MP,XX,HP,XX,ASC,FMT)
      CALL FILCH(STR,SC,L,ABLANK)
C
      IF (JDAT.EQ.ZIMISS) GOTO 700
      IF (JDAT.EQ.ZINAPP) GOTO 710
C
      HH = JDAT / 3600
      MM = MOD(JDAT,3600)/60
      SS = MOD(JDAT,60)
      CALL FILCH(STR,SC,L,ASC)
C
      IF (SP.NE.0) THEN
         CALL ITOA(STR,S+SP,2,SS,ERR)
         IF (SS.LT.10) CALL PUTT(STR,S+SP,U0)
      ENDIF
C
      CALL ITOA(STR,S+MP,2,MM,ERR)
      IF (MM.LT.10) CALL PUTT(STR,S+MP,U0)
C
      CALL ITOA(STR,S+HP,2,HH,ERR)
      IF (HH.LT.10) CALL PUTT(STR,S+HP,U0)
      GOTO 800
C
C     MISSING VALUE
C
700   CALL STRMOV(KMSSVT,1,MIN(KMSSVL,L),STR,SC)
      GOTO 800
C
C     NOT APPL
C
710   CALL STRMOV(KNAPVT,1,MIN(KNAPVL,L),STR,SC)
C
800   RETURN
      END
