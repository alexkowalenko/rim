      LOGICAL FUNCTION LXDATE(I,JDAT,FMT,TYP)

         USE Globals, only : KRMTMF
         USE DateTime, only : JULDAT, ASMTXT
         USE Text, only : ATOI

         INCLUDE 'syspar.inc'
C
C  RETURN THE DATE FROM THE ITH ITEM.
C
C     JDAT IS JULIAN
C     FMT IS DATE-FORMAT INTEGER
C     LXDATE IS TRUE IF ALL OK
C     TYP IS DATA TYPE
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'rmatts.inc'
C
         LXDATE = .FALSE.
C
         IF (IDI(I).EQ.ZIMISS .OR. IDI(I).EQ.ZINAPP) THEN
            JDAT = IDI(I)
            LXDATE = .TRUE.
            GOTO 900
         ENDIF

         IF (TYP.EQ.KZTIME) GOTO 500
C
C     DATE
C
         CALL DTFSPL(L,DP,MP,ML,YP,YL,SC,FMT)
         IF (DP.EQ.0) THEN
            DD = 01
         ELSE
            IF (.NOT.ATOI(ASCREC(IDP(I)),DP,2,DD)) RETURN
         ENDIF
         IF (ML.EQ.3) THEN
            AM = 0
            CALL STRMOV(ASCREC(IDP(I)),MP,3,AM,1)
            DO 200 J = 1, 12
               MM = J
  200       IF (AM.EQ.ASMTXT(J)) GOTO 210
            RETURN
         ELSE
            IF(.NOT.ATOI(ASCREC(IDP(I)),MP,ML,MM)) RETURN
         ENDIF
C
  210    IF(.NOT.ATOI(ASCREC(IDP(I)),YP,YL,YY)) RETURN
         IF (YL.EQ.2) YY = YY + 1900
         IF (.NOT.JULDAT(DD,MM,YY,JDAT)) RETURN
         LXDATE = .TRUE.
         GOTO 900
C
C     TIME
C
  500    TFMT = FMT
         IF (TFMT.EQ.0) TFMT = KRMTMF
         CALL DTFSPL(L,SP,MP,XX,HP,XX,SC,TFMT)
         IF (MP.EQ.0) RETURN
C
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
C
  900    RETURN
      END
