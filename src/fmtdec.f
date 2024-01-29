      SUBROUTINE FMTDEC(DTFINT,TYPE,DTFASC,DTFASL)

         USE Text, only : ABLANK
         USE Utils, only : NDIGIT

         INCLUDE 'syspar.inc'
C
C     DECODE A FORMAT INTEGER INTO A STRING (ASCII-TEXT)
C
C     DTFINT = DATE FORMAT INTEGER
C     TYPE   = DATA TYPE (KZDATE OR OTHER)
C     DTFASC = DECODED STRING
C     DTFASL = LENGTH OF DTFASC
C
         INTEGER DTFASC(1)
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'lxlcom.inc'
C
C     OFFSETS FOR DATA PACKING FOR DATE TYPES
C
         PARAMETER (T1=12, T2=12**2, T3=12**3, T4=12**4)
         PARAMETER (TA=128)
C
C     SOME ASCII-CHARS FOR INT AND REAL FORMATS
C
         PARAMETER (DCHR=100,MCHR=109,YCHR=121)
         PARAMETER (SCHR=115,HCHR=104)
         PARAMETER (ICHR=105,ACHR=97,FCHR=102,ECHR=101)
C
         DO 10 I = 1, DTFASL
   10    CALL PUTT(DTFASC,I,ABLANK)
         IF (DTFINT.EQ.0) RETURN
C
         IF (TYPE.NE.KZDATE) GOTO 200
C
C     DATE FORMAT
C
         CALL DTFSPL(L,DP,MP,ML,YP,YL,ASC,DTFINT)
C
C     BUILD STRING
C
         DE = DP+1
         ME = MP+ML-1
         YE = YP+YL-1
         IF (L.GT.DTFASL) L = DTFASL
         DO 150 I = 1, L
            CH = ASC
            IF (I.GE.DP .AND. I.LE.DE) CH = DCHR
            IF (I.GE.MP .AND. I.LE.ME) CH = MCHR
            IF (I.GE.YP .AND. I.LE.YE) CH = YCHR
            CALL PUTT(DTFASC,I,CH)
  150    CONTINUE
         GOTO 900
C
  200    IF (TYPE.NE.KZTIME) GOTO 300
C
C     TIME FORMAT
C
         CALL DTFSPL(L,SP,MP,XX,HP,XX,ASC,DTFINT)
C
C     BUILD STRING
C
         SE = SP+1
         ME = MP+1
         HE = HP+1
         IF (L.GT.DTFASL) L = DTFASL
         DO 250 I = 1, L
            CH = ASC
            IF (I.GE.SP .AND. I.LE.SE) CH = SCHR
            IF (I.GE.MP .AND. I.LE.ME) CH = MCHR
            IF (I.GE.HP .AND. I.LE.HE) CH = HCHR
            CALL PUTT(DTFASC,I,CH)
  250    CONTINUE
         GOTO 900
C
C
C     INTEGER OR REAL TYPE
C
  300    CALL TYPER(TYPE,S,T)
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
C
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
  900    RETURN
      END
