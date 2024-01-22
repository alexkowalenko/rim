      SUBROUTINE DTFENC(TYP,DTFINT,DTFCHR)
      INCLUDE 'syspar.inc'
C
C     ENCODE A DATE/TIME FORMAT STRING INTO AN INTEGER
C
C     MAX STRING LENGTH IS 12 CHARS
C     DATE DAY FIELD AND TIME SECONDS FIELD ARE OPTIONAL
C
      CHARACTER*(*) DTFCHR
C
      INCLUDE 'rmatts.inc'
C
C     OFFSETS FOR DATA PACKING
C
      PARAMETER (T1=12, T2=12**2, T3=12**3, T4=12**4, T5=12**5)
      PARAMETER (TA=128)
      CHARACTER*(1) SC
C
      IF (TYP.EQ.KZTIME) GOTO 500
C
C     DATE FORMAT
C
C     DAY
      DP = INDEX(DTFCHR,'DD')
C
C     MONTH
      ML = 3
      MP = INDEX(DTFCHR,'MMM')
      IF (MP.EQ.0) THEN
         ML = 2
         MP = INDEX(DTFCHR,'MM')
      ENDIF
C
C     YEAR
      YL = 4
      YP = INDEX(DTFCHR,'YYYY')
      IF (YP.EQ.0) THEN
         YL = 2
         YP = INDEX(DTFCHR,'YY')
      ENDIF
C
C     LOOK FOR SEPERATION CHAR
C
      DO 100 I = 1, LEN(DTFCHR)
      SC = DTFCHR(I:I)
      IF (SC.NE.'Y' .AND. SC.NE.'M' .AND. SC.NE.'D') GOTO 110
100   CONTINUE
      SC = ' '
110   CONTINUE
C
C     CHECK VALIDITY
C
      DTFINT = 0
      IF (MP.EQ.0 .OR. YP.EQ.0) RETURN
      L = LEN(DTFCHR)
      DTFINT = (L*T5 + DP*T4 + MP*T3 + ML*T2 + YP*T1 + YL)*TA
     X   + ASCCHR(SC)
      GOTO 900
C
C     TIME FORMAT
C
C     SECOND
500   SP = INDEX(DTFCHR,'SS')
C
C     MINUTE
      MP = INDEX(DTFCHR,'MM')
C
C     HOUR
      HP = INDEX(DTFCHR,'HH')
C
C     LOOK FOR SEPERATION CHAR
C
      DO 600 I = 1, LEN(DTFCHR)
      SC = DTFCHR(I:I)
      IF (SC.NE.'H' .AND. SC.NE.'M' .AND. SC.NE.'S') GOTO 610
600   CONTINUE
      SC = ' '
610   CONTINUE
C
C     CHECK VALIDITY
C
      DTFINT = 0
      IF (MP.EQ.0 .OR. HP.EQ.0) RETURN
      L = LEN(DTFCHR)
      DTFINT = (L*T5 + SP*T4 + MP*T3 + HP*T1)*TA
     X   + ASCCHR(SC)
C
900   RETURN
      END
