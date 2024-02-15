      INTEGER FUNCTION SWVACP(TUP1,TUP2,ATTNUM)
         INCLUDE 'syspar.inc'
C  THIS ROUTINE COMPARES TWO VARIABLE LENGTH ATTRIBUTES
C
C  DAVID BOWEN
C    UNIVERSITY COMPUTING COMPANY (UCC)
C    DALLAS, TX 75207
C
         INCLUDE 'srtcom.inc'
         INTEGER TUP1(1),TUP2(1),ATTNUM
         INTEGER SWIICP,SWIDCP,SWIRCP,SWITCP
    1    CONTINUE
         L = VARPOS(ATTNUM) + OFFSET
         I1 = TUP1(L) + OFFSET
         I2 = TUP2(L) + OFFSET
         LEN1 = TUP1(I1)
         LEN2 = TUP2(I2)
         LEN = MIN(LEN1,LEN2)
         KGOTO = VARTYP(ATTNUM)
         GO TO (11,12,13,14), KGOTO
   11    J2 = SWIICP(TUP1(I1+2),TUP2(I2+2),LEN)
         GO TO 15
   12    J2 = SWIRCP(TUP1(I1+2),TUP2(I2+2),LEN)
         GO TO 15
   13    J2 = SWIDCP(TUP1(I1+2),TUP2(I2+2),LEN)
         GO TO 15
   14    J2 = SWITCP(TUP1(I1+2),TUP2(I2+2),LEN)
   15    CONTINUE
         IF (J2.EQ.0) GO TO 20
         SWVACP = J2
         RETURN
   20    IF (VARTYP(ATTNUM).NE.4) GO TO 30
C  USE LENGTH IN CHARACTERS FOR RM_Text
         LEN1 = TUP1(I1+1)
         LEN2 = TUP2(I2+1)
   30    CONTINUE
         IF (LEN1.GT.LEN2) GO TO 40
         IF (LEN1.LT.LEN2) GO TO 50
         SWVACP = 0
         RETURN
   40    SWVACP = -1
         RETURN
   50    SWVACP = 1
         RETURN
      END
