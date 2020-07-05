      FUNCTION LOCBOO(KOMPAR)
      INCLUDE 'syspar.d'
C
C  FIND THE TYPE OF BOOLEAN COMPARISON THAT KOMPAR IS.
C  JUST CHECK THE FIRST 3 CHARACTERS
C
C  PARAMETERS:
C         KOMPAR--BOOLEAN OPERATOR
C         LOCBOO--CORRESPONDING NUMBER
C
      CHARACTER*(*) KOMPAR
 
      INCLUDE 'ascpar.d'
C
      PARAMETER (MAXBOO=9)
      CHARACTER*3 BOOL(MAXBOO), KOM
      CHARACTER*3 XBOOL(MAXBOO)
      DATA BOOL /'EXI','EQ ','NE ','GT ','GE ','LT ','LE ',
     X     'FAI','LIK' /
      DATA XBOOL/'EXI','=  ','<> ','>  ','>= ','<  ','<= ',
     X     'FAI','LIK' /
C
      KOM = KOMPAR
      DO 100 I=1,MAXBOO
      IF(KOM.EQ.BOOL(I)) GO TO 200
  100 CONTINUE
      DO 110 I=1,MAXBOO
      IF(KOM.EQ.XBOOL(I)) GO TO 200
  110 CONTINUE
      I = 0
  200 LOCBOO = I
      IF(I.EQ.8) LOCBOO = -1
      RETURN
      END
