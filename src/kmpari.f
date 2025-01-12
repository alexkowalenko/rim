      SUBROUTINE KMPARI(VALUE1,VALUE2,LEN,NK,OK)
         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE COMPARES VALUE1 AND VALUE2 TO SEE IF THEY MEET THE
C  DESIRED CONDITIONS.
C
C  RM_Parameters
C         VALUE1--FIRST VALUE
C         VALUE2--SECOND VALUE
C         LEN-----VALUE LENGTHS
C         NK------NUMBER FOR COMPARISON TYPE
C         OK------.FALSE. COMING IN, .TRUE. GOING OUT IF THE CONDITIONS
C                 ARE MET
C
         INTEGER VALUE1(1),VALUE2(1)
         LOGICAL OK
C
C  BRANCH ON THE VALUE OF NK.
C
         IF(NK.NE.2) GO TO 30
C  EQ.
         DO 25 I=1,LEN
            IF(VALUE1(I).NE.VALUE2(I)) GO TO 999
   25    CONTINUE
         GO TO 900
   30    IF(NK.NE.3) GO TO 40
C  NE.
         DO 35 I=1,LEN
            IF(VALUE1(I).NE.VALUE2(I)) GO TO 900
   35    CONTINUE
         GO TO 999
   40    IF((NK.NE.4).AND.(NK.NE.5)) GO TO 60
C  GT AND GE.
         DO 45 I=1,LEN
            IF(VALUE1(I).GT.VALUE2(I)) GO TO 900
            IF(VALUE1(I).LT.VALUE2(I)) GO TO 999
   45    CONTINUE
         IF(NK.EQ.5) GO TO 900
         GO TO 999
   60    IF((NK.NE.6).AND.(NK.NE.7)) GO TO 80
C  LT AND LE.
         DO 65 I=1,LEN
            IF(VALUE1(I).LT.VALUE2(I)) GO TO 900
            IF(VALUE1(I).GT.VALUE2(I)) GO TO 999
   65    CONTINUE
         IF(NK.EQ.7) GO TO 900
         GO TO 999
   80    CONTINUE
         GO TO 999
  900    OK = .TRUE.
  999    RETURN
      END
