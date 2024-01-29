      SUBROUTINE KMPARD(VALUE1,VALUE2,LEN,NK,OK)

         USE, intrinsic :: iso_fortran_env

         USE Globals, only : PCENT, TOL

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE COMPARES VALUE1 AND VALUE2 TO SEE IF THEY MEET THE
C  DESIRED CONDITIONS.
C
C  PARAMETERS
C         VALUE1--FIRST VALUE
C         VALUE2--SECOND VALUE
C         LEN-----VALUE LENGTHS
C         NK------NUMBER FOR COMPARISON TYPE
C         OK------.FALSE. COMING IN, .TRUE. GOING OUT IF THE CONDITIONS
C                 ARE MET
C
         REAL(real64) :: TOLL
         REAL(real64) :: VALUE1(1),VALUE2(1)
         LOGICAL OK
         TOLL = TOL
C
C  BRANCH ON THE VALUE OF NK.
C
         IF(NK.NE.2) GO TO 30
C  EQ.
         IF(TOL.NE.0.) GO TO 26
         DO 25 I=1,LEN
            IF(VALUE1(I).NE.VALUE2(I)) GO TO 999
   25    CONTINUE
         GO TO 900
   26    CONTINUE
         IF(PCENT) GO TO 28
         DO 27 I=1,LEN
            IF(VALUE1(I).LT.(VALUE2(I)-TOLL)) GO TO 999
            IF(VALUE1(I).GT.(VALUE2(I)+TOLL)) GO TO 999
   27    CONTINUE
         GO TO 900
   28    CONTINUE
         DO 29 I=1,LEN
            IF(VALUE1(I).LT.(VALUE2(I)*(1.-TOLL))) GO TO 999
            IF(VALUE1(I).GT.(VALUE2(I)*(1.+TOLL))) GO TO 999
   29    CONTINUE
         GO TO 900
   30    IF(NK.NE.3) GO TO 40
C  NE.
         IF(TOL.NE.0.) GO TO 36
         DO 35 I=1,LEN
            IF(VALUE1(I).NE.VALUE2(I)) GO TO 900
   35    CONTINUE
         GO TO 999
   36    CONTINUE
         IF(PCENT) GO TO 38
         DO 37 I=1,LEN
            IF(VALUE1(I).LT.(VALUE2(I)-TOLL)) GO TO 900
            IF(VALUE1(I).GT.(VALUE2(I)+TOLL)) GO TO 900
   37    CONTINUE
         GO TO 999
   38    CONTINUE
         DO 39 I=1,LEN
            IF(VALUE1(I).LT.(VALUE2(I)*(1.-TOLL))) GO TO 900
            IF(VALUE1(I).GT.(VALUE2(I)*(1.+TOLL))) GO TO 900
   39    CONTINUE
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
