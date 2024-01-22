      SUBROUTINE KMPART(VALUE1,VALUE2,LEN,NK,OK)
      INCLUDE 'syspar.inc'
C
C     THIS ROUTINE COMPARES LEN'S WORTH OF TEXT WORDS TO
C     SEE IF THEY MEET THE SPECIFIED CONDITION.
C     THE ROUTINE SWITCP IS USED TO ACTUALLY COMPARE
C     TWO WORDS.
C
C     PARAMETERS
C       VALUE1....LIST OF WORDS OF TEXT
C       VALUE2....LIST OF WORDS OF TEXT
C       LEN.......LENGTH OF VALUE1,VALUE2
C       NK........VALUE1 NK'S VALUE2
C                 NK IS AN INTEGER WITH THE FOLLOWING VALUES
C                 NK=2   EQ
C                    3   NE
C                    4   GT
C                    5   GE
C                    6   LT
C                    7   LE
C
C       OK........ .FALSE. COMING IN, .TRUE. GOING OUT IF
C                 CONDITION IS SATISFIED.
C
      INTEGER VALUE1(LEN),VALUE2(LEN)
      INTEGER SWITCP
      LOGICAL OK
 
1     IF(NK.LT.2) GO TO 999
      IF(NK.GT.7) GO TO 999
C
C  COMPARE TWO VALUES 0=EQ  -1=GT  1=LT
C
      J = SWITCP(VALUE1(1),VALUE2(1),LEN)
      IF(J.EQ.0) GO TO 100
      IF(NK.EQ.2) GO TO 999
      K = 5 - J
      IF(NK.EQ.K) GO TO 999
      IF(NK.EQ.K+1) GO TO 999
      GO TO 200
  100 CONTINUE
C
C     EQUAL
C
      IF(NK.EQ.3) GO TO 999
      IF(NK.EQ.4) GO TO 999
      IF(NK.EQ.6) GO TO 999
  200 CONTINUE
      OK = .TRUE.
  999 CONTINUE
CCC   CALL MSG(' ','   RETURN: ','+')
CCC   CALL IMSG(OK,3,' ')
      RETURN
      END
