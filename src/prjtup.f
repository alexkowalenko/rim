      SUBROUTINE PRJTUP(POINTS,LENP,LENNEW,OLDTUP,NEWTUP,LENT)
      INCLUDE 'syspar.inc'
C
C     PROJECTION ROUTINE
C
C     THIS ROUTINE BUILDS A NEW TUPLE FROM AN OLD TUPLE USING
C     POINTS AS A GUIDING ARRAY.
C
C   INPUT
C     POINTS  - ARRAY THE LENGTH OF THE FIXED PORTION OF OLDREL.
C               EACH WORD CONTAINS A ZERO OR THE RECIEVING ADDRESS
C               IN NEW TUPLE (ZERO MEANS NOT IN NEW TUPLE)
C               IF ATTRIBUTE IS VARIABLE ADDRESS IS STORED AS NEGATIVE
C     LENP    - LENGTH OF POINTS
C     LENNEW  - LENGTH OF FIXED PORTION OF NEW TUPLE
C     OLDTUP  - OLD TUPLE
C   OUTPUT
C     NEWTUP  - NEW TUPLE
C     LENT    - LENGTH OF NEW TUPLE
C
      INTEGER POINTS(LENP),OLDTUP(LENP),NEWTUP(LENP)
      LENT = LENNEW
      DO 100 I=1,LENP
      IF(POINTS(I).EQ.0) GO TO 100
      IF(POINTS(I).GT.0) GO TO 50
C
C     VARIABLE ATTRIBUTE
C
      IADD = OLDTUP(I)
      NOCOLS = -POINTS(I)
      NEWTUP(NOCOLS) = LENT + 1
      LEN = OLDTUP(IADD) + 2
      DO 40 K=1,LEN
      LENT = LENT + 1
      NEWTUP(LENT) = OLDTUP(IADD)
      IADD = IADD + 1
   40 CONTINUE
      GO TO 100
C
C     FIXED ATTRIBUTE
C
50    NUM = POINTS(I)
      NEWTUP(NUM) = OLDTUP(I)
100   CONTINUE
      RETURN
      END
