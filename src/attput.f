      SUBROUTINE ATTPUT(STATUS)
      INCLUDE 'syspar.inc'
C
C  PURPOSE:   REPLACE THE CURRENT TUPLE FROM THE ATTRIBUTE RELATION
C             BASED ON CONDITIONS SET UP IN LOCATT AND ATTGET
C
C  PARAMETERS:
C         STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY
      INCLUDE 'flags.inc'
      INCLUDE 'tuplea.inc'
      INCLUDE 'attble.inc'
      INTEGER STATUS
C
      STATUS = 0
      IF(LROW.EQ.0) GO TO 9000
C
C  MOVE THE STUFF TO ROW LROW.
C
      CALL ZMOVE(ATTBLE(ZA2,LROW),ATTNAM)
      CALL ZMOVE(ATTBLE(ZA3,LROW),RELNAM)
      ATTBLE(ZA4,LROW) = ATTCOL
      ATTBLE(ZA5,LROW) = ATTLEN
      ATTBLE(ZA6,LROW) = ATTYPE
      ATTBLE(ZA7,LROW) = ATTKEY
      ATTBLE(ZA8,LROW) = ATTFOR
      ATTMOD = 1
      IFMOD = .TRUE.
      GO TO 9999
C
C  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
C
 9000 CONTINUE
      STATUS = 1
 9999 CONTINUE
      RETURN
      END
