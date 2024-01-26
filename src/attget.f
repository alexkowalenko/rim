      SUBROUTINE ATTGET(STATUS)

         USE Utils, only : ZMOVE, ITOH

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   RETRIEVE THE NEXT TUPLE FROM THE ATTRIBUTE RELATION
C             BASED ON CONDITIONS SET UP IN LOCATT
C
C  PARAMETERS:
C         STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY
         INCLUDE 'ascpar.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'attble.inc'
         LOGICAL EQ
         LOGICAL NE
C
         STATUS = 0
CCC   CALL MSG(' ','  ATTGET: (CROW,CRNAME,CANAME)','+')
C     CALL IMSG(CROW,5,'+')
C     CALL AMSG(CRNAME,ZC,' ')
CCC   CALL AMSG(CANAME,ZC,' ')
         IF(CROW.EQ.0) GO TO 9000
C
C  SEE WHAT THE CALLER WANTS.
C
         IF(EQ(CRNAME,BLANK)) GO TO 1000
C
C  CRNAME IS SPECIFIED.
C
         I = CROW
         GO TO 200
  100    CONTINUE
         CALL ATTPAG(MRSTRT)
C
C  LOOK FOR THE ATTRIBUTE IN THIS RELATION.
C
         I = MRSTRT
  200    CONTINUE
         IF(I.GT.APBUF) GO TO 300
         IF(NE(ATTBLE(ZA3,I),CRNAME)) GO TO 9000
         IF(EQ(CANAME,BLANK)) GO TO 2000
         IF(EQ(ATTBLE(ZA2,I),CANAME)) GO TO 2000
         I = I + 1
         GO TO 200
C
C  GET THE NEXT PAGE.
C
  300    CONTINUE
         MRSTRT = ATTBUF(1)
C
C  CHECK TO SEE THAT WE GET A DIFFERENT PAGE.
C
         IF(MRSTRT.LE.IABS(ATTBUF(2))) GO TO 9000
         IF(MRSTRT.EQ.0) GO TO 9000
         GO TO 100
C
C  SCAN FOR ATTRIBUTE WITHOUT RELATION SPECIFIED.
C
 1000    CONTINUE
         I = CROW
         GO TO 1200
 1100    CONTINUE
         CALL ATTPAG(MRSTRT)
         I = MRSTRT
 1200    CONTINUE
         IF(I.GT.APBUF) GO TO 1400
         IF(ATTBLE(1,I).LT.0) GO TO 1300
         IF(EQ(ATTBLE(ZA2,I),CANAME)) GO TO 2000
 1300    CONTINUE
         I = I + 1
         GO TO 1200
C
C  GET THE NEXT PAGE.
C
 1400    CONTINUE
         MRSTRT = ATTBUF(1)
C
C  CHECK TO SEE THAT WE GET A DIFFERENT PAGE.
C
         IF(MRSTRT.LE.IABS(ATTBUF(2))) GO TO 9000
         IF(MRSTRT.EQ.0) GO TO 9000
         GO TO 1100
C
C  MOVE THE STUFF FROM ROW CROW.
C
 2000    CONTINUE
         CROW = I
         CALL ZMOVE(ATTNAM,ATTBLE(ZA2,CROW))
         CALL ZMOVE(RELNAM,ATTBLE(ZA3,CROW))
         ATTCOL = ATTBLE(ZA4,CROW)
         ATTLEN = ATTBLE(ZA5,CROW)
         ATTYPE = ATTBLE(ZA6,CROW)
         ATTKEY = ATTBLE(ZA7,CROW)
         ATTFOR = ATTBLE(ZA8,CROW)
C
C  UNPAC THE LENGTH DATA
C
         CALL ITOH(ATTCHA,ATTWDS,ATTLEN)
         LROW = CROW
         CROW = CROW + 1
         GO TO 9999
C
C  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
C
 9000    CONTINUE
         STATUS = 1
         CROW = 0
         LROW = 0
 9999    CONTINUE
         RETURN
      END
