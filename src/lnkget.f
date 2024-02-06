      SUBROUTINE LNKGET(STATUS)

         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     GET THE NEXT LINK ROW
C
C     PARAMETERS:
C         STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

         INCLUDE 'lnktbl.inc'
         INCLUDE 'tuplel.inc'
         INTEGER STATUS
         LOGICAL EQ
         STATUS = 0
C
C  SCAN FOR THE NEXT LINK
C
         I = LLROW + 1
         GO TO 200
C
  100    CALL LNKPAG(MRSTRT)
         I = MRSTRT
C
  200    IF(I.GT.LPBUF) GO TO 400
         IF(LNKTBL(1,I).EQ.0) GO TO 9000
         IF(LNKTBL(1,I).LT.0) GO TO 300
         GOTO 500
C     IF(EQ(LNAME,BLANK)) GO TO 500
C     IF(EQ(LNKTBL(ZR2,I),LNAME)) GO TO 500
  300    I = I + 1
         GO TO 200
C
C  GET THE NEXT PAGE.
C
  400    MRSTRT = LNKBUF(1)
         IF(MRSTRT.EQ.0) GO TO 9000
         GO TO 100
C
C  FOUND IT.
C
  500    LLROW = I
         CALL ZMOVE(LNAME,LNKTBL(ZL2,I))
         CALL ZMOVE(R1NAME,LNKTBL(ZL3,I))
         CALL ZMOVE(A1NAME,LNKTBL(ZL4,I))
         CALL ZMOVE(R2NAME,LNKTBL(ZL5,I))
         CALL ZMOVE(A2NAME,LNKTBL(ZL6,I))
         GO TO 9999
C
C  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
C
 9000    CONTINUE
         STATUS = 1
         LLROW = 0
 9999    CONTINUE
         RETURN
      END
