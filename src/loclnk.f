      FUNCTION LOCLNK(LKNAME)

         USE Utils, only : ZMOVE, NULLIT

         INCLUDE 'syspar.inc'
C
C     LOOK FOR A LINK IN THE LNKTBL RELATION
C
C     PARAMETERS:
C         LKNAME---NAME OF RELATION OR BLANK
C         LOCLNK--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY
         INCLUDE 'ascpar.inc'
         INCLUDE 'lnktbl.inc'
         INCLUDE 'tuplel.inc'
         INCLUDE 'rimptr.inc'
         INTEGER LKNAME(Z)
         LOGICAL EQ
         LOCLNK = 0
C
C  SCAN FOR THE LINK
C
         MRSTRT = ZLNKRI
         IF (MRSTRT.EQ.0) GOTO 9000
  100    CALL LNKPAG(MRSTRT)
         I = MRSTRT

  200    IF(I.GT.LPBUF) GO TO 400
         IF(LNKTBL(1,I).EQ.0) GO TO 9000
         IF(LNKTBL(1,I).LT.0) GO TO 300
CC    CALL MSG(' ','  LOCLNK: ','+')
CC    CALL AMSG(LKNAME,ZC,'+')
CC    CALL AMSG(LNKTBL(ZL2,I),ZC,' ')
         IF(EQ(LKNAME,BLANK)) GO TO 500
         IF(EQ(LNKTBL(ZL2,I),LKNAME)) GO TO 500
  300    CONTINUE
         I = I + 1
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
  500    LLROW = I - 1
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
         LOCLNK = 1
         LLROW = 0
         ! CALL ZMOVE(LNAME,NULL)
         CALL NULLIT(LNAME)
 9999    CONTINUE
         RETURN
      END
