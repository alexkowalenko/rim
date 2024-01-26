      SUBROUTINE RELGET(STATUS)

         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   GET THE NEXT TUPLE IN THE RELTBL RELATION
C
C  PARAMETERS:
C         STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY
         INCLUDE 'ascpar.inc'
         INCLUDE 'reltbl.inc'
         INCLUDE 'tupler.inc'
         INTEGER STATUS
         LOGICAL EQ
         STATUS = 0
C
C  SCAN FOR THE NEXT RELATION.
C
         I = LRROW + 1
         GO TO 200
  100    CONTINUE
         CALL RELPAG(MRSTRT)
         I = MRSTRT
  200    CONTINUE
         IF(I.GT.RPBUF) GO TO 400
         IF(RELTBL(1,I).EQ.0) GO TO 9000
         IF(RELTBL(1,I).LT.0) GO TO 300
         IF(EQ(CNAME,BLANK)) GO TO 500
         IF(EQ(RELTBL(ZR2,I),CNAME)) GO TO 500
  300    CONTINUE
         I = I + 1
         GO TO 200
C
C  GET THE NEXT PAGE.
C
  400    CONTINUE
         MRSTRT = RELBUF(1)
         IF(MRSTRT.EQ.0) GO TO 9000
         GO TO 100
C
C  FOUND IT.
C
  500    CONTINUE
         LRROW = I
         CALL ZMOVE(NAME,RELTBL(ZR2,I))
         RDATE = RELTBL(ZR3,I)
         NCOL = RELTBL(ZR4,I)
         NATT = RELTBL(ZR5,I)
         NTUPLE = RELTBL(ZR6,I)
         RSTART = RELTBL(ZR7,I)
         REND = RELTBL(ZR8,I)
         CALL ZMOVE(RPW,RELTBL(ZR9,I))
         CALL ZMOVE(MPW,RELTBL(ZR10,I))
         GO TO 9999
C
C  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
C
 9000    CONTINUE
         STATUS = 1
         LRROW = 0
 9999    CONTINUE
         RETURN
      END
