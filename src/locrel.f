      FUNCTION LOCREL(RNAME)

         USE RM_Text, only : BLANK
         USE Utils, only : ZMOVE, NULLIT

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   LOOK FOR A RELATION IN THE RELTBL RELATION
C
C  RM_Parameters:
C         RNAME---NAME OF RELATION OR BLANK
C         LOCREL--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY
         INCLUDE 'reltbl.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'rimptr.inc'
         LOGICAL EQ
         INCLUDE 'dclar1.inc'
         LOCREL = 0
C
C  SCAN FOR THIS RELATION.
C
         MRSTRT = ZRELRI
  100    CONTINUE
         CALL RELPAG(MRSTRT)
         I = MRSTRT
  200    CONTINUE
         IF(I.GT.RPBUF) GO TO 400
         IF(RELTBL(1,I).EQ.0) GO TO 9000
         IF(RELTBL(1,I).LT.0) GO TO 300
         IF(EQ(RNAME,BLANK)) GO TO 500
CC    CALL MSG(' ','  LOCREL: ','+')
CC    CALL AMSG(RNAME,ZC,'+')
CC    CALL AMSG(RELTBL(ZR2,I),ZC,' ')
         IF(EQ(RELTBL(ZR2,I),RNAME)) GO TO 500
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
         LRROW = I - 1
         CALL ZMOVE(NAME,RELTBL(ZR2,I))
         RDATE = RELTBL(ZR3,I)
         NCOL = RELTBL(ZR4,I)
         NATT = RELTBL(ZR5,I)
         NTUPLE = RELTBL(ZR6,I)
         RSTART = RELTBL(ZR7,I)
         REND = RELTBL(ZR8,I)
         CALL ZMOVE(RPW,RELTBL(ZR9,I))
         CALL ZMOVE(MPW,RELTBL(ZR10,I))
         CALL ZMOVE(CNAME,RNAME)
C
C  ALSO SET THE VALUES IN THE RIMPTR COMMON BLOCK.
C
         IVAL = 0
         LIMVAL = 0
         CID = RSTART
         NID = CID
         NS = 0
         MID = 0
         GO TO 9999
C
C  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
C
 9000    CONTINUE
         LOCREL = 1
         LRROW = 0
         ! CALL ZMOVE(CNAME,NULL)
         CALL NULLIT(CNAME)
 9999    CONTINUE
         RETURN
      END
