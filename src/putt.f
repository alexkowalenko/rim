      SUBROUTINE PUTT(ASTXT,POS,ASCHR)
         USE RM_Parameters, only : ZCW
         implicit none
C
C     PUT THE ASCII-CHAR INTO ASTXT AT POS
C
         INTEGER ASTXT(*), POS, ASCHR
C
         INTEGER, PARAMETER :: MM=2**(ZCW*7)-1
         INTEGER, PARAMETER :: MC=2**7-1
         INTEGER :: IW, IC
C
         IW = (POS+ZCW-1)/ZCW
         IC = (IW*ZCW) - POS
         ASTXT(IW) =  OR(  AND(ASTXT(IW),MM-LSHIFT(MC,IC*7)),
     1      LSHIFT( AND(ASCHR,MC),IC*7))
         RETURN
      END
