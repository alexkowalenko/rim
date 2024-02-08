      INTEGER FUNCTION BLKLOC(IND)
         USE Globals, only : RMSTAT
         INCLUDE 'syspar.inc'
C
C  PURPOSE:    RETURN THE STARTING ADDRESS FOR THE REQUESTED BLOCK
C
C  PARAMETERS
C     INPUT:   IND-----BLOCK INDEX
C     OUTPUT:  BLKLOC--ADDRESS OF 1,1 ENTRY FOR THE BLOCK


         INCLUDE 'incore.inc'
C
C     TRACING
CCC   IF (TRACE.GT.20) THEN
CCC      CALL MSG('T','BLKLOC:','+')
CCC      CALL IMSG(IND,5,'+')
CCC      CALL IMSG(BLOCKS(1,IND),6,' ')
CCC   ENDIF
C
         KS = BLOCKS(1,IND)
         IF(KS.EQ.0) GO TO 7500
         BLKLOC = KS
         RETURN
C
C  UNDEFINED BLOCK.
C
 7500    CONTINUE
         RMSTAT = 1002
         BLKLOC = 0
         RETURN
      END
