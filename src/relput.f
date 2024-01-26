      SUBROUTINE RELPUT

         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   REPLACE THE CURRENT TUPLE FROM THE RELTBL RELATION
C             BASED ON CONDITIONS SET UP IN LOCREL
C
         INCLUDE 'flags.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'reltbl.inc'
C
    1    IF(LRROW.EQ.0) GO TO 9999
C
C  MOVE THE STUFF TO ROW LRROW.
C
         CALL ZMOVE(RELTBL(ZR2,LRROW),NAME)
         RELTBL(ZR3,LRROW) = RDATE
         RELTBL(ZR4,LRROW) = NCOL
         RELTBL(ZR5,LRROW) = NATT
         RELTBL(ZR6,LRROW) = NTUPLE
         RELTBL(ZR7,LRROW) = RSTART
         RELTBL(ZR8,LRROW) = REND
         CALL ZMOVE(RELTBL(ZR9,LRROW),RPW)
         CALL ZMOVE(RELTBL(ZR10,LRROW),MPW)
         RELMOD = 1
         IFMOD = .TRUE.
 9999    CONTINUE
         RETURN
      END
