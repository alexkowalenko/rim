      SUBROUTINE RELADD

         USE Globals, only : IFMOD
         USE Files, only : LF1REC
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   ADD A NEW TUPLE TO THE RELTBL RELATION
C
         INCLUDE 'tupler.inc'
         INCLUDE 'reltbl.inc'
C
C  GET THE PAGE FOR ADDING NEW TUPLES.
C
         MRSTRT = NRROW
         CALL RELPAG(MRSTRT)
         I = MRSTRT
         NRROW = NRROW + 1
         IF(I.EQ.RPBUF) NRROW = (RPBUF * LF1REC) + 1
C
C  MOVE THE DATA FROM THE TUPLE TO THE BUFFER.
C
         RELTBL(ZR1,I) = NRROW
         CALL ZMOVE(RELTBL(ZR2,I),NAME)
         ! CALL ZMOVE(RELTBL(ZR3,I),RDATE)
         RELTBL(ZR3,I) = RDATE ! Copy an integer
         RELTBL(ZR4,I) = NCOL
         RELTBL(ZR5,I) = NATT
         RELTBL(ZR6,I) = NTUPLE
         RELTBL(ZR7,I) = RSTART
         RELTBL(ZR8,I) = REND
         CALL ZMOVE(RELTBL(ZR9,I),RPW)
         CALL ZMOVE(RELTBL(ZR10,I),MPW)
         RELMOD = 1
         IFMOD = .TRUE.
         LRROW = 0
         IF(I.LT.RPBUF) RETURN
C
C  WE JUST FILLED A BUFFER. MAKE SURE RELTBL GETS THE NEXT ONE.
C
         RELBUF(1) = NRROW
         MRSTRT = NRROW
         CALL RELPAG(MRSTRT)
         RETURN
      END
