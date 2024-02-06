      SUBROUTINE RMSAV(INDPTR)

         USE Globals, only : TRACE
         USE Rim, only : RMSTAT

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   SAVE THE INTERNAL POINTERS FOR THE
C             MULTIPLE PROGRAM INTERFACE.
C
C  PARAMETERS:
C     INPUT:  INDPTR--INDEX TO SAVE BLOCK (RANGE OF 1 TO ZPIMAX)
         INCLUDE 'keydat.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'vardat.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'ptrcom.inc'
         INCLUDE 'buffer.inc'
C
C  SEE IF THE INDEX IS WITHIN RANGE.
C
         RMSTAT = 0
         IF (INDPTR.EQ.NULL) GOTO 420
         IF((INDPTR.LT.0).OR.(INDPTR.GT.ZPIMAX)) GO TO 500
         IF(INDMAX.EQ.0) GO TO 300
         DO 200 I=1,INDMAX
            IF(INDNUM(I).EQ.INDPTR) GO TO 400
  200    CONTINUE
C
C  NUMBER HAS NOT BEEN SAVED.
C
  300    INDMAX = INDMAX + 1
         INDNUM(INDMAX) = INDPTR
C
C  SAVE ALL BLOCKS.
C
C  SEE HOW MANY WORDS WE NEED TO SAVE THE POINTERS.
C
  400    NW = 0
C  TUPLEA
         NW = NW + ZTUPAL
C  TUPLER
         NW = NW + ZTUPAR
C  RIMPTR
         NW = NW + ZRMPTR
C  VARDAT
         N = 1 + (NUMVAR*2)
         NW = NW + N
C  KEYDAT
         N = 1 + (NUMKEY*ZKEYDA)
         NW = NW + N
C  SRTCOM
         NW = NW + 7
C  WHCOM
         NW = NW + 6
         NW = NW + (NBOO*8)
         IF(NBOO.NE.0) NW = NW + NEXPOS
         IF(NBOO.NE.0) NW = NW + NEXPOT
C
C  MAKE ROOM IN THE BUFFER.
C
         I = INDPTR + 10
         RMSTAT = 0
         CALL BLKCHG(I,NW,1)
         IF(RMSTAT.NE.0) RETURN
         KQ1 = BLKLOC(I)

         IF (TRACE.GE.5) THEN
            CALL MSG('T','RMSAV:','+')
            CALL IMSG(INDPTR,5,'+')
            CALL IMSG(KQ1,6,'+')
            CALL IMSG(NW,6,' ')
         ENDIF

C
C  MOVE THE VALUES TO THE SAVE BUFFER.
C
C  TUPLEA
         CALL BLKMOV(BUFFER(KQ1),ATTNAM,ZTUPAL)
         KQ1 = KQ1 + ZTUPAL
C  TUPLER
         CALL BLKMOV(BUFFER(KQ1),NAME,ZTUPAR)
         KQ1 = KQ1 + ZTUPAR
C  RIMPTR
         CALL BLKMOV(BUFFER(KQ1),IVAL,ZRMPTR)
         KQ1 = KQ1 + ZRMPTR
C  VARDAT
         NW = 1 + (NUMVAR*2)
         CALL BLKMOV(BUFFER(KQ1),NUMVAR,NW)
         KQ1 = KQ1 + NW
C  KEYDAT
         NW = 1 + (NUMKEY*ZKEYDA)
         CALL BLKMOV(BUFFER(KQ1),NUMKEY,NW)
         KQ1 = KQ1 + NW
C  SRTCOM
         BUFFER(KQ1  ) = LTUPLE
         BUFFER(KQ1+1) = NSORT
         BUFFER(KQ1+2) = NREAD
         BUFFER(KQ1+3) = NSOVAR
         CALL BLKMOV(BUFFER(KQ1+4),FIXLT,1)
         BUFFER(KQ1+5) = OFFSET
         BUFFER(KQ1+6) = NKSRT0
         KQ1 = KQ1 + 7
C  WHCOM
         BUFFER(KQ1) = NBOO
         BUFFER(KQ1+1) = KSTRT
         BUFFER(KQ1+2) = MAXTU
         BUFFER(KQ1+3) = LIMTU
         BUFFER(KQ1+4) = NEXPOS
         BUFFER(KQ1+5) = NEXPOT
         KQ1 = KQ1 + 6
         IF(NBOO.EQ.0) GO TO 420
         CALL BLKMOV(BUFFER(KQ1),BOO,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KATTP,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KATTL,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KATTY,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KOMTYP,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KOMPOS,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KOMLEN,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),KOMPOT,NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(BUFFER(KQ1),WHRVAL,NEXPOS)
         KQ1 = KQ1 + NEXPOS
         CALL BLKMOV(BUFFER(KQ1),WHRLEN,NEXPOT)
         KQ1 = KQ1 + NEXPOT
  420    CONTINUE
         INDCUR = INDPTR
         RETURN
  500    CONTINUE
         RMSTAT = 70
         RETURN
      END
