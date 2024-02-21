      SUBROUTINE RMRES(INDPTR)

         USE RM_Globals, only : TRACE, RMSTAT
         USE RM_BufferData, only: BUFFER
         USE Extern, only: IMSG, MSG
         USE RM_Relations, only: LOCREL

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   RESTORE THE INTERNAL POINTERS FOR THE
C             MULTIPLE PROGRAM INTERFACE.
C
C  RM_Parameters:
C     INPUT:  INDPTR--INDEX TO SAVE BLOCK (RANGE OF 0 TO ZPIMAX)
         INCLUDE 'vardat.inc'
         INCLUDE 'keydat.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'reltbl.inc'
         INCLUDE 'ptrcom.inc'
         INCLUDE 'srtcom.inc'
         LOGICAL NE
         LOGICAL EQ
         INTEGER BLKLOC
C
C  SEE IF THE INDEX IS WITHIN RANGE.
C
         IF(INDPTR.EQ.NULL) GO TO 400
         IF((INDPTR.LT.1).OR.(INDPTR.GT.ZPIMAX)) GO TO 500
C
C  SEE IF THE CURRENT BLOCK IS ALREADY THERE.
C
         IF (TRACE.GE.5) THEN
            CALL MSG('T','RMRES1:','+')
            CALL IMSG(INDPTR,5,'+')
            CALL IMSG(INDCUR,11,' ')
         ENDIF

         IF(INDPTR.EQ.INDCUR) GO TO 999
C
C  SAVE THE CURRENT BLOCKS.
C
         CALL RMSAV(INDCUR)
C
C  RESTORE THE BLOCKS.
C
         DO 100 I=1,INDMAX
            IF(INDNUM(I).EQ.INDPTR) GO TO 200
  100    CONTINUE
C
C  NUMBER HAS NOT BEEN SAVED.
C
         GO TO 400
C
C     SET INDCUR = NULL IN CASE RESTORE IS BAD
C
  200    INDCUR = NULL
C
C  GET THE START OF THE SAVE BUFFER.
C
         I = INDPTR + 10
         RMSTAT = 0
         KQ1 = BLKLOC(I)
         IF (TRACE.GE.5) THEN
            CALL MSG('T','RMRES2:','+')
            CALL IMSG(INDPTR,5,'+')
            CALL IMSG(KQ1,6,' ')
         ENDIF
         IF(RMSTAT.NE.0) RETURN
C
C  MOVE THE VALUES FROM THE SAVE BUFFER.
C
C  TUPLEA
         CALL BLKMOV(ATTNAM,BUFFER(KQ1),ZTUPAL)
         KQ1 = KQ1 + ZTUPAL
C  TUPLER
         CALL BLKMOV(NAME,BUFFER(KQ1),ZTUPAR)
         KQ1 = KQ1 + ZTUPAR
         IF(NE(NAME,CNAME)) THEN
            J = LOCREL(NAME)
            LRROW = LRROW + 1
         ENDIF
C  RIMPTR
         CALL BLKMOV(IVAL,BUFFER(KQ1),ZRMPTR)
         KQ1 = KQ1 + ZRMPTR
C  VARDAT
         N = BUFFER(KQ1)
         NW = 1 + (N*2)
         CALL BLKMOV(NUMVAR,BUFFER(KQ1),NW)
         KQ1 = KQ1 + NW
C  KEYDAT
         N = BUFFER(KQ1)
         NW = 1 + (N*ZKEYDA)
         CALL BLKMOV(NUMKEY,BUFFER(KQ1),NW)
         KQ1 = KQ1 + NW
C  SRTCOM
         LTUPLE = BUFFER(KQ1  )
         NSORT = BUFFER(KQ1+1)
         NREAD = BUFFER(KQ1+2)
         NSOVAR = BUFFER(KQ1+3)
         CALL BLKMOV(FIXLT,BUFFER(KQ1+4),1)
         OFFSET = BUFFER(KQ1+5)
         NKSRT0 = BUFFER(KQ1+6)
         KQ1 = KQ1 + 7
C  WHCOM
         NBOO = BUFFER(KQ1)
         KSTRT = BUFFER(KQ1+1)
         MAXTU = BUFFER(KQ1+2)
         LIMTU = BUFFER(KQ1+3)
         NEXPOS = BUFFER(KQ1+4)
         NEXPOT = BUFFER(KQ1+5)
         KQ1 = KQ1 + 6
         IF(NBOO.EQ.0) GO TO 230
         CALL BLKMOV(BOO,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KATTP,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KATTL,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KATTY,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KOMTYP,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KOMPOS,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KOMLEN,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(KOMPOT,BUFFER(KQ1),NBOO)
         KQ1 = KQ1 + NBOO
         CALL BLKMOV(WHRVAL,BUFFER(KQ1),NEXPOS)
         KQ1 = KQ1 + NEXPOS
         CALL BLKMOV(WHRLEN,BUFFER(KQ1),NEXPOT)
         KQ1 = KQ1 + NEXPOT
  230    CONTINUE
         INDCUR = INDPTR
         GO TO 999
  400    CONTINUE
         RMSTAT = 50
         GO TO 999
  500    CONTINUE
         RMSTAT = 70
  999    CONTINUE
         RETURN
      END
