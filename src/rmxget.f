      SUBROUTINE RMXGET(INDPTR,TUPLE)
         USE RM_Globals, only : RMSTAT
         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE GETS THE NEXT ROW FROM A RELATION AND STORES
C  IT IN TUPLE.
C
C  RM_Parameters:
C         INDPTR--INDEX TO SAVE BLOCK
C         TUPLE---USER ARRAY TO HOLD ONE COMPLETE TUPLE

         INCLUDE 'vardat.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'picom.inc'
C
         INTEGER TUPLE(1)

         RMSTAT = 0

C
C  SET THE INDEX POINTER
C
         INDEX = INDPTR
         IF(INDEX.EQ.0) INDEX = 1
         IF(INDEX.GT.3) INDEX = 3

         IF(NS.EQ.1) THEN
C
C  SORTED RETRIEVAL
            LENGTH = NCOL
            CALL GTSORT(MAT,INDEX,1,LENGTH)
            IF(RMSTAT.EQ.0) GO TO 100
         ELSE
C  UNSORTED RETRIEVAL
            CALL RMLOOK(MAT,INDEX,1,LENGTH)
            IF(RMSTAT.EQ.0) GO TO 100
         ENDIF
C
C  END OF DATA
C
   75    RMSTAT = -1
         IVAL = ALL9S
         GO TO 9999
C
C  MOVE THE DATA.
C
  100    CALL BLKMOV(TUPLE,BUFFER(MAT),LENGTH)
         IF (NUMVAR.NE.0) CALL RMVARC(-1,TUPLE)

 9999    CID2=CID
         NID2=NID
         STAT2=RMSTAT
         LEN2=LENGTH
         RETURN
      END
