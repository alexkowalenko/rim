      SUBROUTINE LOADIT(MAT,ATT)

         USE Globals, only: HXFLAG
         USE Utils, only : ZEROIT

         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE IS THE FORTRAN ROUTINE FOR LOADING DATA VALUES IN THE
C  RIM DATA BASE.
C
C  PARAMETERS:
C         MAT-----SCRATCH ARRAY FOR BUILDING TUPLES
C         ATT-----ARRAY OF TUPLEA VALUES
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'files.inc'
         INCLUDE 'start.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
C
         INTEGER COLUMN
         LOGICAL EQKEYW
         INTEGER ENDCOL
         INTEGER MAT(1)
         INTEGER ATT(ZTUPAL,1)
C
C  READ A CARD.
C
  100    CONTINUE
C
C     POSSIBLE SYSTEM INTERRUPTION
C
         IF (HXFLAG.NE.0) THEN
            CALL WARN(6,0,0)
            GOTO 900
         ENDIF
C
         CALL LODREC

         IF(EQKEYW(1,'LOAD')) GO TO 900
         IF(EQKEYW(1,'END')) GO TO 900
C
C  ASSUME THIS IS A DATA CARD.
C
C  ZERO OUT THE TUPLE.
C
  160    CALL ZEROIT(MAT,MAXCOL)
C
C  CHECK EACH ATTRIBUTE AND MOVE IT TO THE TUPLE FROM INPUT.
C
         NUMKEY = 0
         J = 1
         ENDCOL = NCOL + 1
         IRR = 0
         DO 500 I=1,NATT
            CALL BLKMOV(ATTNAM,ATT(1,I),ZTUPAL)
            COLUMN = ATTCOL
            IF(J.GT.ITEMS) THEN
C       TOO FEW ITEMS
               CALL MSG('W','RECORD WAS IGNORED - TOO FEW ITEMS.',' ')
               GO TO 100
            ENDIF

            IF(ATTKEY.NE.0) NUMKEY = NUMKEY + 1
C
C     CALL PARVAL TO CRACK VALUE STRING
C
            IF(ATTWDS.NE.0) THEN
C
C        FIXED ATTRIBUTE
C
               CALL PARVAL(J,MAT(COLUMN),ATTYPE,ATTWDS,ATTCHA,0,IERR)
               IF(IERR.NE.0) IRR = 1
               GO TO 500
            ELSE
C
C        VARIABLE ATTRIBUTE
C
               MAT(COLUMN) = ENDCOL
               NCOLT = ENDCOL + 1
               CALL PARVAL(J,MAT(ENDCOL+2),ATTYPE,ATTWDS,ATTCHA,
     +                  NCOLT,IERR)
               IF(IERR.NE.0) IRR = 1
               MAT(ENDCOL) = ATTWDS
               MAT(ENDCOL+1) = ATTCHA
               ENDCOL = ENDCOL + ATTWDS + 2
            ENDIF
  500    CONTINUE
         IF(IRR.NE.0) GO TO 100
         ENDCOL = ENDCOL - 1
         IF(J.LE.ITEMS) THEN
C       TOO MANY ITEMS
            CALL MSG('W','RECORD WAS IGNORED - TOO MANY ITEMS.',' ')
            GO TO 100
         ENDIF
         NTUPLE = NTUPLE + 1
         CALL ADDDAT(1,REND,MAT,ENDCOL)
         IF(RSTART.EQ.0) RSTART = REND
         CALL RELPUT
C
C  PROCESS ANY KEY ATTRIBUTES.
C
         IF(NUMKEY.EQ.0) GO TO 100
         CALL PRCKEY('ADD',MAT,ATT)
         GO TO 100
C
C  ALL DONE.
C
  900    CONTINUE
         RETURN
      END
