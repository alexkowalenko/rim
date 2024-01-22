      SUBROUTINE SWINPO(INFIL,OUTFIL,BUFFER)
      INCLUDE 'syspar.inc'
C
C  PURPOSE  CONTROLLING ROUTINE FOR IN-CORE SORT
C              USING IN-SITU POINTER METHOD
C
C
C  TIMING   UNKNOWN
C
C  DEFINITION OF VARIABLES
C
C  INFIL  FILE UNIT WITH INPUT TUPLES
C         INFIL IS UNFORMATTED (BINARY)
C         EACH TUPLE IS WRITTEN AS A
C         RECORD AS FOLLOWS
C         FOR FIXED LENGTH RECORDS
C           WRITE(INFIL) (TUP(I),I=1,LTUPLE)
C         FOR VARIABLE LENGTH RECORDS
C           WRITE(INFIL) L,(TUP(I),I=1,L)
C
C  OUTFIL FILE UNIT FOR OUTPUT TUPLES
C          OUTFIL MAY EQ INFIL
C          FORMAT OF OUTFIL IS THE
C          SAME AS THAT OF INFIL
C
C  BUFFER  CORE BUFFER TO USE FOR SORT      (ANY,SCR)
C
      INCLUDE 'srtcom.inc'
      DIMENSION BUFFER(1)
      INTEGER BUFFER
C
      I1 = NSORT
      IF(FIXLT) GO TO 10
C
C  INCORE,VAR LENGTH
C
      I1 = I1 + 1
      DO 5 I2=1,NSORT
      BUFFER(I2) = I1 + 1
      READ(INFIL) I4,(BUFFER(I1+K),K=1,I4)
      BUFFER(I1) = I4
    5 I1 = I1 + I4 + 1
      GO TO 20
   10 CONTINUE
C
C  INCORE,FIXED LENGTH TUPLES
C
      DO 15 I2=1,NSORT
      BUFFER(I2)= I1 + 1
      READ(INFIL) (BUFFER(I1+K),K=1,LTUPLE)
   15 I1 = I1 + LTUPLE
   20 CONTINUE
C
C  READ COMPLETED,SORT
C
      CALL SWICST(BUFFER,BUFFER,NSORT)
C
C  SORT COMPLETE,UNLOAD
C
      REWIND OUTFIL
      IF(FIXLT) GO TO 40
C
C  VARIABLE LENGTH TUPLES
C
      DO 35 I2=1,NSORT
      I3 = BUFFER(I2) - 1
      I4 = BUFFER(I3)
      WRITE(OUTFIL) I4,(BUFFER(I3+K),K=1,I4)
   35 CONTINUE
      RETURN
   40 CONTINUE
C
C  WRITE FIXED LENGTH TUPLES
C
      DO 45 I2=1,NSORT
      I3 = BUFFER(I2) - 1
      WRITE(OUTFIL) (BUFFER(I3+K),K=1,LTUPLE)
   45 CONTINUE
      RETURN
      END
