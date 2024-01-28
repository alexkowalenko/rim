      SUBROUTINE SWFILO(BUFFER,LTUP,LREC,NTUREC,NINTUP,
     X                  INFIL,OUTFIL)

         USE RandomFiles, only: RIOIN, RIOOUT

         INCLUDE 'syspar.inc'
C
C  PURPOSE  LOADING PASS FOR OUT-OF-CORE SORT
C           OF FIXED LENGTH TUPLES
C
C  TIMING   UNKNOWN
C
C  DEFINITION OF VARIABLES
C
C  BUFFER   CORE SCRATCH AREA OF                  (SCRATCH)
C           SUFFICIENT LENGTH
C              GE NINTUP*(1+LREC)+NTUREC*LREC
C
C  LTUP     LENGTH, IN WORDS, OF INDIVIDUAL       (INT,I)
C           TUPLE
C
C  LREC     LENGTH, IN WORDS, OF OUTPUT RECORD    (INT,I)
C
C  NTUREC   NUMBER OF TUPLES PER OUTPUT           (INT,I)
C           RECORD
C
C  NINTUP     NUMBER OF TUPLES                      (INT,I)
C           IN ONE SORT CHAIN
C
C
C  INFIL   FILE UNIT WITH INPUT TUPLES
C           INFIL IS UNFORMATTED (BINARY)
C           EACH TUPLE IS WRITTEN AS A
C           RECORD AS FOLLOWS
C           FOR FIXED LENGTH RECORDS
C             WRITE(INFIL) (TUP(I),I=1,LTUPLE)
C           FOR VARIABLE LENGTH RECORDS
C             WRITE(INFIL) L,(TUP(I),I=1,L)
C
C  OUTFIL   FILE UNIT OF RANDOM SORT FILE
C           CONTAINS CHAINS OF SORTED TUPLES
C           EACH CHAIN CONTAINS ONE OR MORE BLOCKS
C           EACH BLOCK CONTAINS
C            WORD 1   = NO TUPLES IN BLOCK
C            WORD 2   = CHAIN NO,NEG FOR LAST BLOCK
C            WORD 3FF = TUPLES INSORTED ORDER
C
C
         INCLUDE 'srtcom.inc'
         INTEGER BUFFER(1)
         I2 = 0
         J1 = NINTUP*(1+LTUP)
         I8 = 0
   10    CONTINUE
         I8 = I8 + 1
         I1 = NINTUP
         DO 20 I=1,NINTUP
            READ(INFIL) (BUFFER(I1+K),K=1,LTUP)
            I2 = I2 + 1
            BUFFER(I) = I1 + 1
            I1 = I1 + LTUP
            IF(I2 .EQ. NSORT) GO TO 21
   20    CONTINUE
         I = NINTUP
   21    CONTINUE
C
C     READ COMPLETE FOR ONE CHAIN - SORT
C
         CALL SWICST(BUFFER,BUFFER,I)
C
C     SORT COMPLETE - UNLOAD
C
         I3 = 0
   40    CONTINUE
         I4 = J1 + 2
         DO 50 I5=1,NTUREC
            I3 = I3 + 1
            I7 = BUFFER(I3) - 1
            DO 45 I6=1,LTUP
   45       BUFFER(I4+I6) = BUFFER(I7+I6)
            I4 = I4 + LTUP
            IF(I3 .EQ. I) GO TO 55
   50    CONTINUE
         I5 = NTUREC
   55    CONTINUE
C
C  WRITE ONE RECORD
C
         BUFFER(J1+1) = I5
         I7 = I8
         IF(I3 .EQ. I) I7 = -I7
   60    BUFFER(J1+2) = I7
C
C  ADD IN RANDOM I/O STUFF
C
         CALL RIOOUT(OUTFIL,0,BUFFER(J1+1),LREC,IOS)
         IF(I3 .LT. I) GO TO 40
         IF(I2 .LT. NSORT) GO TO 10
C
C     SORT PASS COMPLETE FOR ALL CHAINS
C
         RETURN
      END
