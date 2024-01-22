      SUBROUTINE SWCOST(NOPASS,NREC,LREC,SORD,COST)
      INCLUDE 'syspar.inc'
C
C  PURPOSE  DETERMINE COST OF A SORTING STRATEGY
C
C  METHOD   COMPUTE COST FROM FORMULA
C           COST=NOPASS*(2*NREC*(IOPOSC+LREC*IOTRAC) +
C                 + NSORT*NSOVAR*.5*SORD*COCOST
C                 + NREC*LREC*MOCOFI
C                 + NREC*(LREC-1)*MOCOAD)
C
C  DEFINITION OF PARAMETERS
C
C  NOPASS  NUMBER OF SORT PASSES EXCLUDING SEQUENTIAL     (INT,I)
C          READ AND WRITE (FIRST AND LAST)
C          EACH PASS CONSISTS OF ONE READ AND ONE WRITE
C
C  NREC   NUMBER OF PAGES ON SORT SCRATCH FILE           (INT,I)
C
C  LREC    LENGTH OF A SORT PAGE                          (INT,I)
C
C  SORD     SORT ORDER,I.E. NUMBER OF INPUT SORT BLOCKS   (INT,I)
C           IN CORE DURING MERGE PHASE
C
C  COST FORMULA PARAMETERS
C
C  IOPOSC  = RELATIVE COST FOR I OR O POSITIONING
C
C  IOTRAC  = RELATIVE COST OF I OR O TRANSFER OF ONE WORD
C
C  COCOST  = RELATIVE COST OF COMPARING TWO SINGLE VARIABLES
C
C  MOCOFI  = RELATIVE COST OF MOVING FIRST WORD OF ONE
C            BLOCK IN CORE
C
C  MOCOAD  = RELATIVE COST OF MOVING ADDITIONAL WORDS
C            OF THE BLOCK IN CORE
C
      INCLUDE 'srtcom.inc'
      INTEGER SORD
      REAL IOPOSC,IOTRAC,COCOST,MOCOFI,MOCOAD
      REAL COST
CCC   INCLUDE (DATA5)
C
C  COST CONSTANTS FOR SWCOST
C
      DATA IOPOSC /375000./
      DATA IOTRAC /130./
      DATA COCOST /12./
      DATA MOCOFI /4./
      DATA MOCOAD /4./
C
      COST = NOPASS*(2*NREC*(IOPOSC+LREC*IOTRAC)
     X      +NSORT*NSOVAR*.5*SORD*COCOST
     X      +NREC*MOCOFI+NREC*(LREC-1)*MOCOAD)
      RETURN
      END
