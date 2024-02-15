      SUBROUTINE SWICST(MM,M,N)
         INCLUDE 'syspar.inc'
         DIMENSION M(1),MM(1)
C
C
C  PURPOSE       TO SORT A SUBSET OF EQUIDISTANT
C                ELEMENTS OF A VECTOR
C
C  TIMING        .00015*N*LN(N) SEC
C
C  DEFINITION OF RM_Parameters
C
C  M         VECTOR OF POINTERS TO MM
C
C  MM        VECTOR OF DATA TO SORT
C
C  N         NUMBER OF ELEMENTS TO SORT
C
C
         INCLUDE 'srtcom.inc'
         INTEGER SWIICP,SWIDCP,SWIRCP,SWITCP,SWVACP
         I = 1
         DO 10 J=1,30
            IF(I .GE. N) GO TO 20
   10    I = I * 2
   20    CONTINUE
         ID1 = I
         NN = N
   50    ID2 = ID1
         I = I/2
         IF(I .GT. 0) GO TO 60
         RETURN
   60    CONTINUE
         ID1 = I
         III = N - I
         IF(III .GT. I) III = I
         DO 500 J=1,III
            I1 = J
            I2 = I1 + ID1
            J1 = M(I1)
            J2 = M(I2)
  200       CONTINUE
            DO 220 JJ3=1,NSOVAR
               JJ4 = VARPOS(JJ3) + OFFSET - 1
               IF (VARLEN(JJ3).NE.0) GO TO 210
               JJJ = SWVACP(MM(J1),MM(J2),JJ3)
               GO TO 215
  210          CONTINUE
               KGOTO = VARTYP(JJ3)
               GO TO (211,212,213,214),KGOTO
  211          JJJ = SWIICP(MM(J1+JJ4),MM(J2+JJ4),VARLEN(JJ3))
               GO TO 215
  212          JJJ = SWIRCP(MM(J1+JJ4),MM(J2+JJ4),VARLEN(JJ3))
               GO TO 215
  213          JJJ = SWIDCP(MM(J1+JJ4),MM(J2+JJ4),VARLEN(JJ3))
               GO TO 215
  214          JJJ = SWITCP(MM(J1+JJ4),MM(J2+JJ4),VARLEN(JJ3))
  215          CONTINUE
               IF(.NOT. SORTYP(JJ3)) JJJ = -JJJ
               IF(JJJ .GT. 0) GO TO 400
               IF(JJJ .LT. 0) GO TO 240
  220       CONTINUE
            GO TO 400
  240       CONTINUE
C
C  NOT IN SORT
C
            M(I1) = J2
            I1 = I1 + ID1
            IF(I1 .LT. I2) GO TO 250
C
C  JUST FLIP-FLOP
C
            M(I2) = J1
            I2 = I2 + ID2
            IF(I2 .GT. NN) GO TO 500
            J2 = M(I2)
            GO TO 200
C
C  MORE THAN ONE TO MOVE DOWN
C
  250       JJ = I2 - ID1
            DO 300 II=I1,JJ,ID1
               J2 = M(I2 - ID1)
               M(I2) = J2
  300       I2 = I2 - ID1
            I2 = JJ + ID1 + ID2
            M(I1) = J1
            IF(I2 .GT. NN) GO TO 500
            J2 = M(I2)
            GO TO 200
C
C  IN SORT
C
  400       I1 = I1 + ID1
            IF(I1 .LT. I2) GO TO 450
C
C  ONE ONLY
C
            I2 = I2 + ID1
            IF(I2 .GT. NN) GO TO 500
            J1 = J2
            J2 = M(I2)
            GO TO 200
C
C   MORE THAN ONE
C
  450       J1 = M(I1)
            GO TO 200
  500    CONTINUE
         GO TO 50
      END
