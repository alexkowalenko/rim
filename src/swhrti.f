      SUBROUTINE SWHRTI(NN,LL,BUFFER)
      INCLUDE 'syspar.inc'
C
C  PURPOSE   TO SORT FIXED OR VARIABLE LENGTH
C            TUPLES ON ONE OR MORE ATTRIBUTES
C            INCORE SORT
C            FIRST SORT ATTRIBUTE IS INTEGER
C
C  METHOD    FAST SORTING ALGORITHM PUBLISHED
C            1978 BY HART
C            CREATIVE COMPUTING JAN/FEB 1978
C            P 96 FF
C
C  TIMING   UNKNOWN
C
C  DEFINITION OF VARIABLES
C
C  NN       VECTOR OF POINTERS TO BUFFER    (INT,I)
C
C  LL       LINK LIST OF POINTERS TO NN     (INT,O)
C           THE LIST DEFINES THE SORTED ORDER
C           ORDER OF BUFFER
C
C  BUFFER    VECTOR CONTAINING TUPLES TO SORT    (ANY,I)
C            NN POINTER ARE RELATIVE TO BUFFER(1)
C
      INCLUDE 'srtcom.inc'
      INTEGER SWIICP,SWIRCP,SWIDCP,SWITCP
      DIMENSION NN(1),LL(1)
      INTEGER BUFFER(1)
      INTEGER S1
      REAL B1,B2, T1,T2,T3,T4
C
      K1=0
      I=0
      M1=0
      T2=0.
      T4=0.
      J=NSORT+1
      LL(1)=1
      LL(J)=1
      K2=1
      IF(NSORT.LE.1) RETURN
      S1=NSORT
  250 CONTINUE
C  CLIMB THE TREE
      IF(S1.LT.4) GO TO 320
      K2=K2*2
      B2=S1
      B2=B2/2.
      S1=INT(B2)
      T4=T4+(B2-S1)*K2
      GO TO 250
  320 CONTINUE
C  INITIAL CALCULATIONS
      T4=K2-T4
      B2=K2/2
  350 CONTINUE
C  NEXT TWIG
      IF(K1.EQ.K2) RETURN
      K1=K1+1
      T1=K1
      B1=B2
      T3=T2
  400 CONTINUE
C  ADD 1 TO REFLECTED BINARY COUNTER AND CARRY
      T1=T1/2.
      IF(INT(T1).LT.T1) GO TO 470
      M1=M1+1
      T2=T2-B1
      B1=B1/2.
      GO TO 400
  470 CONTINUE
C  TWIG CALCULATIONS
      T2=T2+B1
      IF(S1.EQ.2) GO TO 550
C  3-TWIGS AND 4-TWIGS
      IF(T3.LT.T4) GO TO 560
C  4-TWIG
      M1=-M1
      GO TO 630
  550 IF(T3.LT.T4) GO TO 610
  560 CONTINUE
C  3-TWIG
      M1=M1+1
      I=I+1
      LL(I)=I
      LL(J)=I
      J=J+1
  610 CONTINUE
C  2-TWIG
      M1=M1+1
  630 I=I+1
      L1=I
      LL(I)=I
      LL(J)=I
      L0=J
      J=J+1
      I=I+1
      L2=I
      LL(I)=I
      LL(J)=I
      GO TO 750
  700 CONTINUE
C  MERGE TWIGS AND BRANCHES
      J=J-1
      L0=J-1
      L1=LL(L0)
      L2=LL(J)
  750 CONTINUE
      NNL2 = NN(L2) + VARPOS(1) - 1
      NNL1 = NN(L1) + VARPOS(1) - 1
      J2 = BUFFER(NNL2) - BUFFER(NNL1)
      IF(J2 .GT. 0 .AND. SORTYP(1)) GO TO 820
      IF(J2 .LT. 0 .AND. .NOT. SORTYP(1)) GO TO 820
      IF(J2 .NE. 0) GO TO 765
      IF(NSOVAR .EQ. 1) GO TO 820
      DO 760 J3=2,NSOVAR
      JJ3 = VARPOS(J3) - 1
      NNL1 = NN(L1) + JJ3
      NNL2 = NN(L2) + JJ3
      KGOTO = VARTYP(J3)
      GO TO (752,753,754,755),KGOTO
  752 J2 = SWIICP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
      GO TO 756
  753 J2 = SWIRCP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
      GO TO 756
  754 J2 = SWIDCP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
      GO TO 756
  755 J2 = SWITCP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
  756 CONTINUE
      IF(J2 .EQ. 0) GO TO 760
      IF((J2 .GT. 0 .AND. SORTYP(J3)) .OR.
     X   (J2 .LT. 0 .AND. .NOT. SORTYP(J3)))
     XGO TO 820
      GO TO 765
  760 CONTINUE
      GO TO 820
  765 CONTINUE
      LL(L0)=L2
  770 L0=L2
      L2=LL(L0)
      IF(L2.EQ.L0) GO TO 870
      NNL2 = NN(L2) + VARPOS(1) - 1
      NNL1 = NN(L1) + VARPOS(1) - 1
      J2 = BUFFER(NNL2) - BUFFER(NNL1)
      IF(J2 .GT. 0 .AND. SORTYP(1)) GO TO 795
      IF(J2 .LT. 0 .AND. .NOT. SORTYP(1)) GO TO 795
      IF(J2 .NE. 0) GO TO 770
      IF(NSOVAR .EQ. 1) GO TO 795
      DO 790 J3=2,NSOVAR
      JJ3 = VARPOS(J3) - 1
      NNL1 = NN(L1) + JJ3
      NNL2 = NN(L2) + JJ3
      KGOTO = VARTYP(J3)
      GO TO (781,782,783,784),KGOTO
  781 J2 = SWIICP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
      GO TO 785
  782 J2 = SWIRCP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
      GO TO 785
  783 J2 = SWIDCP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
      GO TO 785
  784 J2 = SWITCP(BUFFER(NNL1),BUFFER(NNL2),VARLEN(J3))
  785 CONTINUE
      IF(J2 .EQ. 0) GO TO 790
      IF((J2 .GT. 0 .AND. SORTYP(J3)) .OR.
     X   (J2 .LT. 0 .AND. .NOT. SORTYP(J3)))
     XGO TO 795
      GO TO 770
  790 CONTINUE
  795 CONTINUE
      LL(L0)=L1
  820 L0=L1
      L1=LL(L0)
      IF(L1.NE.L0) GO TO 750
      LL(L0)=L2
      GO TO 880
  870 LL(L0)=L1
  880 M1=M1-1
      IF(M1.GT.0) GO TO 700
      IF(M1.EQ.0) GO TO 350
C  GENERATE 2ND HALF OF A 4-TWIG
      M1=1-M1
      GO TO 630
      END
