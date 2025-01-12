      SUBROUTINE SWSINK(IP,IIP,NIP,BUFFER)
         INCLUDE 'syspar.inc'
C
C  PURPOSE   TO INSERT A TUPLE INTO A SEQUENCE
C            OF SORTED TUPLES USING A SINK
C            SORT.  THE TOP TUPLE IS MOVED DOWN
C            IN THE EXISTING SEQUENCE UNTIL IT
C            IS NOT LESS THAN THE NEXT TUPLE
C            (IF ASCENDING SORT) OR NOT GREATER
C            THAN THE NEXT TUPLE (DESCENDING SORT)
C
C  DEFININITION OF VARIABLES
C
C  IP        VECTOR OF INDIRECT POINTERS          (INT,I/O)
C            IP(I) POINTS TO IIP.
C            IP(2), ... , IP(NIP) ARE
C            IN SORT UPON ENTRY. UPON
C            EXIT IP(1), ... ,IP(NIP)
C            ARE IN SORT
C
C  IIP       VECTOR OF CURRENT POINTERS           (INT,I)
C            TO BUFFER
C
C  NIP       NUMBER OF CURRENT CHAINS             (INT,I)
C            ** NOTICE **   NIP MUST BE GT 1
C
C  BUFFER     VECTOR CONTAINING TUPLES TO SORT    (ANY,I)
C             IIP POINTERS ARE RELATIVE TO
C             BUFFER(1)
C
         INCLUDE 'srtcom.inc'
         INTEGER SWIICP,SWIDCP,SWIRCP,SWITCP,SWVACP
         DIMENSION IP(1),IIP(1)
         INTEGER, intent(in) :: NIP
         DIMENSION BUFFER(1)
         J1 = IP(1)
         I1 = IIP(J1)
         DO I=2,NIP
            J3 = IP(I)
            I2 = IIP(J3)
            DO 20 J4=1,NSOVAR
               JJ4 = VARPOS(J4) + OFFSET - 1
               IF (VARLEN(J4).NE.0) GO TO 10
               J2 = SWVACP(BUFFER(I1),BUFFER(I2),J4)
               GO TO 15
   10          CONTINUE
               KGOTO = VARTYP(J4)
               GO TO (11,12,13,14),KGOTO
   11          J2 = SWIICP(BUFFER(I1+JJ4),BUFFER(I2+JJ4),VARLEN(J4))
               GO TO 15
   12          J2 = SWIRCP(BUFFER(I1+JJ4),BUFFER(I2+JJ4),VARLEN(J4))
               GO TO 15
   13          J2 = SWIDCP(BUFFER(I1+JJ4),BUFFER(I2+JJ4),VARLEN(J4))
               GO TO 15
   14          J2 = SWITCP(BUFFER(I1+JJ4),BUFFER(I2+JJ4),VARLEN(J4))
   15          CONTINUE
               IF(J2 .EQ. 0) GO TO 20
               IF((J2 .GT. 0 .AND. SORTYP(J4)) .OR.
     X            (J2 .LT. 0 .AND. .NOT. SORTYP(J4)))
     X             GO TO 200
               GO TO 30
   20       CONTINUE
C
C    EQUAL,PRESERVE ORIGINAL ORDER
C
            IF(J1 .LT. J3) GO TO 200
   30       CONTINUE
C
C     NOT IN SORT, CONTINUE TO SINK
C
            IP(I-1) = J3
            IP(I) = J1
         END DO
  200    RETURN
      END
