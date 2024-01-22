      SUBROUTINE SWUNVL(BUFFER,CHAIN1,NCHAIN,LCHAIN,
     X                   INCH1,LREC,INFIL,OUTFIL)
      INCLUDE 'syspar.inc'
C
C  PURPOSE   MERGE ONE SET OF CHAINS INTO
C            SINGLE CHAIN OF SORTED TUPLES
C
C  METHOD    A STACK IS ESTABLISHED WITH
C            CURRENT FIRST TUPLE IN EACH
C            CHAIN.THE STACK IS IN ORDER.
C            THE FIRST TUPLE IS REMOVED
C            FROM THE STACK AND MOVED TO
C            OUTPUT BUFFER.THE NEXT TUPLE
C            IN THE PARTICULAR CHAIN IS
C            (IF ONE EXISTS) PUT ON TOP
C            OF STACK AND ALLOWED TO
C            SINK UNTIL IT IS IN SORT.
C            IF ONE DOES NOT EXIST,THE
C            STACK IS SHORTENED.WHEN
C            ONLY ONE CHAIN EXISTS,
C            ITS TAIL IS MOVED DIRECTLY
C            TO OUTPUT FILE
C  DEFINITION OF PARAMETERS
C
C  CHAIN1    RECORD NO ON INFILE WHICH CONTAINS   (INT,I)
C            PAGE 1 OF FIRST CHAIN
C
C  NCHAIN   NUMBER OF CHAINS TO MERGE           (INT,I)
C
C  LCHAIN    NUMBER OF PAGES PER INPUT CHAIN     (INT,I)
C
C  INCH1     CHAIN NUMBER OF FIRST INPUT CHAIN   (INT,I)
C
C  INFIL     FILE UNIT FOR INPUT
CC
C  OUTFIL    FILE UNIT FOR OUTPUT TUPLES
C
C  DEFINITION OF LOCAL VARIABLES
C
C  IP    IP(I)  CONTAINS POINTER TO IP1
C               FOR I:TH TUPLE IN STACK
C  IP1   IP1(I) CONTAINS POINTER TO CURRENT
C               TUPLE ON PAGE I
C  IP2   IP2(I) CONTAINS NUMBER OF TUPLES
C               ON PAGE I
C  IP3   IP3(I) CONTAINS RECORD NUMBER ON
C               INFILE FOR CURRENT PAGE IN
C               CHAIN I.NEG IF LAST PAGE IN CHAIN
C  IP4   IP4(I) CONTAINS POINTER TO FIRST
C               WORD ON PAGE I
C
C  IP5   IP5(I) CONTAINS SEQUENTIAL TUPLE NUMBER
C                OF CURRENT TUPLE PAGE I.
C
C  DEFINITION OF LOCAL VARIABLES
C
C  INCH    INPUT CHAIN NUMBER
C
      INTEGER BUFFER(1)
      INTEGER CHAIN1
      DIMENSION IP(10),IP1(10),IP2(10),IP3(10),IP4(10)
      DIMENSION IP5(10)
C
C  INITIALIZE,IE LOAD THE FIRST
C  BLOCKS OF THE INPUT CHAINS,SET
C  UP CONTROL ARRAYS IP,IP1,...,IP4
C
      REWIND OUTFIL
      I1 = CHAIN1
      I2 = 1
      INCH = INCH1
      DO 10 I=1,NCHAIN
C* READ RECORD I1 TO BUFFER I2,LENGTH= LREC
    1 CONTINUE
C
C     LOOK FOR CORRECT RECORD
C
      CALL RIOIN(INFIL,I1,BUFFER(I2),LREC,IOS)
      NUMCH = IABS(BUFFER(I2+1))
      IF(NUMCH.LT.INCH) GO TO 5
      IF(NUMCH.GT.INCH) GO TO 7
C
C     WE ARE IN THE CORRECT CHAIN
C
      INT = BUFFER(I2+2)
      IF(INT.EQ.1) GO TO 8
      I1 = I1 - INT + 1
      GO TO 1
    5 CONTINUE
C
C     IN SOME PREVIOUS CHAIN
C
      I1 = I1 + 1
      IF(BUFFER(I2+1).GT.0) I1 = I1 + 1
      GO TO 1
    7 CONTINUE
C
C     GOOD LORD - IN SOME SUBSEQUENT CHAIN
C
      I1 = I1 - BUFFER(I2+2)
      GO TO 1
    8 CONTINUE
C
C     FOUND THE FIRST RECORD IN CHAIN INCH
C
      IP1(I) = I2+4
      IP2(I) = BUFFER(I2)
      IP5(I) = 1
      IP3(I) = I1
      IF(BUFFER(I2+1) .LT. 0) IP3(I) = -IP3(I)
      IP(I) = I
      IP4(I) = I2
      I1 = I1 + LCHAIN
      I2 = I2 + LREC
      INCH = INCH + 1
   10 CONTINUE
      IF(NCHAIN.EQ.1) GO TO 18
      DO 15 I=2,NCHAIN
      CALL SWSINK(IP(NCHAIN-I+1),IP1(1),I,BUFFER)
   15 CONTINUE
   18 CONTINUE
      NIP = NCHAIN
C
C  INITIAL SETUP COMPLETE,
C  PREPARE FOR MERGE CYCLE
C
   25 CONTINUE
      I1 = IP(1)
      I2 = IP1(I1) - 2
      LTUP = BUFFER(I2+1) + 1
   27 CONTINUE
      WRITE(OUTFIL) (BUFFER(I2+K),K=1,LTUP)
      IP1(I1) = IP1(I1) + LTUP
      IP5(I1) = IP5(I1) + 1
      IF(IP5(I1) .LE. IP2(I1)) GO TO 50
C
C  INPUT BLOCK EMPTY
C
      IF(IP3(I1) .LT. 0) GO TO 40
      I2 = IP4(I1)
C*  READ BLOCK IP3(I1) TO BUFFER(I2)
      IP3(I1) = IP3(I1) + 1
      CALL RIOIN(INFIL,IP3(I1),BUFFER(I2),LREC,IOS)
      IP1(I1) =I2 + 4
      IP2(I1) = BUFFER(I2)
      IP5(I1) = 1
      IF(BUFFER(I2+1) .LT. 0) IP3(I1) = -IP3(I1)
      GO TO 50
   40 CONTINUE
C
C  CURRENT PAGE IS LAST PAGE IN CHAIN
C
      IF(NIP.EQ.1) GO TO 100
      DO 45 I=2,NIP
   45 IP(I-1) = IP(I)
      NIP = NIP - 1
      GO TO 25
   50 CONTINUE
C
C  CURRENT IP(1) TUPLE MOVED
C  PICK UP NEXT AND LET IT SINK
C
      IF(NIP.GT.1) CALL SWSINK(IP,IP1,NIP,BUFFER)
      GO TO 25
  100 CONTINUE
C
C     ALL DONE
C
      RETURN
      END
