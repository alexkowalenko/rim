      SUBROUTINE BTSERT(VAL,IP,STACK,SP,LOC,IN)

         USE Files, only : LENBF3, LAST, LF3REC, REUSE
         USE Utils, only : ZEROIT

         INCLUDE 'syspar.inc'
C
C  INSERT VAL INTO LOC REFERENCED BY THE STACK POINTER.
C
C  SUBROUTINES USED
C         BTGET---PAGING ROUTINE
C         BTPUT---PAGING ROUTINE
C         BTMOVE--MOVES DATA BETWEEN AREAS
C
         INCLUDE 'btbuf.inc'
         INCLUDE 'start.inc'
         INTEGER VALT
         INTEGER VAL,STACK(1),SP
C
         KEND = IN + (LENBF3/3) - 1
         J = LOC
C
C  CHECK TO SEE IF THE NODE IS ALREADY FULL.
C
         IF(VALUE(2,KEND).NE.0) GO TO 100
C
C  STILL ROOM.
C
         NV = KEND - J
         CALL BTMOVE(KEND,KEND-1,-NV)
         VALUE(1,J) = VAL
         VALUE(2,J) = IP
         VALUE(3,J) = 0
C
C  WRITE OUT THIS NODE.
C
         CALL BTPUT(STACK(SP))
         SP = 0
         RETURN
C
C  WE NEED TO SPLIT THE NODE. SAVE THE CURRENT LAST VALUE.
C
  100    CONTINUE
         VALT = VALUE(1,KEND)
         IBT = VALUE(2,KEND)
         IMT = VALUE(3,KEND)
C
C  PUT THE NEW VALUE IN ITS PLACE.
C
         NV = KEND - J
         CALL BTMOVE(KEND,KEND-1,-NV)
         VALUE(1,J) = VAL
         VALUE(2,J) = IP
         VALUE(3,J) = 0
C
C  NEW VALUE IS IN
C
C  MOVE THE LOW PART
C
         NV = 2 * (LENBF3/3) / 3
         CALL BTGET(LF3REC,N2)
         CALL BTMOVE(N2,IN,NV)
C
C  WRITE OUT THIS NEW NODE.
C
         CALL BTPUT(LF3REC)
         L = N2 + NV - 1
C
C  SAVE IN A NEW NODE POINTER.
C
         VAL = VALUE(1,L)
         IP = -LF3REC
C
C  MOVE THE TOP OF THE OLD NODE TO THE BOTTOM.
C
         NV = (LENBF3/3) - NV
         CALL BTMOVE(IN,KEND-NV+1,NV)
C
C  RESTORE THE OLD LAST VALUE.
C
         L = NV
         VALUE(1,IN+L) = VALT
         VALUE(2,IN+L) = IBT
         VALUE(3,IN+L) = IMT
C
C  ZERO OUT THE REMAINDER OF THE NODE.
C
         NV = (LENBF3/3) - NV - 1
         IF(NV.LE.0) GO TO 300
         J = 3 * (KEND - IN - L)
         CALL ZEROIT(VALUE(1,IN+L+1),J)
  300    CONTINUE
C
C  WRITE OUT THIS NODE AGAIN.
C
         CALL BTPUT(STACK(SP))
         SP = SP - 1
         LF3REC = LF3REC + 1
         IF(SP.NE.0) RETURN
C
C  NEW STARTING NODE.
C
         CALL BTGET(LF3REC,N1)
         VALUE(1,N1) = VAL
         VALUE(2,N1) = IP
         VALUE(3,N1) = 0
         VALUE(1,N1+1) = VALT
         VALUE(2,N1+1) = -STACK(1)
         VALUE(3,N1+1) = 0
         CALL REUSE
C
C  WRITE OUT THIS NEW NODE.
C
         CALL BTPUT(LF3REC)
         START = LF3REC
         LF3REC = LF3REC + 1
         RETURN
      END
