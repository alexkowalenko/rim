MODULE RM_BTree
   implicit none
   private

   public BTADD
   public BTGET
   public BTINIT
   public BTLKI
   public BTLKR
   public BTPUT

contains

   SUBROUTINE BTADD(VALU,IPTR,TYPE)
      !!
      !!  PURPOSE:   ADD NEW VALUES TO A BTREE
      !!
      !!  RM_Parameters
      !!   INPUT:  VALU----KEY VALUE TO PROCESS
      !!        IPTR----POINTER TO TUPLE HAVING THIS KEY VALUE
      !!        TYPE----TYPE OF VARIABLE BEING ADDED
      !!
      !!  SUBROUTINES USED
      !!     BTGET---PAGING ROUTINE
      !!     BTSERT--USED TO INSERT VALUES IN A BTREE
      !!     BTPUT---PAGING ROUTINE
      !!
      USE RM_Parameters
      USE Files, only : LENBF3, MOTREC, MOTADD, LF3REC
      USE RM_Globals, only : RMSTAT
      USE Maths, only : DTOR
      USE Utils, only : HTOI, ITOH

      INTEGER, intent(in) :: VALU(Z)
      INTEGER, intent(in) :: IPTR, TYPE

      !
      !  PURPOSE:   ADD NEW VALUES TO A BTREE
      !
      !  RM_Parameters
      !    INPUT:  VALU----KEY VALUE TO PROCESS
      !        IPTR----POINTER TO TUPLE HAVING THIS KEY VALUE
      !        TYPE----TYPE OF VARIABLE BEING ADDED
      !
      !  SUBROUTINES USED
      !     BTGET---PAGING ROUTINE
      !     BTSERT--USED TO INSERT VALUES IN A BTREE
      !     BTPUT---PAGING ROUTINE
      !
      INCLUDE 'rmatts.inc'
      INCLUDE 'btbuf.inc'
      INCLUDE 'start.inc'
      INCLUDE 'stack.inc'
      !
      ! RVAL IS REAL UNTIL BTREE IS ALLOWED 2 WORDS PER ENTRY
      !
      INTEGER :: VAL,VALT, KSTART, ITYPE, IN, KEND, IP, J, IPTR1, KWORD, MOTIND, MOTID, IPT
      REAL :: RVAL
      EQUIVALENCE (RVAL,VAL)
      !
      !  INITIAL START OF THE SCAN.
      !
      SP = 0
      KSTART = START
      VAL = VALU(1)
      IF (TYPE.EQ.KZREAL) VAL = DTOR(VALU)
      ITYPE = TYPE
      IF(TYPE.EQ.KZDATE) ITYPE = KZINT
      IF(TYPE.EQ.KZTIME) ITYPE = KZINT
      IF(TYPE.EQ.KZTEXT) ITYPE = KZINT
      IP = IPTR
100   CONTINUE
      SP = SP + 1
      STACK(SP) = KSTART
      !
      !  FETCH A NODE.
      !
      CALL BTGET(KSTART,IN)
      KEND = IN + (LENBF3/3) - 1
      !
      !  LOOP THROUGH A NODE.
      !
      DO J=IN,KEND
         !
         !  CHECK FOR END-OF-LIST WORD.
         !
         IF(VALUE(1,J).EQ.ENDWRD) GO TO 200
         !
         !  IF THE VALUE IS LT VAL THEN KEEP LOOKING.
         !
         IF((ITYPE.EQ.KZINT).AND.(VALUE(1,J).LT.VAL)) GO TO 300
         IF((ITYPE.NE.KZINT).AND.(RVALUE(1,J).LT.RVAL)) GO TO 300
         !
         !  FOUND A BIGGER VALUE.
         !
200      CONTINUE
         !
         !  GO TO THE NEXT BRANCH IF THERE IS ONE.
         !
         IF(VALUE(2,J).GE.0) GO TO 400
         KSTART = -VALUE(2,J)
         GO TO 100
300      CONTINUE
      END DO
      !
      !  WE DID NOT FIND THE END-OF-LIST WORD. DISASTER.
      !
      GO TO 1000
      !
      !  ADD IT BETWEEN EXISTING VALUES.
      !
400   CONTINUE
      !
      !  CHECK FOR A DUPLICATE VALUE.
      !
      IF(VALUE(1,J).NE.VAL) GO TO 500
      !
      !  WE HAVE A MULTIPLE VALUE. SEE IF THIS IS THE FIRST DUPLICATE.
      !
      IF(VALUE(3,J).NE.0) GO TO 420
      !
      !  DO SPECIAL PROCESSING FOR THE FIRST MULTIPLE VALUE.
      !
      IPTR1 = VALUE(2,J)
      IF(MOTADD.LT.LENBF3) GO TO 410
      MOTADD = 0
      MOTREC = LF3REC
      LF3REC = LF3REC + 1
410   CONTINUE
      CALL HTOI(MOTADD+1,MOTREC,KWORD)
      VALUE(3,J) = KWORD
      VALUE(2,J) = KWORD
      CALL BTGET(STACK(SP),IN)
      CALL BTPUT(STACK(SP))
      !
      !  ADD THE FIRST LINK TO THE MOT.
      !
      CALL BTGET(MOTREC,IN)
      MOTIND = 3 * IN - 3
      MOTADD = MOTADD + 1
      MOTIND = MOTIND + MOTADD
      CORE(MOTIND+1) = IPTR1
      MOTADD = MOTADD + 1
      CALL BTPUT(MOTREC)
420   CONTINUE
      !
      !  FIX UP THE END POINTER.
      !
      IF(MOTADD.LT.LENBF3) GO TO 430
      MOTADD = 0
      MOTREC = LF3REC
      LF3REC = LF3REC + 1
430   CONTINUE
      CALL ITOH(MOTIND,MOTID,VALUE(2,J))
      CALL HTOI(MOTADD+1,MOTREC,VALUE(2,J))
      CALL BTGET(STACK(SP),IN)
      CALL BTPUT(STACK(SP))
      !
      !  GET THE END OF THE MOT TRAIL.
      !
      CALL BTGET(MOTID,IN)
      IN = 3 * IN - 3
      MOTIND = MOTIND + IN
      !
      !  ADD THE NEXT LINK IN THE MOT.
      !
      MOTADD = MOTADD + 1
      CALL HTOI(MOTADD,MOTREC,KWORD)
      CORE(MOTIND) = KWORD
      CALL BTPUT(MOTID)
      !
      !  NOW ADD THE POINTER TO THE MOT.
      !
      CALL BTGET(MOTREC,IN)
      IN = 3 * IN - 3
      MOTADD = MOTADD + 1
      MOTIND = IN + MOTADD
      CORE(MOTIND) = IPTR
      CALL BTPUT(MOTREC)
      RETURN
      !
      !  THIS VALUE IS NOT IN THE BTREE YET.
      !
500   CONTINUE
      !
      !  CALL BTSERT TO INSERT THE DATA.
      !
      VALT = VAL
      IPT = IP
600   CONTINUE
      CALL BTSERT(VALT,IPT,STACK,SP,J,IN)
      IF(SP.EQ.0) RETURN
      !
      !  FETCH THE NEXT NODE UP THE STACK.
      !
      CALL BTGET(STACK(SP),IN)
      !
      !  CALCULATE A NEW VALUE FOR J.
      !
      KEND = IN + (LENBF3/3) - 1
      DO J=IN,KEND
         IF(VALUE(1,J).EQ.ENDWRD) GO TO 600
         IF((ITYPE.EQ.KZINT).AND.(VALUE(1,J).LT.VAL)) GO TO 700
         IF((ITYPE.NE.KZINT).AND.(RVALUE(1,J).LT.RVAL)) GO TO 700
         !
         !  WE FOUND A BIGGER VALUE.
         !
         GO TO 600
700      CONTINUE
      END DO
      !
      !  SOMETHING IS WRONG. WE CANNOT FIND A LARGER VALUE.
      !
      RMSTAT = 1003
      RETURN
      !
      !  LOOKUP FOR A VALUE NOT IN THE TREE.
      !
1000  CONTINUE
      RETURN
   END SUBROUTINE BTADD


   SUBROUTINE BTGET(ID,NSTRT)
      !!
      !!  PURPOSE:    RETREIVE OR SET UP A BTREE OR MOT NODE.
      !!
      !!  RM_Parameters
      !! INPUT:   ID------DESIRED RECORD NUMBER
      !! OUTPUT:  NSTRT---BUFFER INDEX FOR REQUESTED NODE
      !!

      USE RM_Parameters
      USE Files, only: FILE3, LENBF3, LAST, NUMIC, MAXIC, ICORE
      USE Files, only: LF3REC
      USE RM_Globals, only : RMSTAT
      USE RandomFiles, only : RIOIN, RIOOUT
      USE Utils, only : ZEROIT

      INTEGER, intent(in) :: ID
      INTEGER, intent(out) :: NSTRT

      INCLUDE 'btbuf.inc'

      INTEGER :: NUMB, MINUMB, MINUSE, NUMUSE, ISTRT, IEND, IOBN, IOS

      !
      !  SEE IF THE BLOCK IS IN CORE.
      !
      DO NUMB=1,NUMIC
         IF(ID.EQ.ICORE(3,NUMB)) GO TO 1000
      END DO
      !
      !  THE REQUESTED BLOCK IS NOT IN THE BUFFER.
      !
      !   DETERMINE WHICH SLOT IN THE BUFFER WE SHOULD USE.
      !
      IF(NUMIC.GE.MAXIC) GO TO 200
      !
      !  STILL ROOM IN THE BUFFER.
      !
      NUMIC = NUMIC + 1
      NUMB = NUMIC
      GO TO 500
      !
      !  WE MUST DETERMINE WHO WILL BE MOVED OUT.
      !
200   CONTINUE
      MINUMB = 1
      IF(MINUMB.EQ.LAST) MINUMB = 2
      MINUSE = ICORE(1,MINUMB)
      DO NUMB=1,NUMIC
         IF(NUMB.EQ.LAST) GO TO 300
         NUMUSE = ICORE(1,NUMB)
         IF(NUMUSE.EQ.0) GO TO 400
         IF(NUMUSE.GT.MINUSE) GO TO 300
         MINUSE = NUMUSE
         MINUMB = NUMB
300      CONTINUE
      END DO
      !
      !  USE THE BLOCK THAT WAS USED THE LEAST.
      !
      NUMB = MINUMB
400   CONTINUE
      !
      !  BLOCK NUMB WILL BE USED.
      !
      !  SEE IF THE BLOCK CURRENTLY THERE MUST BE WRITTEN OUT.
      !
      IF(ICORE(2,NUMB).EQ.0) GO TO 500
      !
      !  WRITE IT OUT.
      !
      ISTRT = (NUMB-1) * LENBF3 + 1
      IEND = ISTRT + LENBF3 - 1
      IOBN = ICORE(3,NUMB)
      CALL RIOOUT(FILE3,IOBN,CORE(ISTRT),LENBF3,IOS)
      IF(IOS.NE.0) RMSTAT = 2300 + IOS
500   CONTINUE
      !
      !  CHANGE THE ICORE ENTRY.
      !
      ICORE(3,NUMB) = ID
      ICORE(2,NUMB) = 0
      !
      !  READ IN DESIRED BLOCK.
      !
      ISTRT = (NUMB-1) * LENBF3 + 1
      CALL RIOIN(FILE3,ID,CORE(ISTRT),LENBF3,IOS)
      IF(ID.GE.LF3REC) GO TO 600
      IF(IOS.EQ.0) GO TO 1000
600   CONTINUE
      CALL ZEROIT(CORE(ISTRT),LENBF3)
      CALL RIOOUT(FILE3,0,CORE(ISTRT),LENBF3,IOS)
      IF(IOS.NE.0) RMSTAT = 2300 + IOS
      !
      !  UPDATE THE ICORE ARRAY AND SET NSTRT.
      !
1000  CONTINUE
      ICORE(1,NUMB) = ICORE(1,NUMB) + 1
      ISTRT = ((NUMB-1) * LENBF3) / 3 + 1
      NSTRT = ISTRT
      LAST = NUMB
      RETURN
   END SUBROUTINE BTGET


   SUBROUTINE BTINIT(START)
      !!
      !!  PURPOSE:   INITIALIZE FOR A NEW BTREE
      !!
      !!  Parameters:
      !!     START---NEW RECORD USED FOR THIS BTREE
      !!

      USE RM_Parameters
      USE Files, only: LF3REC
      INCLUDE 'btbuf.inc'
      !
      INTEGER, intent(out) :: START

      INTEGER :: N1
      !
      !  GET THE NEXT NODE.
      !
      CALL BTGET(LF3REC,N1)
      !
      !  INSERT THE END-OF-LIST WORD.
      !
      VALUE(1,N1) = ENDWRD
      VALUE(2,N1) = 1
      VALUE(3,N1) = 0
      !
      !  WRITE OUT THIS NODE.
      !
      CALL BTPUT(LF3REC)
      START = LF3REC
      LF3REC = LF3REC + 1
      RETURN
   END SUBROUTINE BTINIT


   SUBROUTINE BTLKI(VAL,IPTR,MOTID)
      !!
      !!  PURPOSE:  LOOKUP PROCESSING ROUTINE FOR BTREES
      !!
      !!  RM_Parameters
      !!    INPUT:  VAL-----KEY VALUE TO PROCESS
      !!        IPTR----POINTER TO TUPLE HAVING THIS KEY VALUE
      !!        MOTID---MOT LINK
      !!
      !!  SUBROUTINES USED
      !!     BTGET---PAGING ROUTINE
      !!

      USE RM_Parameters
      USE Files, only : LENBF3

      INCLUDE 'btbuf.inc'
      INCLUDE 'start.inc'
      !
      INTEGER, intent(in) :: VAL
      INTEGER, intent(out) :: IPTR, MOTID

      INTEGER :: KSTART, KEND, J, IN

      !
      !  SET UP VARIABLES BASED ON THE ENTRY POINT.
      !
      !
      !  INITIAL START OF THE SCAN.
      !
      KSTART = START
100   CONTINUE
      !
      !  FETCH A NODE.
      !
      CALL BTGET(KSTART,IN)
      KEND = IN + (LENBF3/3) - 1
      !
      !  LOOP THROUGH A NODE.
      !
      DO J=IN,KEND
         !
         !  CHECK FOR END-OF-LIST WORD.
         !
         IF(VALUE(1,J).EQ.ENDWRD) GO TO 200
         !
         !  IF THE VALUE IS LT VAL THEN KEEP LOOKING.
         !
         IF(VALUE(1,J).LT.VAL) GO TO 300
         !
         !  FOUND A BIGGER VALUE.
         !
200      CONTINUE
         !
         !  GO TO THE NEXT BRANCH IF THERE IS ONE.
         !
         IF(VALUE(2,J).GE.0) GO TO 400
         KSTART = -VALUE(2,J)
         GO TO 100
300      CONTINUE
      END DO
      !
      !  WE DID NOT FIND THE END-OF-LIST WORD. DISASTER.
      !
      GO TO 500
      !
      !  DONE SCANNING THE BTREE.
      !
400   CONTINUE
      !
      !  CHECK FOR AN EQUAL VALUE.
      !
      IF(VALUE(1,J).NE.VAL) GO TO 500
      !
      !  PROCESS WAS A LOOKUP. RETURN THE TUPLE POINTER.
      !
      IPTR = VALUE(2,J)
      MOTID = VALUE(3,J)
      IF(MOTID.NE.0) CALL MOTSCN(MOTID,IPTR)
      RETURN
      !
      !  THIS VALUE IS NOT IN THE BTREE YET.
      !
500   CONTINUE
      IPTR = 0
      MOTID = 0
      RETURN
   END SUBROUTINE BTLKI


   SUBROUTINE BTLKR(VAL_I,IPTR,MOTID)
      !!
      !!  PURPOSE:  LOOKUP PROCESSING ROUTINE FOR BTREES
      !!
      !!  RM_Parameters
      !!    INPUT:  VAL-----KEY VALUE TO PROCESS
      !!        IPTR----POINTER TO TUPLE HAVING THIS KEY VALUE
      !!        MOTID---MOT LINK
      !!
      !!  SUBROUTINES USED
      !!     BTGET---PAGING ROUTINE
      !!
      USE RM_Parameters
      USE Files, only : LENBF3

      INCLUDE 'btbuf.inc'
      INCLUDE 'start.inc'
      !
      ! VAL IS REAL UNTIL BTREE IS ALLOWED 2 WORDS
      !
      INTEGER, intent(in) :: VAL_I
      INTEGER, intent(out) :: IPTR, MOTID

      INTEGER :: KSTART, KEND, J, IN
      REAL :: VAL

      VAL = transfer(VAL_I, VAL) ! transfer from INTEGER to REAL
      !
      !  SET UP VARIABLES BASED ON THE ENTRY POINT.
      !
      !
      !  INITIAL START OF THE SCAN.
      !
      KSTART = START
100   CONTINUE
      !
      !  FETCH A NODE.
      !
      CALL BTGET(KSTART,IN)
      KEND = IN + (LENBF3/3) - 1
      !
      !  LOOP THROUGH A NODE.
      !
      DO J=IN,KEND
         !
         !  CHECK FOR END-OF-LIST WORD.
         !
         IF(VALUE(1,J).EQ.ENDWRD) GO TO 200
         !
         !  IF THE VALUE IS LT VAL THEN KEEP LOOKING.
         !
         IF(RVALUE(1,J).LT.VAL) GO TO 300
         !
         !  FOUND A BIGGER VALUE.
         !
200      CONTINUE
         !
         !  GO TO THE NEXT BRANCH IF THERE IS ONE.
         !
         IF(VALUE(2,J).GE.0) GO TO 400
         KSTART = -VALUE(2,J)
         GO TO 100
300      CONTINUE
      END DO
      !
      !  WE DID NOT FIND THE END-OF-LIST WORD. DISASTER.
      !
      GO TO 500
      !
      !  DONE SCANNING THE BTREE.
      !
400   CONTINUE
      !
      !  CHECK FOR AN EQUAL VALUE.
      !
      IF(RVALUE(1,J).NE.VAL) GO TO 500
      !
      !  PROCESS WAS A LOOKUP. RETURN THE TUPLE POINTER.
      !
      IPTR = VALUE(2,J)
      MOTID = VALUE(3,J)
      IF(MOTID.NE.0) CALL MOTSCN(MOTID,IPTR)
      RETURN
      !
      !  THIS VALUE IS NOT IN THE BTREE YET.
      !
500   CONTINUE
      IPTR = 0
      MOTID = 0
      RETURN
   END SUBROUTINE BTLKR


   SUBROUTINE BTPUT(ID)
      !!
      !!  PURPOSE:    TURN ON THE WRITE FLAG ON THE INDICATED BLOCK
      !!
      !!  Parameters
      !!  INPUT:   ID------RECORD NUMBER
      !!
      !!  LOOK FOR THIS BLOCK IN CORE.
      !!
      USE Files, only : NUMIC, ICORE
      USE RM_Globals, only : IFMOD, RMSTAT

      INTEGER, intent(in) :: ID

      INTEGER :: NUMB

      DO NUMB=1,NUMIC
         IF(ID.EQ.ICORE(3,NUMB)) GO TO 200
      END DO
      !
      !  DISASTER. WE CANNOT FIND THE BLOCK.
      !
      RMSTAT = 1004
      RETURN
      !
      !  SET THE WRITE FLAG.
      !
200   CONTINUE
      ICORE(2,NUMB) = 1
      IFMOD = .TRUE.
      RETURN
   END SUBROUTINE BTPUT


   SUBROUTINE BTSERT(VAL,IP,STACK,SP,LOC,IN)
      !!
      !!  INSERT VAL INTO LOC REFERENCED BY THE STACK POINTER.
      !!
      !!  SUBROUTINES USED
      !!     BTGET---PAGING ROUTINE
      !!     BTPUT---PAGING ROUTINE
      !!     BTMOVE--MOVES DATA BETWEEN AREAS
      !!
      USE RM_Parameters
      USE Files, only : LENBF3, LF3REC, REUSE
      USE Utils, only : ZEROIT

      INTEGER :: VAL, IP, LOC, IN
      INTEGER :: STACK(1)

      INCLUDE 'btbuf.inc'
      INCLUDE 'start.inc'

      INTEGER :: VALT, SP
      INTEGER :: KEND, NV, J, IBT, IMT, N2, L, N1
      !
      KEND = IN + (LENBF3/3) - 1
      J = LOC
      !
      !  CHECK TO SEE IF THE NODE IS ALREADY FULL.
      !
      IF(VALUE(2,KEND).NE.0) GO TO 100
      !
      !  STILL ROOM.
      !
      NV = KEND - J
      CALL BTMOVE(KEND,KEND-1,-NV)
      VALUE(1,J) = VAL
      VALUE(2,J) = IP
      VALUE(3,J) = 0
      !
      !  WRITE OUT THIS NODE.
      !
      CALL BTPUT(STACK(SP))
      SP = 0
      RETURN
      !
      !  WE NEED TO SPLIT THE NODE. SAVE THE CURRENT LAST VALUE.
      !
100   CONTINUE
      VALT = VALUE(1,KEND)
      IBT = VALUE(2,KEND)
      IMT = VALUE(3,KEND)
      !
      !  PUT THE NEW VALUE IN ITS PLACE.
      !
      NV = KEND - J
      CALL BTMOVE(KEND,KEND-1,-NV)
      VALUE(1,J) = VAL
      VALUE(2,J) = IP
      VALUE(3,J) = 0
      !
      !  NEW VALUE IS IN
      !
      !  MOVE THE LOW PART
      !
      NV = 2 * (LENBF3/3) / 3
      CALL BTGET(LF3REC,N2)
      CALL BTMOVE(N2,IN,NV)
      !
      !  WRITE OUT THIS NEW NODE.
      !
      CALL BTPUT(LF3REC)
      L = N2 + NV - 1
      !
      !  SAVE IN A NEW NODE POINTER.
      !
      VAL = VALUE(1,L)
      IP = -LF3REC
      !
      !  MOVE THE TOP OF THE OLD NODE TO THE BOTTOM.
      !
      NV = (LENBF3/3) - NV
      CALL BTMOVE(IN,KEND-NV+1,NV)
      !
      !  RESTORE THE OLD LAST VALUE.
      !
      L = NV
      VALUE(1,IN+L) = VALT
      VALUE(2,IN+L) = IBT
      VALUE(3,IN+L) = IMT
      !
      !  ZERO OUT THE REMAINDER OF THE NODE.
      !
      NV = (LENBF3/3) - NV - 1
      IF(NV.LE.0) GO TO 300
      J = 3 * (KEND - IN - L)
      CALL ZEROIT(VALUE(1,IN+L+1),J)
300   CONTINUE
      !
      !  WRITE OUT THIS NODE AGAIN.
      !
      CALL BTPUT(STACK(SP))
      SP = SP - 1
      LF3REC = LF3REC + 1
      IF(SP.NE.0) RETURN
      !
      !  NEW STARTING NODE.
      !
      CALL BTGET(LF3REC,N1)
      VALUE(1,N1) = VAL
      VALUE(2,N1) = IP
      VALUE(3,N1) = 0
      VALUE(1,N1+1) = VALT
      VALUE(2,N1+1) = -STACK(1)
      VALUE(3,N1+1) = 0
      CALL REUSE
      !
      !  WRITE OUT THIS NEW NODE.
      !
      CALL BTPUT(LF3REC)
      START = LF3REC
      LF3REC = LF3REC + 1
      RETURN
   END SUBROUTINE BTSERT

END MODULE RM_BTREE
