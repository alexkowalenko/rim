      SUBROUTINE RMLOOK(MAT,INDEX,IFLAG,LENGTH)

         USE RM_Globals, only : TRACE, HXFLAG, RMSTAT
         USE RM_BTree, only: BTLKI, BTLKR
         USE RM_BTree_Data, only : START
         USE RM_BufferData, only: BUFFER
         USE RM_Buffer, only: GETDAT
         USE Extern, only: IMSG, MSG
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C   LOCATE NEXT DESIRED TUPLE
C
C  RM_Parameters:
C         MAT-----ARRAY TO HOLD ONE TUPLE
C                 IF(IFLAG.NE.0) MAT IS POINTER TO TUPLE
C                 IN INPUT BUFFER.
C         INDEX---PAGE BUFFER TO USE
C         IFLAG---0 IFF TUPLE IS RETURNED
C                 ELSE POINTER TO TUPLE IS RETURNED IN MAT
C         LENGTH--LENGTH OF TUPLE IN WORDS
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
C
         DIMENSION MAT(1)
         LOGICAL EQTEST, WHEVAL
C
C  SCAN MAT.
C
         RMSTAT = 0
    1    CONTINUE
C
C  SEE IF WE ARE USING A KEY VALUE.
C
         IF(NS.EQ.0) GO TO 30
         IF(NS.EQ.3) GO TO 10
C
C  FIRST TIME THROUGH. USE BTLOOK TO FIND THE TUPLES.
C
         START = KSTRT
         IF (NBOO.LE.1) KBOOP = 1
         NUMP = KOMPOS(KBOOP)
         IF (KATTY(KBOOP).EQ.KZREAL .OR. KATTY(KBOOP).EQ.KZDOUB) THEN
            CALL BTLKR(WHRVAL(NUMP),NID,MID)
         ELSE
            CALL BTLKI(WHRVAL(NUMP),NID,MID)
         ENDIF
         NS = 3
         IF(NID.NE.0) GO TO 20
   10    CONTINUE
         IF(MID.EQ.0) GO TO 1300
         CALL MOTSCN(MID,NID)
         IF(NID.NE.0) GO TO 20
         GO TO 10
   20    CONTINUE
         CID = NID
         CALL GETDAT(INDEX,NID,ITUP,LENGTH)
         GO TO 40
   30    CONTINUE
         IF (HXFLAG.NE.0) GOTO 1300
         IF(NID.EQ.0) GO TO 1300
         CALL ITOH(N1,N2,NID)
         IF(N2.EQ.0) GO TO 1300
         CID = NID
         CALL GETDAT(INDEX,NID,ITUP,LENGTH)
         IF(NID.LT.0) GO TO 1300
C
C     EVALUATE THE WHERE CLAUSE WITH THIS TUPLE
C
   40    CONTINUE
         IVAL = IVAL + 1
         IF(NBOO.LE.0) GO TO 1200
         IF(IVAL.GT.MAXTU) GO TO 1300

         IF (.NOT.WHEVAL(ITUP)) GOTO 1
C
C  FOUND IT.
C
 1200    CONTINUE
         LIMVAL = LIMVAL + 1
         IF(LIMVAL.GT.LIMTU) GO TO 1300
         MAT(1) = ITUP
         IF (IFLAG.EQ.0) THEN
            IP = ITUP
            DO 1250 I=1,LENGTH
               MAT(I) = BUFFER(IP)
               IP = IP + 1
 1250       CONTINUE
         ENDIF
         RMSTAT = 0
         GOTO 9000
C
C  END OF DATA.
C
 1300    CONTINUE
         NS = 0
         RMSTAT = -1
C
C
 9000    IF (TRACE.GT.4) THEN
            CALL MSG('T','RMLOOK ','+')
            CALL IMSG(RMSTAT,5,'+')
            CALL ITOH(N1,N2,NID)
            CALL IMSG(N1,6,'+')
            CALL IMSG(N2,9,' ')
         ENDIF
         RETURN
      END
