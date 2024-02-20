MODULE RM_Links
   implicit none
   private

   public LNKADD
   public LNKGET
   public LNKPUT
   public LOCLNK

contains

   SUBROUTINE LNKADD
      !!
      !! ADD A NEW LINK ROW
      !!
      USE RM_Parameters
      USE Extern, only: MSG
      USE Files, only : LF1REC
      USE RM_Globals, only : IFMOD
      USE Utils, only : ZMOVE

      INCLUDE 'tuplel.inc'
      INCLUDE 'lnktbl.inc'

      INTEGER :: MRSTRT, I

      !
      !  GET THE PAGE FOR ADDING NEW TUPLES.
      !
      MRSTRT = NLROW
      IF (MRSTRT.EQ.0) THEN
         CALL MSG('E','MUST RELOAD DB TO USE LINKS',' ')
         RETURN
      ENDIF
      CALL LNKPAG(MRSTRT)
      I = MRSTRT
      NLROW = NLROW + 1
      IF(I.EQ.LPBUF) NLROW = (LPBUF * LF1REC) + 1
      !
      !  MOVE THE DATA FROM THE TUPLE TO THE BUFFER.
      !
      LNKTBL(ZL1,I) = NLROW
      CALL ZMOVE(LNKTBL(ZL2,I),LNAME)
      CALL ZMOVE(LNKTBL(ZL3,I),R1NAME)
      CALL ZMOVE(LNKTBL(ZL4,I),A1NAME)
      CALL ZMOVE(LNKTBL(ZL5,I),R2NAME)
      CALL ZMOVE(LNKTBL(ZL6,I),A2NAME)
      LNKMOD = 1
      IFMOD = .TRUE.
      LLROW = 0
      IF(I.LT.LPBUF) RETURN
      !
      !  WE JUST FILLED A BUFFER. MAKE SURE LNKTBL GETS THE NEXT ONE.
      !
      LNKBUF(1) = NLROW
      MRSTRT = NLROW
      CALL LNKPAG(MRSTRT)
      RETURN
   END SUBROUTINE LNKADD


   SUBROUTINE LNKGET(STATUS)
      !!
      !! GET THE NEXT LINK ROW
      !!
      !! RM_Parameters:
      !!     STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE Utils, only : ZMOVE

      INTEGER, intent(out) :: STATUS

      INCLUDE 'lnktbl.inc'
      INCLUDE 'tuplel.inc'

      INTEGER :: I, MRSTRT
      LOGICAL :: EQ
      STATUS = 0
      !
      !  SCAN FOR THE NEXT LINK
      !
      I = LLROW + 1
      GO TO 200
      !
100   CALL LNKPAG(MRSTRT)
      I = MRSTRT
      !
200   IF(I.GT.LPBUF) GO TO 400
      IF(LNKTBL(1,I).EQ.0) GO TO 9000
      IF(LNKTBL(1,I).LT.0) GO TO 300
      GOTO 500
      ! IF(EQ(LNAME,BLANK)) GO TO 500
      ! IF(EQ(LNKTBL(ZR2,I),LNAME)) GO TO 500
300   I = I + 1
      GO TO 200
      !
      !  GET THE NEXT PAGE.
      !
400   MRSTRT = LNKBUF(1)
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 100
      !
      !  FOUND IT.
      !
500   LLROW = I
      CALL ZMOVE(LNAME,LNKTBL(ZL2,I))
      CALL ZMOVE(R1NAME,LNKTBL(ZL3,I))
      CALL ZMOVE(A1NAME,LNKTBL(ZL4,I))
      CALL ZMOVE(R2NAME,LNKTBL(ZL5,I))
      CALL ZMOVE(A2NAME,LNKTBL(ZL6,I))
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      STATUS = 1
      LLROW = 0
9999  CONTINUE
      RETURN
   END SUBROUTINE LNKGET


   SUBROUTINE LNKPAG(THEROW)
      !!
      !! DO PAGING AS NEEDED FOR THE LINK HEADER PAGES
      !!
      !! RM_Parameters:
      !!     THEROW--INPUT - ROW WANTED
      !!             OUTPUT - ACTUAL ROW TO USE IN THE BUFFER

      USE RM_Parameters
      USE Files, only : FILE1,LENBF1,LF1REC, CLREC
      USE RM_Globals, only : RMSTAT
      USE RandomFiles, only: RIOIN, RIOOUT
      USE Utils, only : ZEROIT

      INTEGER, intent(in out) :: THEROW

      INCLUDE 'lnktbl.inc'

      INTEGER :: NNREC, NNROW, IOS, RELMOD
      !
      !  TURN THE REQUESTED ROW INTO A RECORD AND OFFSET.
      !
      NNREC = ((THEROW - 1) / LPBUF) + 1
      NNROW = THEROW - ((NNREC - 1) * LPBUF)
      !
      !  SEE IF WE ALREADY HAVE THIS RECORD IN THE BUFFER.
      !
      IF(NNREC.EQ.CLREC) GO TO 300
      !
      !  WE MUST DO PAGING.
      !
      !  SEE IF THE CURRENT RECORD IN THE BUFFER HAS BEEN MODIFIED.
      !
      IF(LNKMOD.EQ.0) GO TO 100
      !
      !  WRITE OUT THE CURRENT RECORD.
      !
      CALL RIOOUT(FILE1,CLREC,LNKBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      !
      !  READ IN THE NEEDED RECORD.
      !
100   RELMOD = 0
      CALL RIOIN(FILE1,NNREC,LNKBUF,LENBF1,IOS)
      IF(IOS.EQ.0) GO TO 200
      !
      !  THERE WAS NO DATA ON THE FILE - WRITE SOME.
      !
      CALL ZEROIT(LNKBUF,LENBF1)
      CALL RIOOUT(FILE1,0,LNKBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      LF1REC = LF1REC + 1
200   CONTINUE
      CLREC = NNREC
      !
      !  SET THE POINTER TO THE ACTUAL ROW IN THE BUFFER.
      !
300   CONTINUE
      THEROW = NNROW
      RETURN
   END SUBROUTINE LNKPAG


   SUBROUTINE LNKPUT(STATUS)
      !!
      !! REPLACE THE CURRENT LINK TUPLE INTO /LNKTBL/
      !!
      !!     STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Globals, only : IFMOD
      USE Utils, only : ZMOVE

      INTEGER, intent(out) :: STATUS

      INCLUDE 'tuplel.inc'
      INCLUDE 'lnktbl.inc'
      !
      STATUS = 0
      IF(LLROW.EQ.0) GO TO 9000
      !
      !  MOVE THE STUFF TO ROW LLROW.
      !
      CALL ZMOVE(LNKTBL(ZL2,LLROW),LNAME)
      CALL ZMOVE(LNKTBL(ZL3,LLROW),R1NAME)
      CALL ZMOVE(LNKTBL(ZL4,LLROW),A1NAME)
      CALL ZMOVE(LNKTBL(ZL5,LLROW),R2NAME)
      CALL ZMOVE(LNKTBL(ZL6,LLROW),A2NAME)
      LNKMOD = 1
      IFMOD = .TRUE.
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      STATUS = 1
9999  CONTINUE
      RETURN
   END SUBROUTINE LNKPUT


   INTEGER FUNCTION LOCLNK(LKNAME)
      !!
      !! LOOK FOR A LINK IN THE LNKTBL RELATION
      !!
      !! RM_Parameters:
      !!     LKNAME---NAME OF RELATION OR BLANK
      !!     LOCLNK--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE, NULLIT

      INCLUDE 'lnktbl.inc'
      INCLUDE 'tuplel.inc'
      INCLUDE 'rimptr.inc'

      INTEGER, intent(in) :: LKNAME(Z)

      INTEGER :: MRSTRT, I
      LOGICAL :: EQ
      LOCLNK = 0
      !
      !  SCAN FOR THE LINK
      !
      MRSTRT = ZLNKRI
      IF (MRSTRT.EQ.0) GOTO 9000
100   CALL LNKPAG(MRSTRT)
      I = MRSTRT

200   IF(I.GT.LPBUF) GO TO 400
      IF(LNKTBL(1,I).EQ.0) GO TO 9000
      IF(LNKTBL(1,I).LT.0) GO TO 300
      !C    CALL MSG(' ','  LOCLNK: ','+')
      !C    CALL AMSG(LKNAME,ZC,'+')
      !C    CALL AMSG(LNKTBL(ZL2,I),ZC,' ')
      IF(EQ(LKNAME,BLANK)) GO TO 500
      IF(EQ(LNKTBL(ZL2,I),LKNAME)) GO TO 500
300   CONTINUE
      I = I + 1
      GO TO 200
      !
      !  GET THE NEXT PAGE.
      !
400   MRSTRT = LNKBUF(1)
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 100
      !
      !  FOUND IT.
      !
500   LLROW = I - 1
      CALL ZMOVE(LNAME,LNKTBL(ZL2,I))
      CALL ZMOVE(R1NAME,LNKTBL(ZL3,I))
      CALL ZMOVE(A1NAME,LNKTBL(ZL4,I))
      CALL ZMOVE(R2NAME,LNKTBL(ZL5,I))
      CALL ZMOVE(A2NAME,LNKTBL(ZL6,I))
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      LOCLNK = 1
      LLROW = 0
      ! ! CALL ZMOVE(LNAME,NULL)
      CALL NULLIT(LNAME)
9999  CONTINUE
      RETURN
   END FUNCTION LOCLNK


END MODULE RM_Links
