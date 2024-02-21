MODULE RM_Relations
   implicit none
   private

   public RELADD
   public RELGET
   public RELPUT
   public LOCREL

contains

   SUBROUTINE RELADD
      !!
      !!  PURPOSE:   ADD A NEW TUPLE TO THE RELTBL RELATION
      !!
      USE RM_Parameters
      USE RM_Globals, only : IFMOD
      USE Files, only : LF1REC
      USE Utils, only : ZMOVE

      INCLUDE 'tupler.inc'
      INCLUDE 'reltbl.inc'

      INTEGER :: MRSTRT, I
      !
      !  GET THE PAGE FOR ADDING NEW TUPLES.
      !
      MRSTRT = NRROW
      CALL RELPAG(MRSTRT)
      I = MRSTRT
      NRROW = NRROW + 1
      IF(I.EQ.RPBUF) NRROW = (RPBUF * LF1REC) + 1
      !
      !  MOVE THE DATA FROM THE TUPLE TO THE BUFFER.
      !
      RELTBL(ZR1,I) = NRROW
      CALL ZMOVE(RELTBL(ZR2,I),NAME)
      ! ! CALL ZMOVE(RELTBL(ZR3,I),RDATE)
      RELTBL(ZR3,I) = RDATE ! Copy an integer
      RELTBL(ZR4,I) = NCOL
      RELTBL(ZR5,I) = NATT
      RELTBL(ZR6,I) = NTUPLE
      RELTBL(ZR7,I) = RSTART
      RELTBL(ZR8,I) = REND
      CALL ZMOVE(RELTBL(ZR9,I),RPW)
      CALL ZMOVE(RELTBL(ZR10,I),MPW)
      RELMOD = 1
      IFMOD = .TRUE.
      LRROW = 0
      IF(I.LT.RPBUF) RETURN
      !
      !  WE JUST FILLED A BUFFER. MAKE SURE RELTBL GETS THE NEXT ONE.
      !
      RELBUF(1) = NRROW
      MRSTRT = NRROW
      CALL RELPAG(MRSTRT)
      RETURN
   END SUBROUTINE RELADD


   SUBROUTINE RELGET(STATUS)
      !!
      !!  PURPOSE:   GET THE NEXT TUPLE IN THE RELTBL RELATION
      !!
      !!  RM_Parameters:
      !!     STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INCLUDE 'reltbl.inc'
      INCLUDE 'tupler.inc'

      INTEGER, intent(out) :: STATUS

      INTEGER :: MRSTRT, I
      LOGICAL :: EQ
      STATUS = 0
      !
      !  SCAN FOR THE NEXT RELATION.
      !
      I = LRROW + 1
      GO TO 200
100   CONTINUE
      CALL RELPAG(MRSTRT)
      I = MRSTRT
200   CONTINUE
      IF(I.GT.RPBUF) GO TO 400
      IF(RELTBL(1,I).EQ.0) GO TO 9000
      IF(RELTBL(1,I).LT.0) GO TO 300
      IF(EQ(CNAME,BLANK)) GO TO 500
      IF(EQ(RELTBL(ZR2,I),CNAME)) GO TO 500
300   CONTINUE
      I = I + 1
      GO TO 200
      !
      !  GET THE NEXT PAGE.
      !
400   CONTINUE
      MRSTRT = RELBUF(1)
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 100
      !
      !  FOUND IT.
      !
500   CONTINUE
      LRROW = I
      CALL ZMOVE(NAME,RELTBL(ZR2,I))
      RDATE = RELTBL(ZR3,I)
      NCOL = RELTBL(ZR4,I)
      NATT = RELTBL(ZR5,I)
      NTUPLE = RELTBL(ZR6,I)
      RSTART = RELTBL(ZR7,I)
      REND = RELTBL(ZR8,I)
      CALL ZMOVE(RPW,RELTBL(ZR9,I))
      CALL ZMOVE(MPW,RELTBL(ZR10,I))
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      STATUS = 1
      LRROW = 0
9999  CONTINUE
      RETURN
   END SUBROUTINE RELGET


   SUBROUTINE RELPAG(THEROW)
      !!
      !!  PURPOSE:   DO PAGING AS NEEDED FOR THE RELTBL RELATION
      !!
      !!  RM_Parameters:
      !!     THEROW--INPUT - ROW WANTED
      !!             OUTPUT - ACTUAL ROW TO USE IN THE BUFFER

      USE RM_Parameters
      USE Files, only : FILE1, LENBF1, LF1REC, CRREC
      USE RM_Globals, only : RMSTAT
      USE RandomFiles, only: RIOIN, RIOOUT
      USE Utils, only : ZEROIT

      INCLUDE 'reltbl.inc'
      INTEGER, intent(in out) :: THEROW

      INTEGER :: NNREC, NNROW, IOS
      !
      !  TURN THE REQUESTED ROW INTO A RECORD AND OFFSET.
      !
      NNREC = ((THEROW - 1) / RPBUF) + 1
      NNROW = THEROW - ((NNREC - 1) * RPBUF)
      !
      !  SEE IF WE ALREADY HAVE THIS RECORD IN THE BUFFER.
      !
      IF(NNREC.EQ.CRREC) GO TO 300
      !
      !  WE MUST DO PAGING.
      !
      !  SEE IF THE CURRENT RECORD IN THE BUFFER HAS BEEN MODIFIED.
      !
      IF(RELMOD.EQ.0) GO TO 100
      !
      !  WRITE OUT THE CURRENT RECORD.
      !
      CALL RIOOUT(FILE1,CRREC,RELBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      !
      !  READ IN THE NEEDED RECORD.
      !
100   CONTINUE
      RELMOD = 0
      CALL RIOIN(FILE1,NNREC,RELBUF,LENBF1,IOS)
      IF(IOS.EQ.0) GO TO 200
      !
      !  THERE WAS NO DATA ON THE FILE - WRITE SOME.
      !
      CALL ZEROIT(RELBUF,LENBF1)
      CALL RIOOUT(FILE1,0,RELBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      LF1REC = LF1REC + 1
200   CONTINUE
      CRREC = NNREC
      !
      !  SET THE POINTER TO THE ACTUAL ROW IN THE BUFFER.
      !
300   CONTINUE
      THEROW = NNROW
      RETURN
   END SUBROUTINE RELPAG


   SUBROUTINE RELPUT
      !!
      !!  PURPOSE:   REPLACE THE CURRENT TUPLE FROM THE RELTBL RELATION
      !!         BASED ON CONDITIONS SET UP IN LOCREL
      !!

      USE RM_Parameters
      USE RM_Globals, only : IFMOD
      USE Utils, only : ZMOVE

      INCLUDE 'tupler.inc'
      INCLUDE 'reltbl.inc'

      IF(LRROW.EQ.0) GO TO 9999
      !
      !  MOVE THE STUFF TO ROW LRROW.
      !
      CALL ZMOVE(RELTBL(ZR2,LRROW),NAME)
      RELTBL(ZR3,LRROW) = RDATE
      RELTBL(ZR4,LRROW) = NCOL
      RELTBL(ZR5,LRROW) = NATT
      RELTBL(ZR6,LRROW) = NTUPLE
      RELTBL(ZR7,LRROW) = RSTART
      RELTBL(ZR8,LRROW) = REND
      CALL ZMOVE(RELTBL(ZR9,LRROW),RPW)
      CALL ZMOVE(RELTBL(ZR10,LRROW),MPW)
      RELMOD = 1
      IFMOD = .TRUE.
9999  CONTINUE
      RETURN
   END SUBROUTINE RELPUT


   INTEGER FUNCTION LOCREL(RNAME)
      !!
      !!  PURPOSE:   LOOK FOR A RELATION IN THE RELTBL RELATION
      !!
      !!  RM_Parameters:
      !!     RNAME---NAME OF RELATION OR BLANK
      !!     LOCREL--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE, NULLIT

      !INTEGER, intent(in) :: RNAME(Z)

      INCLUDE 'reltbl.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'rimptr.inc'
      LOGICAL :: EQ
      INCLUDE 'dclar1.inc'

      INTEGER :: MRSTRT, I

      LOCREL = 0
      !
      !  SCAN FOR THIS RELATION.
      !
      MRSTRT = ZRELRI
100   CONTINUE
      CALL RELPAG(MRSTRT)
      I = MRSTRT
200   CONTINUE
      IF(I.GT.RPBUF) GO TO 400
      IF(RELTBL(1,I).EQ.0) GO TO 9000
      IF(RELTBL(1,I).LT.0) GO TO 300
      IF(EQ(RNAME,BLANK)) GO TO 500
      !C    CALL MSG(' ','  LOCREL: ','+')
      !C    CALL AMSG(RNAME,ZC,'+')
      !C    CALL AMSG(RELTBL(ZR2,I),ZC,' ')
      IF(EQ(RELTBL(ZR2,I),RNAME)) GO TO 500
300   CONTINUE
      I = I + 1
      GO TO 200
      !
      !  GET THE NEXT PAGE.
      !
400   CONTINUE
      MRSTRT = RELBUF(1)
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 100
      !
      !  FOUND IT.
      !
500   CONTINUE
      LRROW = I - 1
      CALL ZMOVE(NAME,RELTBL(ZR2,I))
      RDATE = RELTBL(ZR3,I)
      NCOL = RELTBL(ZR4,I)
      NATT = RELTBL(ZR5,I)
      NTUPLE = RELTBL(ZR6,I)
      RSTART = RELTBL(ZR7,I)
      REND = RELTBL(ZR8,I)
      CALL ZMOVE(RPW,RELTBL(ZR9,I))
      CALL ZMOVE(MPW,RELTBL(ZR10,I))
      CALL ZMOVE(CNAME,RNAME)
      !
      !  ALSO SET THE VALUES IN THE RIMPTR COMMON BLOCK.
      !
      IVAL = 0
      LIMVAL = 0
      CID = RSTART
      NID = CID
      NS = 0
      MID = 0
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      LOCREL = 1
      LRROW = 0
      ! ! CALL ZMOVE(CNAME,NULL)
      CALL NULLIT(CNAME)
9999  CONTINUE
      RETURN
   END FUNCTION LOCREL

END MODULE RM_Relations
