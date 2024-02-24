MODULE RM_Attributes
   implicit none
   private

   public ATTADD
   public ATTDEL
   public ATTGET
   public ATTNEW
   public ATTPUT
   public LOCATT

CONTAINS

   SUBROUTINE ATTADD
      !!
      !!  PURPOSE:   ADD A NEW TUPLE TO THE ATTRIBUTE RELATION
      !!
      USE RM_Parameters
      USE Files, only : LF1REC
      USE RM_Globals, only : IFMOD
      USE Utils, only : ZMOVE

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'attble.inc'

      INTEGER :: MRSTRT, I, L

      !
      !  GET THE PAGE FOR ADDING NEW TUPLES.
      !
      MRSTRT = NAROW
      CALL ATTPAG(MRSTRT)
      I = MRSTRT
      NAROW = NAROW + 1
      IF(I.EQ.APBUF) NAROW = (APBUF * LF1REC) + 1
      !
      !  MOVE THE DATA FROM THE TUPLE TO THE BUFFER.
      !
      L = 1
      ATTBLE(1,I) = NAROW
      CALL ZMOVE(ATTBLE(ZA2,I),ATTNAM)
      CALL ZMOVE(ATTBLE(ZA3,I),RELNAM)
      ATTBLE(ZA4,I) = ATTCOL
      ATTBLE(ZA5,I) = ATTLEN
      ATTBLE(ZA6,I) = ATTYPE
      ATTBLE(ZA7,I) = ATTKEY
      ATTBLE(ZA8,I) = ATTFOR
      ATTMOD = 1
      IFMOD = .TRUE.
      CROW = 0
      LROW = 0
      IF(I.LT.APBUF) RETURN
      !
      !  WE JUST FILLED A BUFFER. MAKE SURE ATTBLE GETS THE NEXT ONE.
      !
      ATTBUF(1) = NAROW
      MRSTRT = NAROW
      CALL ATTPAG(MRSTRT)
      RETURN
   END SUBROUTINE ATTADD


   SUBROUTINE ATTDEL(STATUS)
      !!
      !!  PURPOSE:   DELETE THE CURRENT TUPLE FROM THE ATTRIBUTE RELATION
      !!         BASED ON CONDITIONS SET UP IN LOCATT AND ATTGET
      !!
      !!  RM_Parameters:
      !!     STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters

      INTEGER, intent(out) :: STATUS

      INCLUDE 'rmatts.inc'
      INCLUDE 'attble.inc'
      INCLUDE 'start.inc'
      !
      STATUS = 0
      IF(LROW.EQ.0) GO TO 9000
      !
      !  CHANGE THE TUPLE STATUS FLAG TO DELETED.
      !
      ATTBLE(1,LROW) = -ATTBLE(1,LROW)
      ATTMOD = 1
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      STATUS = 1
9999  CONTINUE
      RETURN
   END SUBROUTINE ATTDEL


   SUBROUTINE ATTGET(STATUS)
      !!
      !!  PURPOSE:   RETRIEVE THE NEXT TUPLE FROM THE ATTRIBUTE RELATION
      !!         BASED ON CONDITIONS SET UP IN LOCATT
      !!
      !!  RM_Parameters:
      !!     STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Text, only : BLANK
      USE Extern, only: IMSG, MSG
      USE Utils, only : ZMOVE, ITOH

      INTEGER, intent(out) :: STATUS

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'attble.inc'
      LOGICAL :: EQ
      LOGICAL :: NE

      INTEGER :: I, MRSTRT

      !
      STATUS = 0
      !CC   CALL MSG(' ','  ATTGET: (CROW,CRNAME,CANAME)','+')
      ! CALL IMSG(CROW,5,'+')
      ! CALL AMSG(CRNAME,ZC,' ')
      !CC   CALL AMSG(CANAME,ZC,' ')
      IF(CROW.EQ.0) GO TO 9000
      !
      !  SEE WHAT THE CALLER WANTS.
      !
      IF(EQ(CRNAME,BLANK)) GO TO 1000
      !
      !  CRNAME IS SPECIFIED.
      !
      I = CROW
      GO TO 200
100   CONTINUE
      CALL ATTPAG(MRSTRT)
      !
      !  LOOK FOR THE ATTRIBUTE IN THIS RELATION.
      !
      I = MRSTRT
200   CONTINUE
      IF(I.GT.APBUF) GO TO 300
      IF(NE(ATTBLE(ZA3,I),CRNAME)) GO TO 9000
      IF(EQ(CANAME,BLANK)) GO TO 2000
      IF(EQ(ATTBLE(ZA2,I),CANAME)) GO TO 2000
      I = I + 1
      GO TO 200
      !
      !  GET THE NEXT PAGE.
      !
300   CONTINUE
      MRSTRT = ATTBUF(1)
      !
      !  CHECK TO SEE THAT WE GET A DIFFERENT PAGE.
      !
      IF(MRSTRT.LE.IABS(ATTBUF(2))) GO TO 9000
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 100
      !
      !  SCAN FOR ATTRIBUTE WITHOUT RELATION SPECIFIED.
      !
1000  CONTINUE
      I = CROW
      GO TO 1200
1100  CONTINUE
      CALL ATTPAG(MRSTRT)
      I = MRSTRT
1200  CONTINUE
      IF(I.GT.APBUF) GO TO 1400
      IF(ATTBLE(1,I).LT.0) GO TO 1300
      IF(EQ(ATTBLE(ZA2,I),CANAME)) GO TO 2000
1300  CONTINUE
      I = I + 1
      GO TO 1200
      !
      !  GET THE NEXT PAGE.
      !
1400  CONTINUE
      MRSTRT = ATTBUF(1)
      !
      !  CHECK TO SEE THAT WE GET A DIFFERENT PAGE.
      !
      IF(MRSTRT.LE.IABS(ATTBUF(2))) GO TO 9000
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 1100
      !
      !  MOVE THE STUFF FROM ROW CROW.
      !
2000  CONTINUE
      CROW = I
      CALL ZMOVE(ATTNAM,ATTBLE(ZA2,CROW))
      CALL ZMOVE(RELNAM,ATTBLE(ZA3,CROW))
      ATTCOL = ATTBLE(ZA4,CROW)
      ATTLEN = ATTBLE(ZA5,CROW)
      ATTYPE = ATTBLE(ZA6,CROW)
      ATTKEY = ATTBLE(ZA7,CROW)
      ATTFOR = ATTBLE(ZA8,CROW)
      !
      !  UNPAC THE LENGTH DATA
      !
      CALL ITOH(ATTCHA,ATTWDS,ATTLEN)
      LROW = CROW
      CROW = CROW + 1
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      STATUS = 1
      CROW = 0
      LROW = 0
9999  CONTINUE
      RETURN
   END SUBROUTINE ATTGET


   SUBROUTINE ATTNEW(RNAME,NATT)
      !!
      !!  PURPOSE:   ADD A NEW RELATION TO THE ATTRIBUTE RELATION
      !!
      !!  RM_Parameters:
      !!     RNAME---NAME OF A RELATION
      !!     NATT----NUMBER OF ATTRIBUTES IN THE RELATION
      USE RM_Parameters
      USE Files, only : LF1REC

      INCLUDE 'rmatts.inc'
      INCLUDE 'attble.inc'
      INCLUDE 'start.inc'
      INCLUDE 'dclar1.inc'

      ! INTEGER, intent(in) :: RNAME(*) ! declared in dclar1.inc (?)
      INTEGER, intent(in) :: NATT

      INTEGER :: MRSTRT, I, KSFRIA
      !
      !  ADJUST NAROW IF ALL ATTRIBUTES WILL NOT FIT ON THE PAGE.
      !
      MRSTRT = NAROW
      CALL ATTPAG(MRSTRT)
      I = MRSTRT
      IF((I + NATT).LE.APBUF) GO TO 100
10    CONTINUE
      NAROW = (APBUF * LF1REC) + 1
      !
      !  CHECK TO SEE THAT WE WILL ACTUALLY POINT TO A NEW PAGE.
      !
      IF(NAROW.GT.IABS(ATTBUF(2))) GO TO 20
      !
      !  THIS IS STRANGE.
      !
      LF1REC = LF1REC + 1
      GO TO 10
20    CONTINUE
      ATTBUF(1) = NAROW
      ATTMOD = 1
      MRSTRT = NAROW
      CALL ATTPAG(MRSTRT)
100   CONTINUE
      IF(START.NE.KSFRIA) KSFRIA = START !! KSFRIA not defined
      RETURN
   END SUBROUTINE ATTNEW


   SUBROUTINE ATTPAG(THEROW)
      !!
      !!  PURPOSE:   DO PAGING AS NEEDED FOR THE ATTRIBUTE RELATION
      !!
      !!  RM_Parameters:
      !!     THEROW--INPUT - ROW WANTED
      !!             OUTPUT - ACTUAL ROW TO USE IN THE BUFFER

      USE RM_Parameters
      USE Files, only : FILE1, LENBF1, LF1REC, CAREC
      USE RM_Globals, only : RMSTAT
      USE RandomFiles, only : RIOIN, RIOOUT
      USE Utils, only : ZEROIT

      INCLUDE 'attble.inc'
      INTEGER, intent(in out) :: THEROW

      INTEGER :: NNREC, NNROW, IOS
      !
      !  TURN THE REQUESTED ROW INTO A RECORD AND OFFSET.
      !
      NNREC = ((THEROW - 1) / APBUF) + 1
      NNROW = THEROW - ((NNREC - 1) * APBUF)
      !
      !  SEE IF WE ALREADY HAVE THIS RECORD IN THE BUFFER.
      !
      IF(NNREC.EQ.CAREC) GO TO 300
      !
      !  WE MUST DO PAGING.
      !
      !  SEE IF THE CURRENT RECORD IN THE BUFFER HAS BEEN MODIFIED.
      !
      IF(ATTMOD.EQ.0) GO TO 100
      !
      !  WRITE OUT THE CURRENT RECORD.
      !
      CALL RIOOUT(FILE1,CAREC,ATTBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      !
      !  READ IN THE NEEDED RECORD.
      !
100   CONTINUE
      ATTMOD = 0
      CALL RIOIN(FILE1,NNREC,ATTBUF,LENBF1,IOS)
      IF(IOS.EQ.0) GO TO 200
      !
      !  THERE WAS NO DATA ON THE FILE - WRITE SOME.
      !
      CALL ZEROIT(ATTBUF,LENBF1)
      CALL RIOOUT(FILE1,0,ATTBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      LF1REC = LF1REC + 1
200   CONTINUE
      CAREC = NNREC
      !
      !  SET THE POINTER TO THE ACTUAL ROW IN THE BUFFER.
      !
300   CONTINUE
      THEROW = NNROW
      RETURN
   END SUBROUTINE ATTPAG


   SUBROUTINE ATTPUT(STATUS)
      !!
      !!  PURPOSE:   REPLACE THE CURRENT TUPLE FROM THE ATTRIBUTE RELATION
      !!         BASED ON CONDITIONS SET UP IN LOCATT AND ATTGET
      !!
      !!  RM_Parameters:
      !!     STATUS--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Globals, only : IFMOD
      USE Utils, only : ZMOVE

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'attble.inc'

      INTEGER, intent(out) :: STATUS
      !
      STATUS = 0
      IF(LROW.EQ.0) GO TO 9000
      !
      !  MOVE THE STUFF TO ROW LROW.
      !
      CALL ZMOVE(ATTBLE(ZA2,LROW),ATTNAM)
      CALL ZMOVE(ATTBLE(ZA3,LROW),RELNAM)
      ATTBLE(ZA4,LROW) = ATTCOL
      ATTBLE(ZA5,LROW) = ATTLEN
      ATTBLE(ZA6,LROW) = ATTYPE
      ATTBLE(ZA7,LROW) = ATTKEY
      ATTBLE(ZA8,LROW) = ATTFOR
      ATTMOD = 1
      IFMOD = .TRUE.
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      STATUS = 1
9999  CONTINUE
      RETURN
   END SUBROUTINE ATTPUT


   INTEGER FUNCTION LOCATT(ANAME,RNAME)
      !!
      !!  PURPOSE:   LOOK FOR ATTRIBUTES AND RELATIONS IN THE ATTRIBUTE
      !!         RELATION
      !!
      !!  RM_Parameters:
      !!     ANAME---NAME OF ATTRIBUTE OR BLANKS
      !!     RNAME---NAME OF RELATION OR BLANKS
      !!     LOCATT--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INCLUDE 'attble.inc'
      INCLUDE 'start.inc'
      LOGICAL :: EQ
      LOGICAL :: NE
      INCLUDE 'dclar1.inc'

      INTEGER :: MRSTRT, I

      LOCATT = 0
      !
      !  SEE WHAT THE CALLER WANTS.
      !
      IF(EQ(RNAME,BLANK)) GO TO 1000
      !
      !  RNAME IS SPECIFIED.
      !
      !
      !  FIND THE START FOR THIS RELATION.
      !
      !
      !  GET THE PAGE WITH THE DATA FOR THIS RELATION.
      !
      CALL ZMOVE(CRNAME,RNAME)
      MRSTRT = ZATTRI
200   CONTINUE
      CALL ATTPAG(MRSTRT)
      !
      !  LOOK FOR THE ATTRIBUTE IN THIS RELATION.
      !
      I = MRSTRT
300   CONTINUE
      IF(I.GT.APBUF) GO TO 400
      IF(ATTBLE(ZA1,I).LT.0) GO TO 350
      !C    CALL MSG(' ','  LOCATT: (REL): ','+')
      !C    CALL AMSG(RNAME,ZC,'+')
      !C    CALL AMSG(ATTBLE(ZA3,I),ZC,' ')
      IF(NE(ATTBLE(ZA3,I),RNAME)) GO TO 350
      IF(EQ(ANAME,BLANK)) GO TO 500
      !C    CALL MSG(' ','  LOCATT: (ATT): ','+')
      !C    CALL AMSG(ANAME,ZC,'+')
      !C    CALL AMSG(ATTBLE(ZA2,I),ZC,' ')
      IF(EQ(ATTBLE(ZA2,I),ANAME)) GO TO 500
350   CONTINUE
      IF(ATTBLE(ZA1,I).EQ.0) GO TO 400
      I = I + 1
      GO TO 300
      !
      !  GET THE NEXT PAGE.
      !
400   CONTINUE
      MRSTRT = ATTBUF(1)
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 200
      !
      !  WE FOUND THE ROW WE ARE LOOKING FOR.
      !
500   CONTINUE
      CALL ZMOVE(CANAME,ANAME)
      CROW = I
      LROW = 0
      GO TO 9999
      !
      !  SCAN FOR ATTRIBUTE WITHOUT RELATION SPECIFIED.
      !
1000  CONTINUE
      IF(EQ(ANAME,BLANK)) GO TO 9000
      MRSTRT = ZATTRI
1100  CONTINUE
      CALL ATTPAG(MRSTRT)
      I = MRSTRT
1200  CONTINUE
      IF(I.GT.APBUF) GO TO 1400
      IF(ATTBLE(ZA1,I).LT.0) GO TO 1300
      !C    CALL MSG(' ','  LOCATT: (NO RELATION): ','+')
      !C    CALL AMSG(ANAME,ZC,'+')
      !C    CALL AMSG(ATTBLE(ZA2,I),ZC,' ')
      IF(EQ(ATTBLE(ZA2,I),ANAME)) GO TO 1500
1300  CONTINUE
      IF(ATTBLE(ZA1,I).EQ.0) GO TO 1400
      I = I + 1
      GO TO 1200
      !
      !  GET THE NEXT PAGE.
      !
1400  CONTINUE
      MRSTRT = ATTBUF(1)
      IF(MRSTRT.EQ.0) GO TO 9000
      GO TO 1100
      !
      !  FOUND IT.
      !
1500  CONTINUE
      CALL ZMOVE(CRNAME,BLANK)
      CALL ZMOVE(CANAME,ANAME)
      CROW = I
      LROW = 0
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      CALL ZMOVE(CRNAME,BLANK)
      CALL ZMOVE(CANAME,BLANK)
      LOCATT = 1
      CROW = 0
      LROW = 0
9999  CONTINUE
      RETURN
   END FUNCTION LOCATT


END MODULE RM_Attributes
