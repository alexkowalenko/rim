MODULE RandomFiles
   implicit none
   private

   public RIOOPN
   public RIOCLO
   public RIOIN
   public RIOOUT

   INTEGER, private :: IRECPS(10)

contains

   SUBROUTINE RIOOPN(FNAME,FILE,NWDS,IOS)
      USE Parameters, only : znsrt1, znsrt2, ZNFIL1, ZCW
      !
      ! *** UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      !  PURPOSE:   COVER ROUTINE TO OPEN A RANDOM FILE
      !
      !  PARAMETERS
      !     FNAME---NAME OF THE FILE TO OPEN
      !     FILE----UNIT TO OPEN
      !     NWDS----NUMBER OF WORDS PER RECORD
      !     IOS-----STATUS VARIABLE - O MEANS SUCCESS, ELSE TILT
      !
      CHARACTER(len=*), intent(in) :: FNAME
      INTEGER, intent(in) :: FILE
      INTEGER, intent(in) :: NWDS
      INTEGER, intent(out) :: IOS

      INTEGER IUN

      ! Ignore names for scratch files
      if (file.eq.znsrt1 .or. file.eq.znsrt2) then
         OPEN  (UNIT=FILE, ACCESS='DIRECT', STATUS='SCRATCH', RECL=NWDS*ZCW, IOSTAT=IOS)
      else
         OPEN  (UNIT=FILE, FILE=FNAME, ACCESS='DIRECT', STATUS='UNKNOWN', RECL=NWDS*ZCW, IOSTAT=IOS)
      end if
      if (ios.ne.0) WRITE(6,1000) ios, file,fname
1000  FORMAT(' rioopn: ',i4,i4,2x,a)

      IUN = FILE - ZNFIL1 + 1
      IRECPS(IUN) = 1
      RETURN
   END SUBROUTINE RIOOPN


   SUBROUTINE RIOCLO(FILE)
      !
      ! ***UNIX SYSDEP ROUTINE
      !
      ! CLOSE A RANDOM ACCESS FILE
      !
      ! no action required
      INTEGER, intent(in) :: FILE
      RETURN
   END SUBROUTINE RIOCLO


   SUBROUTINE RIOIN(FILE,RECORD,BUFFER,NWDS,IOS)
      USE Parameters, only : ZNFIL1, Z
      !
      ! **UNIX SYSTEM DEPENDENT ROUTINE **
      !
      !  PURPOSE:   ROUTINE FOR RANDOM INPUT
      !
      !  PARAMETERS
      !     FILE----UNIT FOR IO
      !     RECORD--RECORD NUMBER WANTED
      !     BUFFER--BUFFER TO READ INTO
      !     NWDS----NUMBER OF WORDS PER BUFFER
      !     IOS-----STATUS VARIABLE - 0 MEANS SUCCESS, ELSE TILT
      !
      INTEGER, intent(in) :: FILE
      INTEGER, intent(in) :: RECORD
      INTEGER :: BUFFER(1)
      INTEGER, intent(in) :: NWDS
      INTEGER, intent(out) :: IOS

      INTEGER :: I, IUN

      INCLUDE 'flags.inc'

      READ(FILE,REC=RECORD,IOSTAT=IOS) (BUFFER(I),I=1,NWDS)
      IUN = FILE - ZNFIL1 + 1
      IRECPS(IUN) = RECORD
      IF (TRACE.GE.3) THEN
         CALL MSG('T','RIOIN: ','+')
         CALL IMSG(FILE,5,'+')
         CALL IMSG(RECORD,5,'+')
         CALL IMSG(NWDS,5,'+')
         CALL IMSG(IOS,4,' ')
      ENDIF
      RETURN
   END SUBROUTINE RIOIN


   SUBROUTINE RIOOUT(FILE,RECORD,BUFFER,NWDS,IOS)
      USE Parameters, only : ZNFIL1, Z
      !
      ! **UNIX SYSTEM DEPENDENT ROUTINE **
      !
      !  PURPOSE:  ROUTINE FOR RANDOM OUTPUT
      !
      !  PARAMETERS
      !     FILE----UNIT FOR WRITING
      !     RECORD--RECORD NUMBER
      !     BUFFER--BUFFER TO WRITE FROM
      !     NWDS----NUMBER OF WORDS PER BUFFER
      !     IOS-----STATUS VARIABLE - 0 MEANS SUCCESS, ELSE TILT
      !
      INCLUDE 'flags.inc'
      INTEGER, intent(in) :: FILE
      INTEGER, intent(in) :: RECORD
      INTEGER :: BUFFER(1)
      INTEGER, intent(in) :: NWDS
      INTEGER, intent(out) :: IOS

      INTEGER :: IUN, I, N, IPOS

      IUN = FILE - ZNFIL1 + 1
      IF(RECORD.NE.0) THEN
         WRITE(FILE, REC=RECORD, IOSTAT=IOS) (BUFFER(I),I=1,NWDS)
      ELSE
         N = IRECPS(IUN)
         WRITE(FILE, REC=N, IOSTAT=IOS) (BUFFER(I),I=1,NWDS)
      ENDIF
      IRECPS(IUN) = IRECPS(IUN) + 1
      IF (TRACE.GE.3) THEN
         CALL MSG('T','RIOOUT:','+')
         CALL IMSG(FILE,5,'+')
         CALL IMSG(IPOS,5,'+')
         CALL IMSG(NWDS,5,'+')
         CALL IMSG(IOS,4,'+')
         CALL IMSG(RECORD,5,' ')
      ENDIF
      RETURN
   END SUBROUTINE RIOOUT

END MODULE RandomFiles
