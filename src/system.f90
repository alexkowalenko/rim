MODULE System

   USE, intrinsic :: iso_fortran_env

   USE Parameters

   IMPLICIT NONE

   private

   public SystemExit
   public SystemDelete
   public SystemCommand
   public SYSCOM
   public SystemTrap
   public SystemInitialise
   public SYSDBG
   public SYSDBN
   public CHKFIL

   ! Local defitions
   CHARACTER(ZFNAML), private :: DBDRF

contains

   SUBROUTINE SystemExit
      ! Final callback before exiting
      RETURN
   END SUBROUTINE SystemExit


   SUBROUTINE SystemDelete(FNAM)
      !
      ! **UNIX SYSTEM DEPENDENT ROUTINE
      !
      ! DELETE A FILE BY NAME
      !
      CHARACTER(len=*), intent(in) :: FNAM

      ! Not needed if rioopn creates scratch files
      RETURN
   END SUBROUTINE SystemDelete


   SUBROUTINE SystemCommand(CMD,ERR)
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! EXECUTE A SYSTEM (UNIX shell) COMMAND
      !
      CHARACTER(len=*), intent(in) :: CMD
      INTEGER, intent(out) :: ERR
      ! Fortran 2008
      call EXECUTE_COMMAND_LINE(cmd, exitstat=err)
      !
      RETURN
   END SUBROUTINE SystemCommand


   SUBROUTINE SYSCOM()
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! SYSTEM DEPENDENT COMMANDS
      !
      !CCCC CALL MSG(' ','SYSCOM',' ')
      RETURN
   END SUBROUTINE SYSCOM


   SUBROUTINE SystemTrap(mode)
      ! **UNIX SYSTEM DEPENDENT ROUTINE **

      ! CATCH SYSTEM INTERRUPTS (CTRL-C)

      character(len=*), intent(in) :: mode

      integer :: TTSTAT = 0

      ! THE MESSAGES SHOULD NEVER PRINT

      !  TTSTAT = signal(2,UNIXTRP,-1)
      IF (TTSTAT.lt.0) PRINT *, 'CTRL-C handling error ',TTSTAT
      RETURN
   END SUBROUTINE SystemTrap


   SUBROUTINE UNIXTRP
      USE Globals, only : HXFLAG

      ! **UNIX SYSTEM DEPENDENT ROUTINE **

      ! CATCH SYSTEM INTERRUPTS (CTRL-C) (PART 2)

      HXFLAG = 1
      RETURN
   END SUBROUTINE UNIXTRP


   SUBROUTINE SystemInitialise
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! SYSTEM INITIALIZE
      !
      INCLUDE 'files.inc'
      INCLUDE 'prom.inc'
      !
      ! GET FILENAME FROM COMMAND LINE
      !
      CHARACTER(ZFNAML) :: FNAME, home
      LOGICAL :: rw
      REAL(real64) :: d0
      INTEGER :: na, ERR, i, bp

      INTRINSIC command_argument_count, get_command_argument, get_environment_variable

      ! ignore any possible float overflows
      d0 = 0
      !---- call trpfpe(0,d0)

      na = command_argument_count()

      IF (na.gt.0) THEN
         ! INPUT IS FILE
         call get_command_argument(1,fname)
         OPEN(UNIT=ZNINT,FILE=FNAME,STATUS='OLD',IOSTAT=ERR)
         IF (ERR.NE.0) THEN
            CALL MSG(' ','COULD NOT OPEN FILE: ' // FNAME,' ')
            CALL EXIT(1)
         ENDIF
         BATCH = .TRUE.
         conni = .false.
         conno = .false.
         prmpt = .false.
      ENDIF

      ! Look for setup file ( ~/.rimrc )
      call get_environment_variable('HOME', home) ! Fortran 2003
      do i = 1, zfnaml
         bp = i
         if (home(i:i).eq.' ') exit
      end do
      home(bp:zfnaml) = '/.rimrc'
      IF (CHKFIL(home, rw))  call setin(home)
      RETURN
   END SUBROUTINE SystemInitialise


   SUBROUTINE SYSDBG(DBX,STATUS)
      USE Globals, only: DBFNAM
      USE Lexer, only : ASCREC, IDP, IDL, ITEMS
      USE Message, only: WARN
      USE Text, only: STRASC, ASCAN
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! SYSTEM DEP PROCESSING OF DATABASE
      ! (GET DIRECTORY AND FILENAME FROM COMMAND LINE)
      !
      ! DBX = POINTER IN TOKENS TO DATABASE NAME
      !
      INCLUDE 'tokens.inc'

      INTEGER, intent(in) :: DBX
      INTEGER, intent(out) :: STATUS

      INTEGER, PARAMETER :: RSBCH=93, COLCH=58
      INTEGER :: P

      STATUS = 0
      ! DBDIR = ' '
      IF (ITEMS.NE.DBX) GOTO 800

      ! note dbdrf contains both dir and name
      CALL STRASC(DBDRF,ASCREC(IDP(DBX)),IDL(DBX))

      ! Extract the actual filename from the input. ie remove dir

      ! look for '/'
      P = ASCAN(ASCREC,1,IDL(DBX),RSBCH,.TRUE.)
      IF (P.LE.0) P = ASCAN(ASCREC,1,IDL(DBX),COLCH,.TRUE.)
      DBFNAM = DBDRF(P+1:ZFNAML)

      RETURN
800   STATUS = 1
      CALL WARN(4)
      RETURN
   END SUBROUTINE SYSDBG


   SUBROUTINE SYSDBN(DBN,F1N,F2N,F3N,FXN)
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! BUILD FILENAMES FROM THE DATABASE NAME
      !
      !    DBN  =  Database name
      !    F1N  =  File 1 name
      !    F2N  =  File 2 name
      !    F3N  =  File 3 name
      !    FXN  =  Setup file name

      CHARACTER*(*), intent(in) :: DBN
      CHARACTER*(*), intent(out) :: F1N,F2N,F3N,FXN

      INCLUDE 'ascpar.inc'

      CHARACTER(ZFNAML) :: CDBN, xdbn
      INTEGER :: I, L, STATUS
      !
      ! Use name from DBDRF unless help DB open
      !
      xdbn = DBN

      ! IF (LIBFLG.EQ.0) THEN
      !    CDBN = DBDRF
      ! ELSE
      !    CDBN = '/usr/local/lib/' // xdbn
      ! ENDIF
      CDBN = DBDRF ! Always current directory

      DO I = 1,ZFNAML
         IF (CDBN(I:I).EQ.' ') GOTO 12
         L = I
      END DO
      L = ZFNAML
12    CONTINUE

      F1N = CDBN(1:L) // '.rimdb1'
      F2N = CDBN(1:L) // '.rimdb2'
      F3N = CDBN(1:L) // '.rimdb3'
      FXN = CDBN(1:L) // '.rim'

      STATUS = 0
      RETURN
   END SUBROUTINE SYSDBN


   LOGICAL FUNCTION CHKFIL(FNAME,RW)
      implicit none
      !
      ! **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
      !
      ! CHECK FOR A FILE'S EXISTANCE AND READ/WRITE PERMISSION
      !
      CHARACTER(len=*), intent(in) :: FNAME
      LOGICAL, intent(out) :: RW
      !
      inquire(FILE=fname,EXIST=rw)
      chkfil = rw
      RW = .TRUE.
      RETURN
   END FUNCTION CHKFIL

END MODULE System
