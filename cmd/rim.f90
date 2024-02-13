PROGRAM MAIN
   !
   ! Rim MAIN PROGRAM
   !
   !  ****************************************************************
   !
   !  RIM IS AN IMPLEMENTATION OF THE RELATIONAL ALGEBRA MODEL
   !  OF DATA BASE MANAGEMENT.
   !
   !  *** COPYRIGHT 1988 BY ACADEMIC COMPUTING SERVICES ***
   !
   !  ****************************************************************
   !
   ! BY JIM FOX, UNIV OF WASHINGTON (ACS)
   !
   !  RIM IS A DESCENDANT OF THE RIM OF UW/BOEING/NASA FAME.
   !
   !------------------------------------------------------------


   USE Parameters
   USE Globals, only : KDBVER, CONNI, CONNO, BATCH, NOUTR
   USE DateTime, only: RMTIME, RMDATE
   USE Extern, only: SETOUT
   USE Rim, only: RMCONS, RMINIT, RIMCMD
   USE System, only: SystemExit, SystemTrap, SystemInitialise

   IMPLICIT NONE

   INCLUDE 'rmatts.inc'
   INCLUDE 'prom.inc'
   !
   CHARACTER(len=4) :: VER
   CHARACTER(len=8) :: VDATE
   INTEGER :: STAT
   INTEGER :: TDAY, TTIM
   !
   ! VERSION INFORMATION
   !
   DATA VER/'1.24'/
   DATA VDATE/'08/03/90'/
   KDBVER = 1240
   !
   ! ASCII DATA AND OTHER 'CONSTANTS'
   !
   CALL RMCONS
   CALL RMINIT
   !
   ! SYSTEM DEPENDENT STARTUP ROUTINE
   !
   ! (OPENS ZNINT FOR INPUT AND ZNOUT FOR OUTPUT)
   ! (MAY ALSO AFFECT BATCH, CONNI, CONNO, AND OTHER VARIABLES)
   !
   CALL SystemInitialise
   !
   !  PRINT THE RIM EXECUTION HEADER
   !
   TDAY = RMDATE()
   TTIM = RMTIME()
   ! CALL MSG(' ',' ',' ')
   ! CALL MSG(' F','UNIVERSITY OF ' ,'+')
   ! CALL MSG(' F','WASHINGTON',' ')
   ! CALL MSG(' F','RELATIONAL ','+')
   ! CALL MSG(' F','INFORMATION ','+')
   ! CALL MSG(' F','MANAGEMENT',' ')
   CALL MSG(' U','UW RIM ','+')
   CALL MSG(' U','(V.' // VER // '  ' // VDATE // ')',' ')
   ! CALL MSG(' ',' ',' ')
   !
   !
   !  SET THE TRAPS FOR RECOVERING ERRORS.
   !
   CALL SystemTrap('SET')
   !
   !  CALL COMMAND INTERPRETER
   !
   CALL RIMCMD
   !
   !  AND EXIT
   !
   IF(BATCH) GO TO 999
   IF(.NOT.CONNI) GO TO 999
   IF(.NOT.CONNO) CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
   !
   !  PRINT THE CLOSING MESSAGE
   !
999 CONTINUE
   TDAY = RMDATE()
   TTIM =  RMTIME()
   CALL MSG(' ','END RIM EXECUTION   ','+')
   CALL DMSG(TDAY,0,'+',KZDATE)
   CALL MSG(' ','  ','+')
   CALL DMSG(TTIM,0,' ',KZTIME)
   !
   CALL SystemTrap('CLEAR')
   !
   !  POSSIBLE SYSTEM DEPENDANT EXIT ROUTINE
   !
   CALL SystemExit
   !
   CALL EXIT(0)
END PROGRAM MAIN
