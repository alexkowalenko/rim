      PROGRAM MAIN
C
C     Rim MAIN PROGRAM
C
C  ****************************************************************
C
C  RIM IS AN IMPLEMENTATION OF THE RELATIONAL ALGEBRA MODEL
C  OF DATA BASE MANAGEMENT.
C
C  *** COPYRIGHT 1988 BY ACADEMIC COMPUTING SERVICES ***
C
C  ****************************************************************
C
C     BY JIM FOX, UNIV OF WASHINGTON (ACS)
C
C  RIM IS A DESCENDANT OF THE RIM OF UW/BOEING/NASA FAME.
C
C------------------------------------------------------------

      USE DateTime, only: RMTIME, RMDATE
      USE System, only: SystemExit, SystemTrap, SystemInitialise

      INCLUDE 'syspar.inc'
      INCLUDE 'ascpar.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'flags.inc'
      INCLUDE 'files.inc'
      INCLUDE 'prom.inc'
C
      CHARACTER*4 VER
      CHARACTER*8 VDATE
C
C     VERSION INFORMATION
C
      DATA VER/'1.24'/
      DATA VDATE/'08/03/90'/
      KDBVER = 1240
C
C     ASCII DATA AND OTHER 'CONSTANTS'
C
      CALL RMCONS
      CALL RMINIT
C
C     SYSTEM DEPENDENT STARTUP ROUTINE
C
C     (OPENS ZNINT FOR INPUT AND ZNOUT FOR OUTPUT)
C     (MAY ALSO AFFECT BATCH, CONNI, CONNO, AND OTHER VARIABLES)
C
      CALL SystemInitialise
C
C  PRINT THE RIM EXECUTION HEADER
C
      TDAY = RMDATE()
      TTIM = RMTIME()
C     CALL MSG(' ',' ',' ')
C     CALL MSG(' F','UNIVERSITY OF ' ,'+')
C     CALL MSG(' F','WASHINGTON',' ')
C     CALL MSG(' F','RELATIONAL ','+')
C     CALL MSG(' F','INFORMATION ','+')
C     CALL MSG(' F','MANAGEMENT',' ')
      CALL MSG(' U','UW RIM ','+')
      CALL MSG(' U','(V.' // VER // '  ' // VDATE // ')',' ')
C     CALL MSG(' ',' ',' ')
C
C
C  SET THE TRAPS FOR RECOVERING ERRORS.
C
      CALL SystemTrap('SET')
C
C  CALL COMMAND INTERPRETER
C
      CALL RIMCMD
C
C  AND EXIT
C
      IF(BATCH) GO TO 999
      IF(.NOT.CONNI) GO TO 999
      IF(.NOT.CONNO) CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
C
C  PRINT THE CLOSING MESSAGE
C
  999 CONTINUE
      TDAY = RMDATE()
      TTIM =  RMTIME()
      CALL MSG(' ','END RIM EXECUTION   ','+')
      CALL DMSG(TDAY,0,'+',KZDATE)
      CALL MSG(' ','  ','+')
      CALL DMSG(TTIM,0,' ',KZTIME)
C
      CALL SystemTrap('CLEAR')
C
C  POSSIBLE SYSTEM DEPENDANT EXIT ROUTINE
C
      CALL SystemExit
C
      CALL EXIT(0)
      END
