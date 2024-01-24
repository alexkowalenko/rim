PROGRAM MAIN

  USE System, only: SystemExit, SystemInitialise

  INCLUDE 'syspar.inc'
  !
  ! TEST LXL ROUTINES
  !
  INCLUDE 'tokens.inc'
  INCLUDE 'cards.inc'
  INCLUDE 'files.inc'
  INCLUDE 'rmatts.inc'
  !
  LOGICAL :: EQKEYW
  !
  CHARACTER(len=1) :: FC

  CALL RMCONS
  CALL RMINIT
  CALL LXINIT
  CALL SystemInitialise
  ! DON'T USE THE INIT FILE
  IF (NINT.NE.ZNINT) CALL SETIN(ZTRMIN)
  ECHO = .TRUE.
  READCD = 0
  !
10   CALL LODREC
  IF (KWS(1).EQ.'END') GOTO 900
  !
  CALL TOKDSP
  !
  IF (EQKEYW(1,'MACRO')) THEN
     CALL MACDEF(*10)
  ENDIF


  IF (EQKEYW(1,'FORMAT')) THEN
     FC = KWS(2)(1:1)
     IF (FC.EQ.'A') TYP = KZTEXT
     IF (FC.EQ.'I') TYP = KZINT
     IF (FC.EQ.'F') TYP = KZREAL
     IF (FC.EQ.'D') TYP = KZDATE
     IF (FC.EQ.'T') TYP = KZTIME
     CALL LXFMT(3,TYP,FMT,LEN)
     CALL MSG(' ','FORMAT = ','+')
     CALL IMSG(FMT,10,'+')
     CALL MSG(' ',', LENGTH = ','+')
     CALL IMSG(LEN,5,' ')
  ENDIF
  GOTO 10
  !
900   CALL SystemExit
  CALL EXIT(0)
END PROGRAM MAIN
