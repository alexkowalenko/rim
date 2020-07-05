      SUBROUTINE SYSINI
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     SYSTEM INITIALIZE
C
      INCLUDE 'files.d'
      INCLUDE 'prom.d'
C
C     GET FILENAME FROM COMMAND LINE
C
      CHARACTER*(ZFNAML) FNAME,HOME
      LOGICAL CHKFIL
      DOUBLE PRECISION D0
 
C     IGNORE ANY POSSIBLE FLOAT OVERFLOWS
C     D0 = 0
C     CALL TRPFPE(0,D0)
C
C     SET TO IGNORE INPUT ERROR 212 (SHORT RECORDS)
C                     AND ERROR 219 (MISSING FILES)
C                     AND SOME OTHER FILE ERRORS 
C
      CALL ERRSET(212,256,-1,1,1,1)
      CALL ERRSET(219,256,-1,1,1,1)
      CALL ERRSET(218,256,-1,1,1,1)
      CALL ERRSET(171,256,-1,1,1,1)
C

      NA = IARGC()
 
      IF (NA.GT.0) THEN
C        INPUT IS FILE
         CALL GETARG(1,FNAME)
         OPEN(UNIT=ZNINT,FILE=FNAME,STATUS='OLD',IOSTAT=ERR)
         IF (ERR.NE.0) THEN
            CALL MSG(' ','COULD NOT OPEN FILE: ' // FNAME,' ')
            CALL EXIT
         ENDIF
         BATCH = .TRUE.
         CONNI = .FALSE.
         CONNO = .FALSE.
         PRMPT = .FALSE.
      ENDIF
 
C     LOOK FOR SETUP FILE ( ~/.rimrc )
      CALL GETENV('HOME',HOME)
      DO 100 I = 1, ZFNAML
      BP = I
100   IF (HOME(I:I).EQ.' ') GOTO 110
110   HOME(BP:ZFNAML) = '/.rimrc'
      IF (CHKFIL(HOME,RW))  CALL SETIN(HOME)
      RETURN
      END
