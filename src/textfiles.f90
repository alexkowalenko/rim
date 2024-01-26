MODULE TextFiles
   implicit none
   private

   public TIOOPN
   public TIOCLO
   public TIOIN
   public TIOOUT

CONTAINS

   SUBROUTINE TIOOPN(FILE,UNIT,MODE,ERR)
      INCLUDE 'syspar.inc'
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! OPEN FILE FOR TEXT IO
      !     FILE -- FILE NAME
      !     UNIT -- UNIT NUMBER
      !     MODE -- 'INPUT' OR 'OUTPUT'
      !     ERR  -- = 0 IF OPEN OK
      !
      CHARACTER(len=*), intent(in) :: FILE
      CHARACTER(len=*), intent(in) :: MODE
      INTEGER, intent(in) :: UNIT
      INTEGER, intent(out) :: ERR
      !
      INCLUDE 'flags.inc'

      character(zfnaml) ffile
      LOGICAL :: EXI
      !
      ERR = 0
      ffile = file
      !
      ! Because unix lends less importance to extensions the
      ! default extensions are NOT added.  If they were,
      ! this is how it would be done.
      !
      !c      IF (FILE.NE.ztrmin) THEN
      !c        L = 0
      !c        DO 50 I = ZFNAML, 1, -1
      !c        IF (FFILE(I:I).EQ.'/') goto 55
      !c        IF (FFILE(I:I).EQ.' ' .OR. FFILE(I:I).EQ.'.') THEN
      !c           IF (L.NE.0) GOTO 60
      !c        ELSE
      !c           IF (L.EQ.0) L = I
      !c        ENDIF
      !c50      CONTINUE
      !cC
      !c55      IF (MODE.EQ.'INPUT') THEN
      !c           FFILE = FILE(1:L) // '.rim'
      !c        ELSE
      !c           FFILE = FILE(1:L) // '.lis'
      !c        ENDIF
      !c      ENDIF
      !
      INQUIRE(FILE=FFILE,EXIST=EXI)
      !
      IF (MODE.EQ.'INPUT' .and. exi) THEN
         OPEN(UNIT=UNIT,FILE=FFILE,STATUS='OLD',IOSTAT=ERR)
      ELSE IF (MODE.EQ.'OUTPUT') THEN
         OPEN(UNIT=unit,FILE=FFILE,STATUS='UNKNOWN',IOSTAT=ERR)
      ELSE
         ERR = 1
      ENDIF
      INLINE = 0
      RETURN
   END SUBROUTINE TIOOPN


   SUBROUTINE TIOCLO(FILE,UNIT,MODE)
      implicit none
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! CLOSE A TEXT FILE
      !     FILE -- FILE NAME
      !     UNIT -- UNIT NUMBER
      !     MODE -- 'INPUT' OR 'OUTPUT'
      !
      CHARACTER(len=*), intent(in) :: FILE
      INTEGER, intent(in) :: UNIT
      CHARACTER(len=*), intent(in) :: MODE
      !
      close(unit)
      RETURN
   END SUBROUTINE TIOCLO


   SUBROUTINE TIOIN(FILE,TEXT,LEN,EOF)

      USE Text, only : ASCCHR

      INCLUDE 'syspar.inc'
      !
      ! **UNIX SYSTEM DEPENDENT ROUTINE **
      !
      !  ROUTINE TO READ A RECORD OF ASCII-TEXT
      !
      !  PARAMETERS
      !
      !     FILE----UNIT TO READ
      !     TEXT----INPUT RECORD (PACKED ASCII-TEXT)
      !     LEN-----NUMBER OF CHARACTERS IN TEXT
      !     EOF-----END-OF-FILE FLAG (0=NO, 1=YES)
      !
      INTEGER, intent(in) :: FILE
      INTEGER, intent(in) :: TEXT(1)
      INTEGER, intent(out) :: LEN
      INTEGER, intent(out) :: EOF

      INCLUDE 'flags.inc'
      INCLUDE 'prom.inc'
      !
      CHARACTER(ZCARDL) :: INCARD
      !
      !  READ A CARD FROM THE CURRENT FILE.
      !
      READ(FILE,'(A)',END=900) INCARD
      INLINE = INLINE + 1
      !
      !  CONVERT INPUT TO ASCII-TEXT
      !
      LEN = 0
      DO I = 1,ZCARDL
         CALL PUTT(TEXT,I,ASCCHR(INCARD(I:I)))
         IF (INCARD(I:I).NE.' ') LEN = I
      END DO
      EOF = 0
      RETURN
      !
      !  END OF RECORD ENCOUNTERED.
      !
900   CONTINUE
      EOF = 1
      !5/15/89  REWIND FILE
      ! IF (PRMPT) THEN
      !    MSUNIT = NOUT
      !    CALL AMSG(PROM,-ZC,' ')
      ! ENDIF
      RETURN
   END SUBROUTINE TIOIN


   SUBROUTINE TIOOUT(FILE,TEXT,LEN,IERR)
      INCLUDE 'syspar.inc'
      !
      ! **UNIX SYSTEM DEPENDENT ROUTINE **
      !
      !  ROUTINE TO WRITE A LINE OF TEXT
      !
      !  PARAMETERS
      !
      !     FILE----UNIT TO WRITE
      !     TEXT----OUTPUT RECORD (PACKED ASCII-TEXT)
      !     LEN-----NUMBER OF CHARACTERS IN TEXT
      !     IERR----ERROR FLAG
      !
      INTEGER, intent(in) :: FILE
      INTEGER, intent(in) :: TEXT(1)
      INTEGER, intent(in) :: LEN
      INTEGER, intent(out) :: IERR

      INCLUDE 'files.inc'

      CHARACTER(len=1) :: CHRASC, ffchar
      CHARACTER(ZPRINL) :: OULINE
      !
      FFCHAR = ' '
      IF (FFFLAG.NE.0) THEN
         FFCHAR = '1'
         FFFLAG = 0
      ENDIF
      !
      !  CONVERT OUTPUT TO TEXT CHARS
      !
      OULINE = ' '
      IF (LEN.LE.0) THEN
         WRITE(FILE,'(A1)') FFCHAR
      ELSE
         L = LEN
         IF (L.GT.ZPRINL) L = ZPRINL
         DO I = 1,L
            CALL GETT(TEXT,I,CH)
            OULINE(I:I) = CHRASC(CH)
         END DO
         ! no forms control on terminal
         if (nint.eq.znint .or. ulpp.eq.0) then
            WRITE(FILE,'(A)') OULINE(1:L)
         else
            WRITE(FILE,'(A1,A)') FFCHAR, OULINE(1:L)
         endif
      ENDIF
      IERR = 0
      RETURN
   END SUBROUTINE TIOOUT

END MODULE TextFiles
