PROGRAM RMTEST
   USE RM_Parameters
   USE RM_Globals, only : RMSTAT

   IMPLICIT NONE

   !
   ! TEST THE FORTRAN INTERFACE OF RIM
   !
   ! OPTIONS (OP) ARE SET VIA THE ONLINE DEBUG PACKAGE
   !
   INTEGER, PARAMETER :: LBUF=30
   !
   CHARACTER(len=80) :: INCARD
   INTEGER :: BUF(LBUF)
   CHARACTER(len=1) :: OP
   !
   !
   NAMELIST /BREAK/ OP, BUF
   !
   ! OPEN AND SELECT
   !
10 WRITE(6,1010 )
1010 FORMAT(' PI COMMAND')
   READ(5,'(A80)') INCARD
   IF (INCARD.EQ.'END') GOTO 900
   CALL RIM(1,INCARD)

   WRITE(6,1020) RMSTAT
1020 FORMAT (' STATUS: ',I5)
   IF (INCARD(1:6).EQ.'SELECT') GOTO 200
   IF (INCARD(1:4).EQ.'LOAD'  ) GOTO 300
   GOTO 10
   !
200 OP = ' '
   CALL RIMDM(1,'GET',BUF)
   WRITE(6,1200) RMSTAT
1200 FORMAT(' GET    ',I5)
210 IF (RMSTAT.NE.0) GOTO 10

   WRITE(6,BREAK)
   READ(5,BREAK)
   IF (OP.EQ.'E') GOTO 10
   IF (OP.EQ.'P') THEN
      CALL RIMDM(1,'PUT',BUF)
      WRITE(6,1300) RMSTAT
1300  FORMAT(' PUT    ',I5)
   ELSE IF (OP.EQ.'D') THEN
      CALL RIMDM(1,'DEL',0)
      WRITE(6,1400) RMSTAT
1400  FORMAT(' DEL    ',I5)
   ENDIF
   GOTO 200
   !
   !
300 WRITE(6,BREAK)
   READ(5,BREAK)
   IF (OP.NE.'L') GOTO 10
   CALL RIMDM(1,'LOAD',BUF)
   GOTO 300
   !
   ! END OF FILE ENCOUNTERED.
   !
900 CONTINUE
   CALL EXIT(0)
END PROGRAM RMTEST
