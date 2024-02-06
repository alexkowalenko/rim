      SUBROUTINE RMCLOS
C
C  PURPOSE:   CLOSE A RIM DATABASE.
C

         USE Globals, only : DFLAG, IFMOD, DBDATE, DBTIME
         USE Files, only : F1CLO, F2CLO, F3CLO
         USE DateTime, only: RMTIME, RMDATE

         INCLUDE 'syspar.inc'
         INCLUDE 'rimcom.inc'
C
C
C  DO NOT CLOSE THE DATABASE IF THERE WERE NO MODIFICATIONS
C
         RMSTAT = 0
         IF(.NOT.DFLAG) GOTO 999
         DFLAG = .FALSE.
         IF(.NOT.IFMOD) GOTO 999
C
C  RESET THE DATABASE DATE AND TIME.
C
         DBDATE = RMDATE()
         DBTIME = RMTIME()
C
C  CLOSE THE THREE DATABASE FILES.
C
         CALL F1CLO
         CALL F2CLO
         CALL F3CLO
  999    DFLAG = .FALSE.
         IFMOD = .FALSE.
         RETURN
      END
