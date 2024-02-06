      SUBROUTINE DBOPCL(*,MODE)

         USE Parameters, only: ZFNAML, ZC
         USE Globals, only : DFLAG, DBNAME, DBFNAM
         USE Message, only : WARN
         USE Rim, only : RMSTAT
         USE System, only : SYSDBG, SYSDBN, CHKFIL
C
C     OPEN/CLOSE A DATABASE
C
         implicit none

         CHARACTER*(*), intent(in) ::  MODE

         CHARACTER*(ZFNAML) F1N,F2N,F3N,FSET
         INTEGER :: DBSTAT
         LOGICAL RW
C
         IF (MODE.EQ.'OPEN') THEN
            CALL SYSDBG(2,DBSTAT)
            IF(DBSTAT.NE.0) GO TO 900
            CALL DBOPEN(DBFNAM,.FALSE.)
            IF(RMSTAT.NE.0) CALL WARN(RMSTAT)
            IF (DFLAG) THEN
               CALL MSG(' ','DATABASE ''','+')
               CALL AMSG(DBNAME,-ZC,'+')
               CALL MSG(' ',''' IS OPEN.',' ')
C           READ SETUP COMMANDS IN DBFNAM RIM
               CALL SYSDBN(DBFNAM,F1N,F2N,F3N,FSET)
               IF (CHKFIL(FSET,RW)) CALL SETIN(FSET)
            ENDIF
         ENDIF

         IF (MODE.EQ.'CLOSE') THEN
            CALL RMCLOS
         ENDIF
C
C
  900    RETURN 1
      END
