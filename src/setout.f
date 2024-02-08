      SUBROUTINE SETOUT(UN,UNF,FILE,STAT)

         USE Globals, only : RMSTAT
         USE TextFiles, only : TIOOPN, TIOCLO
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C     SET THE OUTPUT FILE
C         IF FILE IS THE TERMINAL THEN SET UN TO NOUT
C         ELSE SET UN TO UNF AND OPEN THE FILE
C     IF COULD NOT OPEN FILE THEN SET STAT NE 0
C
         CHARACTER*(*) FILE
C
         INCLUDE 'files.inc'
C
         STAT = 0
C
C  SEE IF THE CURRENT OUTPUT FILE NEEDS TO BE CLOSED.
C
         IF(UN.NE.ZNOUT) THEN
            CALL TIOCLO(FILE,UN,'OUTPUT')
            UN = ZNOUT
         ENDIF
C
C  DETERMINE WHICH UNIT TO USE
C
         CONNO = .TRUE.
         IF(FILE.NE.ZTRMOU) THEN
            CALL TIOOPN(FILE,UNF,'OUTPUT',STAT)
            IF (STAT.EQ.0) THEN
               UN = UNF
               CONNO = .FALSE.
            ELSE
               CALL MSG('E','COULD NOT OPEN ''','+')
               CALL MSG(' ',FILE,'+')
               CALL MSG(' ',''' FOR OUTPUT.',' ')
               RMSTAT = 200
            ENDIF
         ENDIF
C
         IF(BATCH) CONNO = .FALSE.
         RETURN
      END
