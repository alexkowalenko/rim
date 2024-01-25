      SUBROUTINE SETIN(FILE)

         USE TextFiles, only : TIOOPN, TIOCLO

         INCLUDE 'syspar.inc'
C
C     SET THE INPUT FILE TO FILE
C         IF FILE IS THE TERMINAL THEN SWITCH TO UNIT NINT
C         ELSE SWITCH TO UNIT NINTA AND OPEN THE FILE
C
         CHARACTER*(*) FILE
C
         INCLUDE 'files.inc'
         INCLUDE 'prom.inc'

C
C  SEE IF THE CURRENT INPUT FILE NEEDS TO BE CLOSED.
C
         IF(NINT.NE.ZNINT) THEN
            CALL TIOCLO(FILE,NINT,'INPUT')
            NINT = ZNINT
         ENDIF
C
C  DETERMINE WHICH UNIT TO USE (TERMINAL IS ALWAYS OPEN)
C
         CONNI = .TRUE.
         IF(FILE.NE.ZTRMIN) THEN
            CALL TIOOPN(FILE,ZNINTA,'INPUT',STAT)
            IF (STAT.EQ.0) THEN
               NINT = ZNINTA
               CONNI = .FALSE.
            ELSE
C          NOTE.. ON ERROR MSG, MSG CALLS SETIN
C                 SO THIS CANNOT BE ERROR MESSAGE
               CALL MSG(' ','FILE NOT FOUND',' ')
            ENDIF
         ENDIF
C
         IF(BATCH) CONNI = .FALSE.
         PRMPT = CONNI
         RETURN
      END
