      SUBROUTINE TIOCLO(FILE,UNIT,MODE)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C     CLOSE A TEXT FILE
C         FILE -- FILE NAME
C         UNIT -- UNIT NUMBER
C         MODE -- 'INPUT' OR 'OUTPUT'
C
      CHARACTER*(*) FILE
      CHARACTER*(*) MODE
C
      close(unit)
      RETURN
      END
