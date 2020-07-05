      SUBROUTINE RMDATE(DAT)
      INCLUDE 'syspar.d'
C
C     ***UNIX SYSTEM DEPENDENT ROUTINE ***
C
C  PURPOSE:   RETURN THE CURRENT DATE AS INTEGER
C
C  PARAMETERS:
C         DAT-----THE CURRENT DATE
C
      integer d(3)
      CALL idate(d)
      CALL JULDAT(d(1),d(2),d(3),DAT)
      RETURN
      END
