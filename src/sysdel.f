      SUBROUTINE SYSDEL(FNAM)
      INCLUDE 'syspar.inc'
C
C     **UNIX SYSTEM DEPENDENT ROUTINE
C
C     DELETE A FILE BY NAME
C
      CHARACTER*(*) FNAM
 
C     Not needed if rioopn creates scratch files
      RETURN
      END
