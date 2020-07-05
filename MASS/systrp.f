      SUBROUTINE SYSTRP(mode)
      INCLUDE 'syspar.d'
 
C     **UNIX SYSTEM & MASSCOMP DEPENDENT ROUTINE **
 
C     CATCH SYSTEM INTERRUPTS (CTRL-C)
 
      character*(*) mode
      EXTERNAL UNIXTRP
 
C     THE MESSAGES SHOULD NEVER PRINT
 
      TTSTAT = signal(2,UNIXTRP)
      IF (TTSTAT.lt.0) PRINT *, 'CTRL-C handling error ',TTSTAT
      RETURN
      END
 
      SUBROUTINE UNIXTRP
      INCLUDE 'syspar.d'
 
C     **UNIX SYSTEM DEPENDENT ROUTINE **
 
C     CATCH SYSTEM INTERRUPTS (CTRL-C) (PART 2)
 
      INCLUDE '../src/flags.d'
C
      HXFLAG = 1
      RETURN
      END
 
 
 
 
