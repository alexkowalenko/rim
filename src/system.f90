MODULE System
  implicit none
  private
  
  public SYSEXI
  public SYSDEL
  public SYSCMD
  public SYSCOM
  public SYSTRP

  contains

    SUBROUTINE SYSEXI
      ! Final callback before exiting
      RETURN
    END SUBROUTINE SYSEXI


    SUBROUTINE SYSDEL(FNAM)
      !
      ! **UNIX SYSTEM DEPENDENT ROUTINE
      !
      ! DELETE A FILE BY NAME
      !
      CHARACTER(len=*), intent(in) :: FNAM
    
      ! Not needed if rioopn creates scratch files
      RETURN
    END SUBROUTINE SYSDEL


    SUBROUTINE SYSCMD(CMD,ERR)
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! EXECUTE A SYSTEM (UNIX shell) COMMAND
      !
      CHARACTER(len=*), intent(in) :: CMD
      INTEGER, intent(out) :: ERR
      ! Fortran 2008
      call EXECUTE_COMMAND_LINE(cmd, exitstat=err)
      !
      RETURN
    END SUBROUTINE SYSCMD


    SUBROUTINE SYSCOM()
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      ! SYSTEM DEPENDENT COMMANDS
      !
      !CCCC CALL MSG(' ','SYSCOM',' ')
      RETURN
    END SUBROUTINE SYSCOM


    SUBROUTINE SYSTRP(mode)
      ! **UNIX SYSTEM DEPENDENT ROUTINE **
    
      ! CATCH SYSTEM INTERRUPTS (CTRL-C)
    
      character(len=*), intent(in) :: mode

      integer :: TTSTAT = 0
    
      ! THE MESSAGES SHOULD NEVER PRINT
    
      !  TTSTAT = signal(2,UNIXTRP,-1)
      IF (TTSTAT.lt.0) PRINT *, 'CTRL-C handling error ',TTSTAT
      RETURN
    END SUBROUTINE SYSTRP
    
    
    SUBROUTINE UNIXTRP
      !INCLUDE '../src/flags.inc'
      COMMON /RIMSTP/ HXFLAG
      INTEGER :: HXFLAG
    
      ! **UNIX SYSTEM DEPENDENT ROUTINE **
    
      ! CATCH SYSTEM INTERRUPTS (CTRL-C) (PART 2)
    
      HXFLAG = 1
      RETURN
    END SUBROUTINE UNIXTRP

END MODULE System