      LOGICAL FUNCTION CHKFIL(FNAME,RW)
      INCLUDE 'syspar.inc'
C
C     **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
C
C     CHECK FOR A FILE'S EXISTANCE AND READ/WRITE PERMISSION
C
      CHARACTER*(*) FNAME
      LOGICAL RW
C
      inquire(FILE=fname,EXIST=rw,iostat=ios)
      chkfil = rw
      RW = .TRUE.
      RETURN
      END
