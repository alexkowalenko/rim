      SUBROUTINE AMSG(MTEXT,NUMC,MCONT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MCONT
         RETURN
      END


      SUBROUTINE DMSG(JDAT,DFMT,MCONT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MCONT
         RETURN
      END


      SUBROUTINE IMSG(NUM,NUMC,MCONT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MCONT
         RETURN
      END


      SUBROUTINE MSG(MTYPE,MTEXT,MCONT)
         USE Rim, only : RMSTAT
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
C     IF THIS IS ERROR MESSAGE AND RMSTAT <> 0 THEN SET RMSTAT
C
         CHARACTER*(*) MTYPE, MTEXT, MCONT
C
         IF (MTYPE(1:1).EQ.'E' .AND. RMSTAT.EQ.0) RMSTAT = 999
         RETURN
      END


      SUBROUTINE MSGCMV(MTEXT,CTYPE)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) CTYPE, MTEXT
         RETURN
      END


      SUBROUTINE NXTCRD(EOF)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         RETURN
      END


      SUBROUTINE PROMPT(PTXT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         RETURN
      END


      SUBROUTINE SETIN(FILE)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) FILE
         RETURN
      END


      SUBROUTINE SETOUT(UN,UNF,FILE,STAT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) FILE
         RETURN
      END


      SUBROUTINE WARN(W,TEXT1,TEXT2)
         USE Rim, only : RMSTAT
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         IF (RMSTAT.EQ.0) RMSTAT = W
         RETURN
      END


      SUBROUTINE PRMSET(MODE,PR)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MODE, PR
C
         RETURN
      END


      SUBROUTINE LOADIT(MAT,ATT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         RETURN
      END
      SUBROUTINE LOADFM(MAT,ATT)
         INCLUDE 'syspar.inc'
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         RETURN
      END


      INTEGER FUNCTION LOCVAR(I)
         LOCVAR = 1
         RETURN
      END


      SUBROUTINE SYSEXI
         RETURN
      END


      SUBROUTINE SYSTRP(I)
         RETURN
      END
