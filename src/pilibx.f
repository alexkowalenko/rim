      SUBROUTINE AMSG(MTEXT,NUMC,MCONT)
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MCONT
         RETURN
      END


      SUBROUTINE DMSG(JDAT,DFMT,MCONT)
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MCONT
         RETURN
      END


      SUBROUTINE IMSG(NUM,NUMC,MCONT)
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) MCONT
         RETURN
      END


      SUBROUTINE MSG(MTYPE,MTEXT,MCONT)
         USE Globals, only : RMSTAT
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
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         CHARACTER*(*) CTYPE, MTEXT
         RETURN
      END


      SUBROUTINE NXTCRD(EOF)
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         RETURN
      END


      SUBROUTINE WARN(W,TEXT1,TEXT2)
         USE Globals, only : RMSTAT
C
C     DUMMY ROUTINE FOR USER LIBRARY
C
         IF (RMSTAT.EQ.0) RMSTAT = W
         RETURN
      END



      SUBROUTINE LOADIT(MAT,ATT)
C     DUMMY ROUTINE FOR USER LIBRARY
C
         RETURN
      END


      SUBROUTINE LOADFM(MAT,ATT)
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
