      SUBROUTINE DMSG(JDAT,DFMT,MCONT,TYP)

         USE DateTime, only : ASCDAT
         INCLUDE 'syspar.inc'
C
C  ROUTINE TO FORMAT AND PRINT A JULIAN DATE
C
C  PARAMETERS
C
C         JDAT---JULIAN DATE OR TIME
C         DFMT----DATE/TIME FORMAT INTEGER
C         MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
C         TYP-----TYPE (KZDATE / KZTIME)
C
         CHARACTER*1 MCONT
C
         INCLUDE 'flags.inc'
         INCLUDE 'files.inc'
         INCLUDE 'msgcom.inc'
         INCLUDE 'rmatts.inc'
C
         PARAMETER (DATW=12/ZCW)
         INTEGER ADAT(DATW)
C
         CALL ASCDAT(ADAT,1,L,JDAT,DFMT,TYP)
         CALL AMSG(ADAT,L,MCONT)
         RETURN
      END
