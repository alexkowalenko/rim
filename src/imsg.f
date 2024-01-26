      SUBROUTINE IMSG(NUM,NUMC,MCONT)

         USE Utils, only : NDIGIT

         INCLUDE 'syspar.inc'
C
C  ROUTINE TO FORMAT AND PRINT AN INTEGER
C
C  PARAMETERS
C
C         NUM-----INTEGER TO PRINT
C         NUMC----NUMBER OF CHARS (NEG = DELETE BLANKS)
C         MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
C
         CHARACTER*1 MCONT
C
         INCLUDE 'files.inc'
         INCLUDE 'msgcom.inc'
         PARAMETER (MAXL=24,MAXW=24/ZCW)
         INTEGER NSTR(MAXW)
C
C
         L = NUMC
         IF (L.LT.0) L = MIN(NDIGIT(NUM),0-L)
         L = MIN(L,MAXL)
         CALL ITOA(NSTR,1,L,NUM,ERR)
         CALL AMSG(NSTR,L,MCONT)
         RETURN
      END
