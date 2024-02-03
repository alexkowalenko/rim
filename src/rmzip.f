      SUBROUTINE RMZIP(*)

         USE Globals, only : DFLAG, DBFNAM
         USE Lexer, only: KXTEXT, TOKTYP, ASCREC, IDP, IDL
         USE Message, only: WARN
         USE Text, only : STRASC
         USE System, only : SystemCommand

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  PROCESS ZIP COMMAND  (CALL SYSTEM FUNCTION)
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'cards.inc'
C
         CHARACTER*80 CCARD
         LOGICAL SAVDF
C
         IF (ITEMS.LT.2) GOTO 910

         CCARD = ' '
         IF (.NOT.TOKTYP(2,KXTEXT)) GOTO 910
         NC = IDL(2)
         IF (NC.GT.80) GOTO 910
         CALL STRASC(CCARD,ASCREC(IDP(2)),IDL(2))
C
C     EXECUTE THE  COMMAND VIA SYSTEM DEPENDENT ROUTINE
C     CLOSE THE DATABASE IN CASE OF NO RETURN
C
         SAVDF = DFLAG
         IF (SAVDF) CALL RMCLOS
         CALL SystemCommand(CCARD,IERR)
         IF (SAVDF) CALL DBOPEN(DBFNAM,.FALSE.)
         CALL MSG(' ','RETURN FROM SYSTEM',' ')
         IF (IERR.NE.0) THEN
            CALL MSG('E',', CODE =','+')
            CALL IMSG(IERR,6,' ')
         ENDIF
         GOTO 999
C
  910    CALL WARN(4)
         GOTO 999
C
  999    RETURN 1
      END
