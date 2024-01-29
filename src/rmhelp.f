      SUBROUTINE RMHELP(*)

         USE, intrinsic :: iso_fortran_env

         USE Globals, only : LIBFLG, TOL, HXFLAG, PCENT, RUCK
         USE Text, only: ASCTXT

         INCLUDE 'syspar.inc'
C
C     PROCESSES THE HELP COMMAND
C
C     THE HELP DATABASE CONTAINS:
C
C      COMKEY     - A 3 CHARACTER FIELD FOR FINDING A COMMAND
C      SUBKEY     - A 3 CHARACTER FIELD FOR FINDING A SUB-COMMAND
C      COMTXT     - A VARIABLE TEXT FIELD WITH A LINE OF STUFF.
C
C     THE USERS DATA BASE FILE IS CLOSED AND THE HELP FILES OPENED.
C     AFTER PROCESSING HELP COMMANDS,
C     THE HELP DATA BASE IS CLOSED AND THE USERS DATA BASE IS REOPENED.
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'files.inc'
         INCLUDE 'cflags.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'rimptr.inc'

         COMMON /SAVDB/STOL,SSAVE,SPCENT,SRUCK
         LOGICAL SSAVE,SPCENT,SRUCK
         REAL(real64) :: STOL
C
C
C     CLOSE AND SAVE THE CURRENT DATA BASE AND OPEN HELP DATABASE
C
         SSAVE = DFLAG
         STOL = TOL
         SPCENT = PCENT
         SRUCK = RUCK
         CALL RMCLOS
C
         LIBFLG = 1
         CALL DBOPEN(ZHFNAM,.FALSE.)
         IF (RMSTAT.NE.0) THEN
            CALL MSG('E','CANNOT FIND THE HELP DATABASE: ' //
     1        ZHFNAM // ' ','+')
            CALL IMSG(RMSTAT,5,' ')
            GOTO 810
         ENDIF
C
C     SET UP WHERE CLAUSE
C
         NBOO = 1
         BOO(1) = WHAND
         KOMTYP(1) = 2
         KOMPOS(1) = 1
         KOMLEN(1) = 1
         KOMPOT(1) = 1
         LIMTU = ALL9S
         MAXTU = ALL9S
         NS = 0
C
         I = LOCREL(KZHPRL)
         IF(I.NE.0) GO TO 800
         I = LOCATT(KZHPKY,KZHPRL)
         IF(I.NE.0) GO TO 800
         CALL ATTGET(ISTAT)
         KATTP(1) = ATTCOL
         KATTL(1) = ATTLEN
         KATTY(1) = ATTYPE
         KSTRT = ATTKEY
         IF(KSTRT.NE.0) NS = 2
C     INCLUDE COMMAND IN THE WHERE CLAUSE
         IF (ITEMS.LT.2) KWS(2) = ' '
         CALL ASCTXT(WHRVAL,ZKEYWL,KWS(2)(1:3))
         WHRLEN(1) = ATTLEN
C
C     ALSO GET THE SUB-COMMAND KEYWORD
C
         I = LOCATT(KZHPSK,KZHPRL)
         IF(I.NE.0) GO TO 800
         CALL ATTGET(ISTAT)
         SKCOL = ATTCOL
         IF (ITEMS.LT.3) KWS(3) = ' '
         CALL ASCTXTX(SUBKEY,ZCW,KWS(3)(1:3))
C
C     LOOP THRU RECORDS AND DISPLAY
C
         NLINES = 0
CC    CALL BLKDEF(9,100,1)
CC    ITUP = BLKLOC(9)
  100    CALL RMLOOK(ITUP,1,1,LENGTH)
  110    IF(RMSTAT.NE.0) GOTO 200
C
C     POSSIBLE USER INTERRUPTION
C
         IF (HXFLAG.NE.0) THEN
            CALL WARN(6,0,0)
            GOTO 900
         ENDIF
         IF (BUFFER(ITUP+SKCOL-1).NE.SUBKEY) GOTO 100
         NLINES = NLINES + 1
         ITEXT = ITUP + BUFFER(ITUP+2)
         NC = BUFFER(ITEXT)
         CALL AMSG(BUFFER(ITEXT+1),NC,' ')
         GOTO 100
C
  200    IF (NLINES.EQ.0) CALL MSG('E','THERE IS NO HELP TEXT FOR: ' //
     1          KWS(2) // ' ' // KWS(3),' ')
         IF(RMSTAT.LE.0) GO TO 900
         CALL MSG(' ','STATUS: ','+')
         CALL IMSG(RMSTAT,5,' ')
         GO TO 900
C
C     HELP NOT AVAILABLE
C
  800    CALL MSG('E','HELP IS NOT AVAILABLE',' ')
  810    CALL MSG(' ','CONSULT YOUR SYSTEM ADMINISTRATOR.',' ')
         GOTO 900
C
C     TRY TO REVERT TO ENTRY CONDITIONS
C
  900    CALL RMCLOS
         LIBFLG = 0
         IF(SSAVE)THEN
            CALL DBOPEN(DBFNAM,.FALSE.)
            TOL=STOL
            PCENT=SPCENT
            RUCK=SRUCK
         ENDIF
         SSAVE=.FALSE.
         RETURN 1
      END
