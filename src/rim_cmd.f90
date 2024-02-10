SUBMODULE (RIM) RIM_CMD
   !! RIM commands for external RIM program

   implicit none

contains

   SUBROUTINE REPORT(*)
      !!
      !! PROCESS REPORT COMMAND
      !!
      USE Parameters
      USE Globals, only : DFLAG, RMSTAT
      USE Lexer, only : ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW
      USE Message, only: WARN
      USE Text, only : STRASC

      INCLUDE 'files.inc'
      !
      INTEGER :: STAT
      CHARACTER(len=ZFNAML) :: FN
      LOGICAL :: OUTFIL
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! CHECK FOR OUTPUT FILE
      !
      IF (ITEMS.GT.1) THEN
         IF (ITEMS.NE.3 .OR. .NOT.EQKEYW(2,'TO')) THEN
            CALL WARN(4)
            GOTO 999
         ENDIF
         CALL STRASC(FN,ASCREC(IDP(3)),IDL(3))
         IF (KWS(3).EQ.'TERMINAL') FN = ZTRMOU
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         IF (STAT.NE.0) GOTO 999
         OUTFIL = .TRUE.
      ELSE
         OUTFIL = .FALSE.
      ENDIF
      !
      !
100   CALL PGCOMP
      IF (RMSTAT.NE.0) THEN
         CALL MSG(' ','COMPILATION HALTED.',' ')
         GOTO 900
      ENDIF

200   CALL PGEXEC

900   IF (OUTFIL) CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)

999   RETURN 1
   END SUBROUTINE REPORT


   SUBROUTINE DBDEFN(*)
      !!
      !! DEFINE THE DATABASE SCHEMA
      !!
      !!  SYNTAX:  DEFINE <DATABASE>
      !!       <COMMANDS>
      !!       END
      !!
      USE Parameters
      USE Globals, only : DFLAG, USERID, OWNER, DBNAME, IFMOD, DBFNAM
      USE Globals, only : RMSTAT
      USE DateTime, only : RMDATE
      USE Lexer, only: ITEMS, EQKEYW, LXSREC
      USE Message, only : WARN
      USE Parser, only: LODREC, LODELE, LODREL, LODLNK, LODPAS
      USE Rim, only: DBOPEN
      USE System, only: SYSDBG
      USE Text, only : NONE
      USE Utils, only : ZMOVE

      INCLUDE 'files.inc'
      INCLUDE 'prom.inc'

      INTEGER :: DBSTAT, ERROR, NEWCSN, NUMELE, RFLAG, TDAY

      LOGICAL :: EQ, NE
      INTEGER :: NAMOWN(Z)
      !

      NUMELE  = 0
      NEWCSN = 0
      TDAY = RMDATE()
      !
      ! SET THE PROMPT CHARACTER
      !
      CALL PRMSET('SET','RIM_DEF:')
      !
      ! CHECK THE DATA BASE NAME.
      !
      IF (ITEMS.GE.2) THEN
         !
         ! NAME SPECIFIED ON DEFINE COMMAND
         !
         CALL SYSDBG(2,DBSTAT)
         IF(DBSTAT.NE.0) GO TO 999
         !
         !    CHECK THE DATABASE AND OPEN IT
         !
         CALL DBOPEN (DBFNAM,.TRUE.)
         IF((RMSTAT.NE.15).AND.(RMSTAT.NE.0)) THEN
            CALL WARN(RMSTAT)
            GO TO 999
         ENDIF
      ELSE
         !
         ! USE CURRENT DATABASE
         !
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
      ENDIF


      CALL MSG(' ','BEGIN DEFINITIONS FOR ','+')
      IF (DFLAG) THEN
         CALL MSG(' ','EXISTING DATABASE: ','+')
         RFLAG = 1
      ELSE
         CALL MSG(' ','NEW DATABASE: ','+')
         CALL ZMOVE(OWNER,USERID)
      ENDIF
      CALL AMSG(DBNAME,ZC,' ')
      NEWCSN = 1

      !
      !  PROCESS DB DEFINITION CAOMMANDS
      !
300   CALL LODREC
350   IF(EQKEYW(1,'COLUMNS')) GO TO 400
      IF(EQKEYW(1,'ATTRIBUTES')) GO TO 400
      IF(EQKEYW(1,'TABLES')) GO TO 500
      IF(EQKEYW(1,'RELATIONS')) GO TO 500
      IF(EQKEYW(1,'LINKS')) GO TO 700
      IF(EQKEYW(1,'PASSWORDS')) GO TO 750
      IF(EQKEYW(1,'OWNER')) GO TO 800
      IF(EQKEYW(1,'END')) GO TO 900
      !
      !  ERROR.
      !
      CALL WARN(4)
      GO TO 300
      !
      !  PROCESS ATTRIBUTES.
      !
400   CALL LODELE(NUMELE)
      GO TO 350
      !
      !
      !  PROCESS RELATIONS.
      !
500   CONTINUE
      CALL LODREL(NUMELE)
      GO TO 350
      !
      !
      !  PROCESS LINKS.
      !
700   CONTINUE
      CALL LODLNK
      GO TO 350
      !
      !  PROCESS PASSWORDS.
      !
750   CONTINUE
      CALL LODPAS(ERROR)
      GO TO 350
      !
      ! PROCESS OWNER
      !
800   IF (ITEMS.GE.2) THEN
         CALL LXSREC(2,NAMOWN,ZC)
      ELSE
         CALL ZMOVE(NAMOWN,NONE)
      ENDIF
      IF (NE(OWNER,NONE) .AND. NE(OWNER,NAMOWN)) THEN
         CALL MSG('E','YOU ARE NOT THE DATABASE OWNER.',' ')
         ERROR = ERROR + 1
         GO TO 300
      ENDIF
      CALL ZMOVE(OWNER,NAMOWN)
      CALL ZMOVE(USERID,OWNER)
      GOTO 300
      !
      !  PROCESS END.
      !
900   CONTINUE
      !
      !  SET THE RETURN CODE AND MAKE SURE A SCHEMA HAS BEEN DEFINED
      !
      IF(NEWCSN.EQ.0) GO TO 999
      CALL MSG(' ','DATABASE DEFINITIONS COMPLETED.',' ')
      !
      !  BUFFER THE SCHEMA AND DATABASE OUT
      !
      DFLAG = .TRUE.
      IFMOD = .TRUE.
      CALL DBOPEN (DBFNAM,.FALSE.)
      IF(RMSTAT.NE.0) CALL WARN(RMSTAT)
      !
      !
      ! RESET THE PROMPT CHARACTER TO R
      !
999   CALL PRMSET('RESET',' ')
      CALL BLKCLR(10)
      RETURN 1
   END SUBROUTINE DBDEFN


   SUBROUTINE UNLOAD(*)
      !!
      !! UNLOAD DATABASE SCHEMA, DATA, OR BOTH
      !!
      USE Parameters
      USE Globals, only : DFLAG, DBNAME, USERID, OWNER, RMSTAT
      USE DateTime, only: RMTIME, RMDATE
      USE Lexer, only : ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW, LFIND
      USE Lexer, only : LXSREC
      USE Message, only: WARN
      USE Text, only : STRASC
      USE Utils, only : ZMOVE

      INCLUDE 'rmatts.inc'
      INCLUDE 'files.inc'
      INCLUDE 'buffer.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'dclar1.inc'

      INTEGER :: IREL(Z,1)
      CHARACTER(len=3) :: MODE
      CHARACTER(len=ZFNAML) :: FN
      EQUIVALENCE (BUFFER(1),IREL(1,1))
      INTEGER :: NWORDS, TP, NN, SAVLPP, NOGO, ICNTR, IPERM, PTR, I, STAT, J, IERR, IN, ISTAT, KK, KQ2, NUM
      LOGICAL :: ALL,PERM,LHASH

      INTEGER LOCREL
      LOGICAL EQ, NE
      DATA NWORDS /2500/

      TP = 0
      NN = 0

      ! SET FOR NO FORMS CONTROL
      SAVLPP = ULPP
      ULPP = 0
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      !  INITIALIZE
      !
      CALL BLKCLN
      CALL BLKDEF(4,100,1)
      RMSTAT = 0
      LHASH = .FALSE.
      NOGO = 0
      ICNTR = 0
      IPERM = 0
      ALL = .TRUE.
      MODE = 'ALL'
      PTR = 2
      !
      ! SEE IF AN OUTPUT FILE HAS BEEN SPECIFIED
      !
      I = LFIND(1,ITEMS,'TO')
      IF (I.NE.0) THEN
         IF (I.EQ.ITEMS) GOTO 900
         CALL STRASC(FN,ASCREC(IDP(I+1)),IDL(I+1))
         IF (KWS(I+1).EQ.'TERMINAL') FN = ZTRMOU
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         IF (STAT.NE.0) GOTO 999
         TP = I
      ENDIF
      !
      IF (TP.GT.0 .AND. TP.LT.ITEMS) ITEMS = TP - 1

      !
      !  CHECK TO SEE IF ALL DEFAULTS        (UNLOAD)
      !
      IF (ITEMS .EQ. 1) GO TO 25
      !
      !  CHECK FOR TYPE OF UNLOAD      (UNLOAD ... ALL/DATA/SCHEMA )
      !
      IF (ITEMS .LT. PTR) GO TO 25
      IF ( EQKEYW(PTR,'ALL') .OR. EQKEYW(PTR,'DEFINITIONS') .OR. &
         EQKEYW(PTR,'DATA') .OR. EQKEYW(PTR,'PASSWORDS') ) THEN
         MODE = KWS(PTR)
         PTR = PTR + 1
      ENDIF
      !
      !  UNLOAD PASSWORDS REQUIRES OWNER PRIV
      !
      IF (MODE.EQ.'PAS' .AND. NE(OWNER,USERID)) THEN
         CALL MSG('E', &
            'YOU ARE NOT PERMITTED TO UNLOAD THE PASSWORDS',' ')
         RMSTAT = 9
         GOTO 999
      ENDIF
      !
      !  CHECK FOR HASH  (( THIS OPTION IS NOT USED ))
      !
20    IF (KWS(PTR).EQ.'=') THEN
         IF (KWS(PTR+1).NE.'HASH') GO TO 900
         LHASH = .TRUE.
         PTR = PTR + 2
      ENDIF
      !
25    IF (ITEMS .LT. PTR) THEN
         ! THE COMMAND IS ALL SO SET ICNTR TO MAX
         ICNTR = ALL9S
         GO TO 400
      ENDIF
      !
      !  THE USER HAS SPECIFIED WHICH RELATIONS HE WANTS DUMPED
      !
      J = PTR
      ALL = .FALSE.
210   CALL LXSREC(J,RNAME,ZC)
      IERR = 0
      IN = LOCREL (RNAME)
      IF (IN .NE. 0) THEN
         CALL WARN(1,RNAME)
         RMSTAT = 2
         IERR = 1
      ENDIF
      !
      !  CALL CHKREL TO CHECK PASSWORD PERMISSION ON THE UNLOAD
      !
      CALL CHKREL (PERM,MODE,ISTAT,USERID)
      IF (.NOT.PERM) THEN
         CALL WARN(9,RNAME)
         RMSTAT = 9
         IERR = 1
         GO TO 350
      ENDIF
      !
      !  CHECK TO MAKE SURE THERE IS ONLY ONE OF THE RELATIONS LISTED
      !
      IF (ICNTR .EQ. 0 ) GO TO 335
      DO KK = 1,ICNTR
         IF (EQ(IREL(1,ICNTR),RNAME)) THEN
            CALL MSG('E','YOU HAVE ALREADY SPECIFIED TABLE ''','+')
            CALL AMSG(RNAME,-ZC,'+')
            CALL MSG(' ','''.',' ')
            GO TO 350
         ENDIF
      END DO
      !
      !  EVERYTHING IS CORRECT -- SAVE CERTAIN DATA IN IREL(ICNTR)
      !
335   ICNTR = ICNTR + 1
      CALL ZMOVE(IREL(1,ICNTR),NAME)
350   J = J + 1
      IF (IERR .EQ. 1) NOGO = 1
      IF ( J .LE. ITEMS) GO TO 210
      !
      !  DONE WITH PERMISSION AND CRACKING
      !
400   CONTINUE
      IF (NOGO .EQ. 1) GO TO 999
      CALL MSG('R','*(UNLOAD OF ','+')
      CALL AMSG(DBNAME,-ZC,'+')
      CALL MSG(' ',' AT ','+')
      I = RMDATE()
      CALL DMSG(I,0,'+',KZDATE)
      CALL MSG(' ','  ','+')
      I = RMTIME()
      CALL DMSG(I,0,'+',KZTIME)
      CALL MSG(' ',')',' ')
      !
      CALL MSG('R','*(SET CON=+)',' ')
      CALL MSG('R','*(SET END=NULL)',' ')
      !--   IF (LHASH) THEN
      !--      CALL RMTIME (ITIM)
      !--      NUM = MOD(ITIM,7)
      !--      CALL MSG('R','RIM COMMUNICATION FILE ??',' ')
      !--   ENDIF
      !
      !  IF DIRECTIVE ALL OR SCHEMA CALL UNDEF
      !
      CALL BLKDEF(5,2500,1)
      KQ2 = BLKLOC(5)
      IF ((MODE.EQ.'DEF') .OR. (MODE.EQ.'ALL')) &
         CALL UNDEF(ALL,ICNTR,NUM,MODE,LHASH,BUFFER(KQ2))
      IF (ICNTR .EQ. 0) GO TO 800
      CALL BLKCHG(5,250,6)
      KQ2 = BLKLOC(5)
      !
      !  IF DIRECTIVE ALL OR DATA CALL UNDATA
      !
      IF ((MODE.EQ.'DAT') .OR. (MODE.EQ.'ALL')) &
         CALL UNDATA(ALL,NUM,MODE,LHASH,BUFFER(KQ2))
      IF (ICNTR .EQ. 0) GO TO 800
      !
      !  IF DIRECTIVE PASSWORDS CALL UNPASS
      !
      IF (MODE.EQ.'PAS') &
         CALL UNPASS(ALL,ICNTR,NUM,MODE,LHASH,BUFFER(KQ2))
      IF (ICNTR .EQ. 0) GO TO 800
      !
      CALL MSG('R','*(SET END=;)',' ')
      CALL MSG('R','EXIT',' ')
      GO TO 999
      !
      !  ERROR FOR UNLOADING ALL OF THE DATA
      !
800   CALL WARN(8)
      RMSTAT = 9
      GO TO 999
      !
      !  INCORRECT SYNTAX ERROR MESSAGE
      !
900   CALL WARN(4)
      RMSTAT = 4
      !
      !  CLEAN UP AND END
      !
999   CALL BLKCLN
      ULPP = SAVLPP
      IF (TP.NE.0) THEN
         CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
         IF (RMSTAT.EQ.0) CALL MSG(' ',' UNLOAD COMPLETED',' ')
      ENDIF
      !
      RETURN 1
   END SUBROUTINE UNLOAD


   MODULE SUBROUTINE RMZIP(*)
      !!
      !!  PURPOSE:  PROCESS ZIP COMMAND  (CALL SYSTEM FUNCTION)
      !!
      USE Globals, only : DFLAG, DBFNAM
      USE Files, only: RMCLOS
      USE Lexer, only: KXTEXT, TOKTYP, ASCREC, IDP, IDL, ITEMS
      USE Message, only: WARN
      USE Text, only : STRASC
      USE System, only : SystemCommand

      INTEGER :: NC, IERR
      CHARACTER(len=80) :: CCARD
      LOGICAL :: SAVDF
      !
      IF (ITEMS.LT.2) GOTO 910

      CCARD = ' '
      IF (.NOT.TOKTYP(2,KXTEXT)) GOTO 910
      NC = IDL(2)
      IF (NC.GT.80) GOTO 910
      CALL STRASC(CCARD,ASCREC(IDP(2)),IDL(2))
      !
      ! EXECUTE THE  COMMAND VIA SYSTEM DEPENDENT ROUTINE
      ! CLOSE THE DATABASE IN CASE OF NO RETURN
      !
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
      !
910   CALL WARN(4)
      GOTO 999
      !
999   RETURN 1
   END SUBROUTINE RMZIP


   SUBROUTINE RMHELP(*)
      !!
      !! PROCESSES THE HELP COMMAND
      !!
      ! THE HELP DATABASE CONTAINS:
      !
      !  COMKEY     - A 3 CHARACTER FIELD FOR FINDING A COMMAND
      !  SUBKEY     - A 3 CHARACTER FIELD FOR FINDING A SUB-COMMAND
      !  COMTXT     - A VARIABLE TEXT FIELD WITH A LINE OF STUFF.
      !
      ! THE USERS DATA BASE FILE IS CLOSED AND THE HELP FILES OPENED.
      ! AFTER PROCESSING HELP COMMANDS,
      ! THE HELP DATA BASE IS CLOSED AND THE USERS DATA BASE IS REOPENED.
      !
      USE, intrinsic :: iso_fortran_env

      USE Globals, only: LIBFLG, TOL, HXFLAG, PCENT, RUCK, DBFNAM, KZHPSK, KZHPRL, KZHPKY, DFLAG, RMSTAT
      USE Files, only: RMCLOS
      USE Lexer, only: KWS, ITEMS
      USE Message, only: WARN
      !USE RIM, only: DBOPEN
      USE Text, only: ASCTXT

      INCLUDE 'syspar.inc'

      INCLUDE 'rmatts.inc'
      INCLUDE 'files.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'buffer.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'rimptr.inc'

      COMMON /SAVDB/STOL,SSAVE,SPCENT,SRUCK
      LOGICAL :: SSAVE,SPCENT,SRUCK
      REAL(real64) :: STOL
      !
      !
      ! CLOSE AND SAVE THE CURRENT DATA BASE AND OPEN HELP DATABASE
      !
      SSAVE = DFLAG
      STOL = TOL
      SPCENT = PCENT
      SRUCK = RUCK
      CALL RMCLOS
      !
      LIBFLG = 1
      CALL DBOPEN(ZHFNAM,.FALSE.)
      IF (RMSTAT.NE.0) THEN
         CALL MSG('E','CANNOT FIND THE HELP DATABASE: ' // ZHFNAM // ' ','+')
         CALL IMSG(RMSTAT,5,' ')
         GOTO 810
      ENDIF
      !
      ! SET UP WHERE CLAUSE
      !
      NBOO = 1
      BOO(1) = WHAND
      KOMTYP(1) = 2
      KOMPOS(1) = 1
      KOMLEN(1) = 1
      KOMPOT(1) = 1
      LIMTU = ALL9S
      MAXTU = ALL9S
      NS = 0
      !
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
      ! INCLUDE COMMAND IN THE WHERE CLAUSE
      IF (ITEMS.LT.2) KWS(2) = ' '
      CALL ASCTXT(WHRVAL,ZKEYWL,KWS(2)(1:3))
      WHRLEN(1) = ATTLEN
      !
      ! ALSO GET THE SUB-COMMAND KEYWORD
      !
      I = LOCATT(KZHPSK,KZHPRL)
      IF(I.NE.0) GO TO 800
      CALL ATTGET(ISTAT)
      SKCOL = ATTCOL
      IF (ITEMS.LT.3) KWS(3) = ' '
      CALL ASCTXT(KZHPSK,ZCW,KWS(3)(1:3))
      !
      ! LOOP THRU RECORDS AND DISPLAY
      !
      NLINES = 0
      !C    CALL BLKDEF(9,100,1)
      !C    ITUP = BLKLOC(9)
100   CALL RMLOOK(ITUP,1,1,LENGTH)
110   IF(RMSTAT.NE.0) GOTO 200
      !
      ! POSSIBLE USER INTERRUPTION
      !
      IF (HXFLAG.NE.0) THEN
         CALL WARN(6)
         GOTO 900
      ENDIF
      IF (BUFFER(ITUP+SKCOL-1).NE.KZHPSK(1)) GOTO 100
      NLINES = NLINES + 1
      ITEXT = ITUP + BUFFER(ITUP+2)
      NC = BUFFER(ITEXT)
      CALL AMSG(BUFFER(ITEXT+1),NC,' ')
      GOTO 100
      !
200   IF (NLINES.EQ.0) CALL MSG('E','THERE IS NO HELP TEXT FOR: ' // KWS(2) // ' ' // KWS(3),' ')
      IF(RMSTAT.LE.0) GO TO 900
      CALL MSG(' ','STATUS: ','+')
      CALL IMSG(RMSTAT,5,' ')
      GO TO 900
      !
      ! HELP NOT AVAILABLE
      !
800   CALL MSG('E','HELP IS NOT AVAILABLE',' ')
810   CALL MSG(' ','CONSULT YOUR SYSTEM ADMINISTRATOR.',' ')
      GOTO 900
      !
      ! TRY TO REVERT TO ENTRY CONDITIONS
      !
900   CALL RMCLOS
      LIBFLG = 0
      IF(SSAVE)THEN
         CALL DBOPEN(DBFNAM,.FALSE.)
         TOL=STOL
         PCENT=SPCENT
         RUCK=SRUCK
      ENDIF
      SSAVE=.FALSE.
      RETURN 1
   END SUBROUTINE RMHELP



   SUBROUTINE RIMCMD
      !!
      !! RIM COMMAND DISPATCHER
      !!
      USE Parameters
      USE Globals, only: HXFLAG
      USE Files, only: RMCLOS
      USE Lexer, only: KXKEYW, TOKTYP, KWS, ITEMS, EQKEYW
      USE Parser, only: LODREC, MACDEF
      USE System, only : SYSCOM
      !USE Rim, only : XHIBIT, DBLOAD

      INCLUDE 'files.inc'
      !
      LOGICAL :: SELREL

      ! -----------------------------------------------------
      !
      !
100   CONTINUE
      !
      IF (HXFLAG.GT.0) CALL SETIN(ZTRMIN)
      HXFLAG = 0
      !
      CALL LODREC
      IF (ITEMS.LT.1) GOTO 100
      IF (KWS(1).EQ.'*') GOTO 100
      !
      ! GET THE COMMAND
      !
      IF (.NOT.TOKTYP(1,KXKEYW)) GOTO 800
      !
      !---- QUERY COMMANDS
      !
      IF (EQKEYW(1,'LIST'))    CALL LSTREL(*100)
      IF (EQKEYW(1,'EXHIBIT')) CALL XHIBIT(*100)
      !
      IF (EQKEYW(1,'SELECT'))  CALL SELECT(*100)
      IF (EQKEYW(1,'FROM'))    THEN
         IF(SELREL(1,2)) GOTO 100
         GOTO 100
      ENDIF
      !
      IF (EQKEYW(1,'UNLOAD'))  CALL UNLOAD(*100)
      !
      IF (EQKEYW(1,'REPORT'))  CALL REPORT(*100)
      !
      !---- MODIFICATION COMMANDS
      !
      IF (EQKEYW(1,'BUILD'))   CALL BUILD(*100)
      IF (EQKEYW(1,'CHANGE'))  THEN
         IF (ITEMS.EQ.4 .AND. EQKEYW(2,'OWNER')) CALL CHGPSW(*100)
         IF (ITEMS.EQ.6 .AND. EQKEYW(2,'RPW'))   CALL CHGPSW(*100)
         IF (ITEMS.EQ.6 .AND. EQKEYW(2,'MPW'))   CALL CHGPSW(*100)
         CALL CHGDAT(*100)
      ENDIF
      IF (EQKEYW(1,'DELETE'))  THEN
         IF (EQKEYW(2,'ROWS'))      CALL DELROW(*100)
         IF (EQKEYW(2,'DUPLICATES'))CALL DELDUP(*100)
      ENDIF
      IF (EQKEYW(1,'REFORMAT'))     CALL RFORMT(*100)
      IF (EQKEYW(1,'RENAME')) THEN
         IF (EQKEYW(2,'RELATION'))  CALL RNAMER(*100)
         IF (EQKEYW(2,'TABLE'))     CALL RNAMER(*100)
         IF (EQKEYW(2,'LINK'))      CALL RNAMEL(*100)
         CALL RNAMEA(*100)
      ENDIF
      IF (EQKEYW(1,'REMOVE'))  THEN
         IF (EQKEYW(2,'KEY'))       CALL REMKEY(*100)
         IF (EQKEYW(2,'LINK'))      CALL REMLNK(*100)
         CALL REMREL(*100)
      ENDIF
      IF (EQKEYW(1,'LOAD'))    CALL DBLOAD(*100)
      !
      !---- DATABASE IDENTIFICATION
      !
      IF (EQKEYW(1,'OPEN'))    CALL DBOPCL(*100,'OPEN')
      IF (EQKEYW(1,'CLOSE'))   CALL DBOPCL(*100,'CLOSE')
      !
      !---- SCHEMA MODIFICATION
      !
      IF (EQKEYW(1,'DEFINE'))  CALL DBDEFN(*100)
      !
      !---- RELATION ALGEBRA
      !
      IF (EQKEYW(1,'INTERSECT'))  CALL TUPLRC('INTERSECT',*100)
      IF (EQKEYW(1,'UNION'))      CALL TUPLRC('UNION',*100)
      IF (EQKEYW(1,'JOIN'))       CALL JOIREL(*100)
      IF (EQKEYW(1,'SUBTRACT'))   CALL TUPLRC('SUBTRACT',*100)
      IF (EQKEYW(1,'PROJECT'))    CALL PJECT(*100)
      !CCCCCIF (EQKEYW(1,'DIVIDE'))     CALL TUPLRC(*100)
      !
      !---- COMMANDS THAT DO NOT AFFECT A DATABASE
      !
      IF (EQKEYW(1,'MACRO'))    CALL MACDEF(*100)
      IF (EQKEYW(1,'HELP'))     CALL RMHELP(*100)
      IF (EQKEYW(1,'?'   ))     CALL RMHELP(*100)
      IF (EQKEYW(1,'SHOW'))     CALL RMSHOW(*100)
      IF (EQKEYW(1,'SET'))      CALL RMSET(*100)
      IF (EQKEYW(1,'USER'))     CALL RMSET(*100)
      IF (EQKEYW(1,'ECHO'))     CALL RMSET(*100)
      IF (EQKEYW(1,'NOECHO'))   CALL RMSET(*100)
      IF (EQKEYW(1,'INPUT'))    CALL RMSET(*100)
      IF (EQKEYW(1,'OUTPUT'))   CALL RMSET(*100)
      IF (EQKEYW(1,'SYSTEM'))   CALL RMZIP(*100)

      IF (EQKEYW(1,'NEWPAGE')) THEN
         FFFLAG = 1
         GOTO 100
      ENDIF

      !---- EXIT
      !
      !---C IF (EQKEYW(1,'END'))     GOTO 900
      IF (EQKEYW(1,'EXIT'))    GOTO 900
      IF (EQKEYW(1,'QUIT'))    GOTO 900
      !
      !---- LODREC EOF-GENERATED END  (IGNORE IT)
      !
      IF (KWS(1).EQ.'END' .AND. KWS(2).EQ.'*EOF*')    GOTO 100
      !
      ! UNRECOGNISED COMMAND - POSSIBLY SYSTEM DEPENDENT
      !
      CALL SYSCOM()
      !
800   CALL MSG('EU', KWS(1),'+')
      CALL MSG(' L',' IS NOT A RIM COMMAND.',' ')
      GOTO 100
      !
      ! EXIT
      !
900   CALL RMCLOS
      RETURN
   END SUBROUTINE RIMCMD

END SUBMODULE RIM_CMD
