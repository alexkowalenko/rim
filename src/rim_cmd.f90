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


   SUBROUTINE RIMCMD
      !!
      !! RIM COMMAND DISPATCHER
      !!
      USE Parameters
      USE Globals, only: HXFLAG
      USE Lexer, only: KXKEYW, TOKTYP, KWS, ITEMS, EQKEYW
      USE Parser, only: LODREC
      USE System, only : SYSCOM

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
