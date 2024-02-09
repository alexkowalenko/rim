MODULE Rim
   !! Contains top level functions for the database.
   implicit none
   private

   INTERFACE
      MODULE SUBROUTINE RIMCMD
      END SUBROUTINE RIMCMD

      MODULE SUBROUTINE RMZIP(*)
      END SUBROUTINE
   END INTERFACE

   public RMCONS
   public RMINIT
   public RIMCMD

   public RMTYPT

   public DBOPCL
   public RMZIP
   public XHIBIT
   public RMSET, RMSHOW

contains

   CHARACTER(4) FUNCTION RMTYPT(TYPE)
      !!
      !! RETURN THE CHARACTER LABEL OF A RMTYPE CODE (KZXXXX)
      !!
      USE Parameters

      INTEGER, intent(in) :: TYPE

      INCLUDE 'rmatts.inc'
      !
      RMTYPT = '    '
      SELECT CASE(TYPE)
       CASE (KZINT)
         RMTYPT = 'INT '
       CASE (KZDATE)
         RMTYPT = 'DATE'
       CASE (KZTIME)
         RMTYPT = 'TIME'
       CASE (KZREAL)
         RMTYPT = 'REAL'
       CASE (KZTEXT)
         RMTYPT = 'TEXT'
       CASE (KZDOUB)
         RMTYPT = 'DOUB'
       CASE (KZRVEC)
         RMTYPT = 'RVEC'
       CASE (KZIVEC)
         RMTYPT = 'IVEC'
       CASE (KZDVEC)
         RMTYPT = 'DVEC'
       CASE (KZRMAT)
         RMTYPT = 'RMAT'
       CASE (KZIMAT)
         RMTYPT = 'IMAT'
       CASE (KZDMAT)
         RMTYPT = 'DMAT'
      END SELECT
      RETURN
   END FUNCTION RMTYPT


   SUBROUTINE RMCONS
      !!
      !! INITIALIZATION OF CONSTANTS
      !! CALLED AT RIM STARTUP - open database
      !
      ! MANY OF THESE ARE SYSTEM OR INSTALLATION DEPENDENT
      !!
      !------------------------------------------------------------
      !

      USE Parameters
      USE Globals, only: KMSSVL, KMSSVT, KNAPVL, KNAPVT, USERID
      USE Globals, only: ARBCHS, ARBCHM, USERID
      USE Globals, only: KZHPDB, KZHPRL, KZHPKY, KZHPSK, KZHPTX
      USE Globals, only: KDBHDR
      USE Globals, only : Globals_Initialise => Initialise
      USE DateTime, only : DateTime_Initialise => Initialise, DTFENC
      USE Text, only : ASCTXT, ASCCHR, NONE
      USE Text, only : Text_initialise => Initialise
      USE Utils, only : ZMOVE

      INCLUDE 'files.inc'
      INCLUDE 'cards.inc'
      INCLUDE 'msgcom.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'prom.inc'
      INCLUDE 'maccom.inc'

      INTEGER :: I

      !
      ! /ASCPAR/
      CALL Text_Initialise
      !

      CALL ASCTXT(KDBHDR,ZC,'RIM DATABASE')
      CALL Globals_Initialise

      KMSSVL = 4
      CALL ASCTXT(KMSSVT,ZC,'-MV-')
      KNAPVL = 4
      CALL ASCTXT(KNAPVT,ZC,'-NA-')
      CALL ZMOVE(USERID,NONE)
      ARBCHS = ASCCHR('?')
      ARBCHM = ASCCHR('*')
      !
      CALL DateTime_Initialise
      !
      ! HELP TEXT NAMES
      CALL ASCTXT(KZHPDB,ZC,'RIM_HELP')
      CALL ASCTXT(KZHPRL,ZC,'HELP_TEXT')
      CALL ASCTXT(KZHPKY,ZC,'COMKEY')
      CALL ASCTXT(KZHPSK,ZC,'SUBKEY')
      CALL ASCTXT(KZHPTX,ZC,'COMTXT')
      !
      !
      ! /FILES/
      NINT = ZNINT
      NOUT = ZNOUT
      NOUTR = ZNOUT
      NOUTL = ZNOUT
      NOUTT = ZNOUT
      ECHO = .FALSE.
      CONNI = .TRUE.
      CONNO = .TRUE.
      BATCH = .FALSE.
      UTERML = 80
      UPRINL = 136
      ULPP = 0

      ! /MSGCOM/
      MSUNIT = NOUT
      MSGPTR = 0

      ! /CARDS/
      READCD = 0
      CRDIDX = 0
      CRDRLL = [(0, I = 1, ZCARDN)]
      LXEOC = 0

      ! /PROM/
      PRMPT = .TRUE.
      CALL PRMSET('INIT','RIM:')

      ! /MACCOM/
      MACNUM = 0
      MACNTX = 1
      MACWPT = MACWPZ

      RETURN
   END SUBROUTINE RMCONS


   SUBROUTINE RMINIT
      !!
      !! RUN-TIME INITIALIZATION (CALLED WHEN DATABASE IS OPENED)
      !!
      USE Parameters
      USE Files, only: FILE1, LENBF1, LF1REC, CAREC, CRREC, CLREC
      USE Files, only: FILE2, LENBF2, CURBLK, MODFLG, FILE3, LENBF3
      USE Files, only: MAXIC
      USE Text, only : BLANK
      USE Utils, only : ZEROIT, ZMOVE


      INCLUDE 'incore.inc'
      INCLUDE 'reltbl.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'attble.inc'
      INCLUDE 'lnktbl.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'srtcom.inc'
      INCLUDE 'ptrcom.inc'
      INCLUDE 'pgmcom.inc'
      !
      !  /RELTBL/

      CALL ZMOVE(CNAME,BLANK)
      LRROW = 0
      NRROW = ZRELRI
      RELMOD = 0
      RPBUF = ZRELR

      !
      !  /TUPLER/
      CALL ZMOVE(NAME,BLANK)

      !  /ATTBLE/
      CALL ZMOVE(CANAME,BLANK)
      CALL ZMOVE(CRNAME,BLANK)
      CRSTRT = 0
      CROW = 0
      LROW = 0
      NAROW = ZATTRI
      ATTMOD = 0
      APBUF = ZATTR

      !  /LNKTBL/
      LLROW = 0
      NLROW = 0
      LPBUF = ZLNKR

      !  /INCORE/
      CALL ZEROIT(BLOCKS(1,1),60)
      NEXT = 1
      LIMIT = ZBUF
      NUMBL = 0

      !  /F1COM/
      FILE1 = ZNFIL1
      LENBF1 = ZF1
      LF1REC = 0
      CAREC = 0
      CRREC = 0
      CLREC = 0

      !  /F2COM/
      FILE2 = ZNFIL2
      LENBF2 = ZF2
      CURBLK = [0, 0, 0]
      MODFLG = [0, 0, 0]

      !  /F3COM/
      FILE3 = ZNFIL3
      LENBF3 = ZF3
      MAXIC = ZICBL

      !  /RIMPTR/
      IVAL = 0
      CID = 0
      NID = 0
      NS = 0
      MID = 0
      INDCUR = ZIMISS
      INDMAX = 0

      !  /SRTCOM/
      NSORT = 25
      FIXLT = .TRUE.
      NSORT = 0
      NREAD = 0

      ! /PTRCOM/
      NEXPOS = 0
      NEXPOT = 0

      ! /PGMCOM/
      PGPBLK = 0
      PGVBLK = 0

      RETURN
   END SUBROUTINE RMINIT


   SUBROUTINE DBOPCL(*,MODE)
      !!
      !! OPEN/CLOSE A DATABASE
      !!
      USE Parameters, only: ZFNAML, ZC
      USE Globals, only : DFLAG, DBNAME, DBFNAM, RMSTAT
      USE Message, only : WARN
      USE System, only : SYSDBG, SYSDBN, CHKFIL

      CHARACTER(len=*), intent(in) ::  MODE

      CHARACTER*(ZFNAML) F1N,F2N,F3N,FSET
      INTEGER :: DBSTAT
      LOGICAL :: RW
      !
      IF (MODE.EQ.'OPEN') THEN
         CALL SYSDBG(2,DBSTAT)
         IF(DBSTAT.NE.0) GO TO 900
         CALL DBOPEN(DBFNAM,.FALSE.)
         IF(RMSTAT.NE.0) CALL WARN(RMSTAT)
         IF (DFLAG) THEN
            CALL MSG(' ','DATABASE ''','+')
            CALL AMSG(DBNAME,-ZC,'+')
            CALL MSG(' ',''' IS OPEN.',' ')
            ! READ SETUP COMMANDS IN DBFNAM RIM
            CALL SYSDBN(DBFNAM,F1N,F2N,F3N,FSET)
            IF (CHKFIL(FSET,RW)) CALL SETIN(FSET)
         ENDIF
      ENDIF

      IF (MODE.EQ.'CLOSE') THEN
         CALL RMCLOS
      ENDIF
      !
      !
900   RETURN 1
   END SUBROUTINE DBOPCL


   SUBROUTINE XHIBIT(*)
      !!
      !! LIST ALL RELATIONS HAVING SELECTED ATTRIBUTES.
      !!
      USE Parameters
      USE Globals, only : DFLAG, USERID, OWNER
      USE Lexer, only: ITEMS, LXSREC
      USE Message, only: WARN
      USE Text, only : BLANK, NONE

      INCLUDE 'tupler.inc'
      INCLUDE 'files.inc'
      INCLUDE 'buffer.inc'
      !
      INTEGER :: NUMBER, B, I, L, K, ISTAT
      LOGICAL :: FLAG

      LOGICAL EQ
      INTEGER LOCREL, LOCATT

      !
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      !
      !  EDIT THE EXHIBIT COMMAND
      !
      IF(ITEMS.EQ.1) THEN
         CALL MSG('E','EXHIBIT REQUIRES A LIST OF COLUMNS.',' ')
         GOTO 999
      ENDIF
      NUMBER = ITEMS - 1

      ! ALLOCATE A BLOCK FOR THE ATTRIBUTE LIST
      CALL BLKDEF(11,NUMBER,Z)
      B = BLKLOC(11) - Z
      !
      !  COMMAND IS OKAY
      !
      FLAG = .FALSE.
      !
      DO I=1,NUMBER
         L = B + I*Z
         CALL LXSREC(I+1,BUFFER(L),ZC)
      END DO
      CALL MSG('R','TABLES CONTAINING ','+')
      DO I = 1, NUMBER
         L = B + I*Z
         CALL MSG(' ',' ','+')
         CALL AMSG(BUFFER(L),-ZC,'+')
      END DO
      CALL MSG(' ',' ',' ')
      !
      !  GO THROUGH EACH RELATION.
      !
      I = LOCREL(BLANK)
200   CALL RELGET(ISTAT)
      IF(ISTAT.NE.0) GO TO 500
      !
      !  SEE IF ALL THE ATTRIBUTES LISTED APPEAR IN THIS RELATION
      !
      DO I=1,NUMBER
         L = B + I*Z
         K = LOCATT(BUFFER(L),NAME)
         IF(K.NE.0) GO TO 200
      END DO
      !
      !  CHECK USER READ SECURITY.
      !
      IF(EQ(USERID,OWNER)) GO TO 400
      IF(EQ(RPW,NONE)) GO TO 400
      IF(EQ(RPW,USERID)) GO TO 400
      IF(EQ(MPW,USERID)) GO TO 400
      ! RELATION IS NOT AVAILABLE TO THE USER.
      GO TO 200
      !
      !  ATTRIBUTES ARE IN THIS RELATION
      !
400   CALL MSG('R','   ','+')
      CALL AMSG(NAME,ZC,' ')
      FLAG = .TRUE.
      GO TO 200
      !
      !  SEE IF ANY RELATIONS HAD THE ATTRIBUTES
      !
500   IF(FLAG) GO TO 999
      !
      !  NONE OF THE RELATIONS HAD THE ATTRIBUTES
      !
      CALL MSG('W','THOSE COLUMNS ARE NOT IN ANY TABLES.',' ')
      GO TO 999
      !
      !
      !  DONE WITH EXHIBIT
      !
999   CALL BLKCLR(11)
      RETURN 1
   END SUBROUTINE XHIBIT


   SUBROUTINE RMSET(*)
      !!
      !!  PURPOSE:  SET A PARAMETER
      !!
      USE Globals, only : DBNAME, USERID, CASEIG, TRACE, ARBCHS, &
         ARBCHM, KRMINF, KRMRNF, KMSSVL, KMSSVT, &
         KNAPVL, KNAPVT, IFMOD

      USE Parameters
      USE DateTime, only: KRMDTF, KRMTMF
      USE Lexer, only: KXINT, TOKTYP, ASCREC, IDP, IDL, KWS, ITEMS
      USE Lexer, only: EQKEYW, IDI, LFIND, LXSREC
      USE Message, only: WARN
      USE Text, only : BLANK, STRASC, NONE
      USE Utils, only : ZMOVE

      INCLUDE 'rmatts.inc'
      INCLUDE 'files.inc'
      !
      INTEGER :: OP, FMT, LEN, STAT, L
      CHARACTER*(ZFNAML) :: FN
      !
      ! DO WHATEVER
      !
      OP = 1
      IF (EQKEYW(1,'SET'))     OP = 2
      !
      ! ----- SET DATABASE NAME -----
      !
      IF (EQKEYW(OP,'NAME'))   THEN
         CALL ZMOVE(DBNAME,BLANK)
         IF (ITEMS.GT.OP) CALL LXSREC(OP+1,DBNAME,ZC)
         IFMOD = .TRUE.
         GOTO 900
      ENDIF

      !
      ! ----- SET PASSWORD -----
      !
      IF (EQKEYW(OP,'USER'))   THEN
         CALL ZMOVE(USERID,NONE)
         IF (ITEMS.GT.OP) CALL LXSREC(OP+1,USERID,ZC)
         IFMOD = .TRUE.
         GOTO 900
      ENDIF

      !
      ! ----- SET ECHO ON/OFF -----
      !

      IF (EQKEYW(OP,'ECHO'))    THEN
         IF (ITEMS.LE.OP)        ECHO = .TRUE.
         IF (EQKEYW(OP+1,'ON'))  ECHO = .TRUE.
         IF (EQKEYW(OP+1,'OFF')) ECHO = .FALSE.
         GOTO 900
      ENDIF
      !
      ! ----- SET CASE IGNORE/RESPECT -----
      !
      IF (EQKEYW(OP,'CASE'))    THEN
         IF (EQKEYW(OP+1,'IGNORE')) THEN
            CASEIG = .TRUE.
         ELSE IF (EQKEYW(OP+1,'RESPECT')) THEN
            CASEIG = .FALSE.
         ELSE
            CALL WARN(4)
         ENDIF
         GOTO 900
      ENDIF
      !
      ! ----- SET DATE FORMAT <FORMAT> -----
      !
      IF (EQKEYW(OP,'DATE'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZDATE,FMT,LEN)
         IF (FMT.NE.0) KRMDTF = FMT
         GOTO 900
      ENDIF
      !
      ! ----- SET TIME FORMAT <FORMAT> -----
      !
      IF (EQKEYW(OP,'TIME'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZTIME,FMT,LEN)
         IF (FMT.NE.0) KRMTMF = FMT
         GOTO 900
      ENDIF
      !
      ! ----- SET INTEGER FORMAT <FORMAT> -----
      !
      IF (EQKEYW(OP,'INTEGER'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZINT,FMT,LEN)
         IF (FMT.NE.0) KRMINF = FMT
         GOTO 900
      ENDIF
      !
      ! ----- SET REAL FORMAT <FORMAT> -----
      !
      IF (EQKEYW(OP,'REAL'))   THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'FORMAT')) GOTO 800
         CALL LXFMT(OP+2,KZREAL,FMT,LEN)
         IF (FMT.NE.0) KRMRNF = FMT
         GOTO 900
      ENDIF
      !
      ! ----- SET INPUT <FILENAME> -----
      !
      IF (EQKEYW(OP,'INPUT'))   THEN
         IF (ITEMS.LE.OP) THEN
            FN = ZTRMIN
         ELSE
            CALL STRASC(FN,ASCREC(IDP(OP+1)),IDL(OP+1))
            IF (KWS(OP+1).EQ.'TERMINAL') FN = ZTRMIN
         ENDIF
         CALL SETIN(FN)
         GOTO 900
      ENDIF
      !
      ! ----- SET OUTPUT <FILENAME> -----
      !
      IF (EQKEYW(OP,'OUTPUT'))  THEN
         IF (ITEMS.LE.OP) THEN
            FN = ZTRMOU
         ELSE
            CALL STRASC(FN,ASCREC(IDP(OP+1)),IDL(OP+1))
            IF (KWS(OP+1).EQ.'TERMINAL') FN = ZTRMOU
         ENDIF
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         GOTO 900
      ENDIF
      !
      ! ----- SET MV <MISSING_VALUE_CHAR> -----
      !
      IF (EQKEYW(OP,'MV'))   THEN
         KMSSVL = 0
         IF (ITEMS.GE.OP+1) THEN
            KMSSVL = IDL(OP+1)
            CALL LXSREC(OP+1,KMSSVT,ZC)
         ENDIF
         GOTO 900
      ENDIF
      !
      ! ----- SET NA <NOT_APPLICABLE_CHAR> -----
      !
      IF (EQKEYW(OP,'NA'))   THEN
         KNAPVL = 0
         IF (ITEMS.GE.OP+1) THEN
            KNAPVL = IDL(OP+1)
            CALL LXSREC(OP+1,KNAPVT,ZC)
         ENDIF
         GOTO 900
      ENDIF
      !
      ! ----- SET SINGLE ARBCHAR <CHAR> -----
      !
      IF (EQKEYW(OP,'SINGLE')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'ARBCHAR')) GOTO 800
         CALL GETT(ASCREC(IDP(OP+2)),1,ARBCHS)
         GOTO 900
      ENDIF
      !
      ! ----- SET MUITIPLE ARBCHAR <CHAR> -----
      !
      IF (EQKEYW(OP,'MULTIPLE')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'ARBCHAR')) GOTO 800
         CALL GETT(ASCREC(IDP(OP+2)),1,ARBCHM)
         GOTO 900
      ENDIF
      !
      ! ----- SET TERMINAL WIDTH <VALUE> -----
      !
      IF (EQKEYW(OP,'TERMINAL')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.EQKEYW(OP+1,'WIDTH')) GOTO 800
         IF (.NOT.TOKTYP(OP+2,KXINT)) GOTO 800
         IF (IDI(OP+2).GT.ZPRINL) GOTO 810
         UTERML = IDI(OP+2)
         GOTO 900
      ENDIF
      !
      ! ----- SET REPORT WIDTH/HEIGHT <VALUE> -----
      !
      IF (EQKEYW(OP,'REPORT')) THEN
         IF (ITEMS.LT.OP+2) GOTO 800
         IF (.NOT.TOKTYP(OP+2,KXINT)) GOTO 800
         IF (EQKEYW(OP+1,'WIDTH')) THEN
            IF (IDI(OP+2).GT.ZPRINL) GOTO 810
            UPRINL = IDI(OP+2)
         ELSE IF (EQKEYW(OP+1,'HEIGHT')) THEN
            ULPP = IDI(OP+2)
         ELSE
            GOTO 800
         ENDIF
         GOTO 900
      ENDIF

      !
      ! (TRACE IS FOR DEBUGGING ONLY)
      ! SET TRACE ON/OFF <FILENAME>
      !
      IF (EQKEYW(OP,'TRACE'))  THEN
         TRACE = 1
         IF (EQKEYW(OP+1,'OFF')) TRACE = 0
         IF (TOKTYP(OP+1,KXINT)) TRACE = IDI(OP+1)
         !
         L = LFIND(3,ITEMS-3,'TO')
         IF (L.GT.0) THEN
            IF (L.EQ.ITEMS) GOTO 800
            CALL STRASC(FN,ASCREC(IDP(L+1)),IDL(L+1))
            CALL SETOUT(NOUTT,ZNOUTT,FN,STAT)
            IF (STAT.NE.0) TRACE = 0
         ENDIF
         IF (TRACE.NE.0) CALL MSG('T','TRACE STARTED',' ')
         GOTO 900
      ENDIF
      !
      !---- ERRORS
      !
800   CALL WARN(4)
      GOTO 900
      !
810   CALL MSG('E','THE MAXIMUM WIDTH IS ','+')
      CALL IMSG(ZPRINL,4,' ')
      !
      !---- EXIT
      !
900   RETURN 1
   END SUBROUTINE RMSET


   SUBROUTINE RMSHOW(*)
      !!
      !! SHOW PARAMETER VALUES
      !!
      USE Parameters
      USE Globals, only : DFLAG, DBNAME, USERID, CASEIG, ARBCHS, &
         ARBCHM, KRMINF, KRMRNF, KMSSVL, KMSSVT, KNAPVL, &
         KNAPVT
      USE Lexer, only: ITEMS, EQKEYW, LXSREC
      USE DateTime, only: RMTIME, RMDATE, KRMDTF, KRMTMF
      USE Text, only : BLANK
      USE Utils, only : NDIGIT

      INCLUDE 'rmatts.inc'
      INCLUDE 'files.inc'
      INCLUDE 'msgcom.inc'
      INCLUDE 'maccom.inc'
      INCLUDE 'tupler.inc'

      INTEGER :: TDAY, TTIM, I, J, CH
      LOGICAL :: Q
      LOGICAL :: NE
      INTEGER :: FMTSTR(Z), SMAC(Z)

      IF (EQKEYW(2,'LIMITS')) GOTO 400
      IF (EQKEYW(2,'MACROS')) GOTO 500
      !
      ! SHOW DATE/TIME
      !
      TDAY = RMDATE()
      TTIM = RMTIME()
      MSUNIT = NOUT
      CALL DMSG(TDAY,0,'+',KZDATE)
      CALL MSG(' ','  ','+')
      CALL DMSG(TTIM,0,' ',KZTIME)
      !
      IF (DFLAG) THEN
         CALL MSG(' ','DATABASE ''','+')
         CALL AMSG(DBNAME,-ZC,'+')
         CALL MSG(' ',''' IS OPEN','+')
      ENDIF
      IF (NE(NAME,BLANK)) THEN
         CALL MSG(' ','   CURRENT TABLE IS: ','+')
         CALL AMSG(NAME,-ZC,'+')
      ENDIF
      CALL MSG(' ',' ',' ')

      CALL MSG(' ',' ',' ')
      !
      ! CHECK FOR ARGUMENT
      !
      IF (ITEMS.GT.1) THEN
         IF (EQKEYW(2,'USER')) THEN
            CALL MSG(' ','USER IS ','+')
            CALL AMSG(USERID,ZC,' ')
            GOTO 900
         ENDIF
         IF (EQKEYW(2,'DATE') .OR. EQKEYW(2,'TIME')) GOTO 900
      ENDIF
      !
      ! NO ARGUMENT MEANS SHOW ALL PARAMETERS
      !
      CALL MSG(' ','STRING MATCHING  ','+')
      IF (CASEIG) CALL MSG(' ','CASE: IGNORE ','+')
      IF (.NOT.CASEIG) CALL MSG(' ','CASE: RESPECT','+')

      CALL MSG(' ','        SINGLE ARBCHAR: ','+')
      MSGPTR = MSGPTR + 1
      CALL PUTT(MSGREC,MSGPTR,ARBCHS)
      CALL MSG(' ','   MULTIPLE ARBCHAR: ','+')
      MSGPTR = MSGPTR + 1
      CALL PUTT(MSGREC,MSGPTR,ARBCHM)
      CALL AMSG(0,0,' ')

      CALL MSG(' ','FORMATS          ','+')
      CALL MSG(' ','DATE: ','+')
      CALL FMTDEC(KRMDTF,KZDATE,FMTSTR,12)
      CALL AMSG(FMTSTR,12,'+')
      CALL MSG(' ','   TIME: ','+')
      CALL FMTDEC(KRMTMF,KZTIME,FMTSTR,12)
      CALL AMSG(FMTSTR,12,' ')

      CALL MSG(' ','                 ','+')
      CALL MSG(' ','INTEGER: ','+')
      CALL FMTDEC(KRMINF,KZINT,FMTSTR,12)
      CALL AMSG(FMTSTR,12,'+')
      CALL MSG(' ','REAL: ','+')
      CALL FMTDEC(KRMRNF,KZREAL,FMTSTR,12)
      CALL AMSG(FMTSTR,12,' ')

      CALL MSG(' ','MISSING VALUES   ','+')
      CALL MSG(' ','MV: ','+')
      CALL AMSG(KMSSVT,KMSSVL,'+')
      CALL MSG(' ','             NA: ','+')
      CALL AMSG(KNAPVT,KNAPVL,' ')
      !
      CALL MSG(' ','TERMINAL         ','+')
      CALL MSG(' ','WIDTH ','+')
      CALL IMSG(UTERML,4,'+')
      CALL MSG(' ','           ECHO: ','+')
      IF (ECHO) CALL MSG(' ','ON',' ')
      IF (.NOT.ECHO) CALL MSG(' ','OFF',' ')

      CALL MSG(' ','REPORTS          ','+')
      CALL MSG(' ','WIDTH ','+')
      CALL IMSG(UPRINL,4,'+')
      CALL MSG(' ','           HEIGHT ','+')
      CALL IMSG(ULPP,4,' ')
      GOTO 900

      !
      ! SHOW LIMITS
      !
400   CALL MSG(' ','NAME LENGTH      ','+')
      CALL IMSG(ZC,5,' ')
      CALL MSG(' ','KEYWORD LENGTH   ','+')
      CALL IMSG(ZKEYWL,5,' ')
      CALL MSG(' ','FILENAME LENGTH  ','+')
      CALL IMSG(ZFNAML,5,' ')
      CALL MSG(' ','INPUT TOKENS     ','+')
      CALL IMSG(ZMTOK,5,' ')
      CALL MSG(' ','INPUT CHARS      ','+')
      CALL IMSG(ZMASC,5,' ')
      CALL MSG(' ','SELECT COLUMNS   ','+')
      CALL IMSG(ZMSEL,5,' ')
      CALL MSG(' ','SORT COLUMNS     ','+')
      CALL IMSG(ZMSRT,5,' ')
      CALL MSG(' ','WHERE COLUMNS    ','+')
      CALL IMSG(ZMWHR,5,' ')
      CALL MSG(' ','INPUT REC CHARS  ','+')
      CALL IMSG(ZCARDL,5,' ')
      CALL MSG(' ','OUTPUT REC CHARS ','+')
      CALL IMSG(ZPRINL,5,' ')
      CALL MSG(' ','MACROS           ','+')
      CALL IMSG(ZMXMAC,5,' ')
      GOTO 900
      !
      ! SHOW MACROS
      !
500   IF (ITEMS.GT.2) CALL LXSREC(3,SMAC,ZC)
      DO I = 1, MACNUM
         IF (ITEMS.EQ.3 .AND. NE(SMAC,MACNAM(1,I))) GOTO 600
         MSUNIT = NOUT
         CALL AMSG(MACNAM(1,I),-ZC,'+')
         CALL MSG(' ',' = ','+')
         Q = .FALSE.
         DO J = 1, MACLEN(I)
            CALL GETT(MACTXT(MACPTR(I)),J,CH)
            IF (CH.GE.32) THEN
               IF (.NOT.Q) CALL MSG(' ','''','+')
               Q = .TRUE.
               IF (MSGPTR.GT.ZPRINL-2) CALL MSG(' ',' ',' ')
               MSGPTR = MSGPTR + 1
               CALL PUTT(MSGREC,MSGPTR,CH)
            ELSE
               IF (Q) CALL MSG(' ','''','+')
               Q = .FALSE.
               IF (MSGPTR.GT.ZPRINL-4) CALL MSG(' ',' ',' ')
               CALL MSG(' ',' ','+')
               CALL IMSG(CH,NDIGIT(CH),'+')
               CALL MSG(' ',' ','+')
            ENDIF
         END DO
         IF (Q) CALL MSG(' ','''','+')
         IF (MSGPTR.GT.0) CALL MSG(' ',' ',' ')
600      CONTINUE
      END DO

      !
      !---- EXIT
      !
900   RETURN 1
   END SUBROUTINE RMSHOW

END MODULE Rim
