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
   public DBOPEN

   public DBOPCL
   public DBLOAD
   public RMZIP
   public BUILD
   public CHGPSW
   public CHGDAT
   public DELROW
   public REMKEY
   public REMLNK
   public REMREL
   public RMQERY
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
      USE Cards, only : Cards_Initialise => Initialise
      USE Globals, only: KMSSVL, KMSSVT, KNAPVL, KNAPVT, USERID
      USE Globals, only: ARBCHS, ARBCHM, USERID
      USE Globals, only: KZHPDB, KZHPRL, KZHPKY, KZHPSK, KZHPTX
      USE Globals, only: KDBHDR
      USE Globals, only : Globals_Initialise => Initialise
      USE Extern, only: PRMSET
      USE DateTime, only : DateTime_Initialise => Initialise, DTFENC
      USE Macros, only : Macros_Initialise => Initialise
      USE Text, only : ASCTXT, ASCCHR, NONE
      USE Text, only : Text_initialise => Initialise
      USE Utils, only : ZMOVE

      INCLUDE 'files.inc'
      INCLUDE 'msgcom.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'prom.inc'

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
      CALL Cards_Initialise

      ! /PROM/
      PRMPT = .TRUE.
      CALL PRMSET('INIT','RIM:')

      ! /MACCOM/
      CALL Macros_Initialise

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


   SUBROUTINE DBOPEN(NEWNAM,NEWOK)
      !!
      !!  PURPOSE:  OPEN A RIM DATABASE.
      !!
      !! NEWNAM CONTAINS THE DB NAME
      !! NEWOK = .TRUE. IF THE DB MAY BE CREATED
      !!
      USE Parameters
      USE Globals, only : DFLAG, DBDATE, DBTIME, RMSTAT
      USE Files, only: FILE1, F1OPN, FILE2, F2OPN, FILE3, F3OPN, RMCLOS
      USE DateTime, only: RMTIME, RMDATE
      USE RandomFiles, only: RIOCLO
      USE System, only: SYSDBN, CHKFIL

      CHARACTER(len=*), intent(in) :: NEWNAM
      LOGICAL, intent(in) :: NEWOK

      LOGICAL :: RW
      CHARACTER*(ZFNAML) RIMDB1,RIMDB2,RIMDB3,RIMDBX
      !
      RMSTAT = 0
      CALL RMCLOS
      !
      !  INITIALIZE AND SET DATE, AND TIME
      !
      CALL RMINIT
      DBDATE = RMDATE()
      DBTIME = RMTIME()
      !
      !  FIX UP THE FILE NAMES.
      !
      CALL SYSDBN(NEWNAM,RIMDB1,RIMDB2,RIMDB3,RIMDBX)
      !
      !  CHECK IF THE DB EXISTS
      !
      IF (NEWOK) GOTO 100
      IF (CHKFIL(RIMDB1,RW) .AND. CHKFIL(RIMDB2,RW) .AND. CHKFIL(RIMDB3,RW)) GOTO 100
      CALL MSG('E','I CANNOT OPEN THE FILES.',' ')
      GOTO 999
      !
      !  OPEN FILE 1.
      !
100   CALL F1OPN(RIMDB1)
      IF((RMSTAT.NE.0).AND.(RMSTAT.NE.15)) GO TO 999
      !
      !  OPEN FILE 2.
      !
      CALL F2OPN(RIMDB2)
      IF((RMSTAT.NE.0).AND.(RMSTAT.NE.12).AND.(RMSTAT.NE.15)) THEN
         GO TO 999
      END IF
      !
      !  OPEN FILE 3.
      !
      CALL F3OPN(RIMDB3)
      IF((RMSTAT.NE.0).AND.(RMSTAT.NE.12).AND.(RMSTAT.NE.15)) THEN
         GO TO 999
      END IF
      !
      !  IF THIS IS A NEW DATABASE WE NEED TO SET UP THE FIRST BTREE.
      !
      IF(DFLAG) DBDATE = RMDATE()
      !
      !  IF THERE IS A LOT OF DELETED SPACE ON FILE TWO THEN ADVISE THE USER
      !  TO DO A RELOAD.
      !
      ! CALL PERDEL
      RETURN
      !
      ! AN ERROR SHOULD CLOSE ANY OPEN FILES WITHOUT UPDATE
      !
999   CALL RIOCLO(FILE1)
      CALL RIOCLO(FILE2)
      CALL RIOCLO(FILE3)
      RETURN
   END SUBROUTINE DBOPEN


   SUBROUTINE DBOPCL(*,MODE)
      !!
      !! OPEN/CLOSE A DATABASE
      !!
      USE Parameters, only: ZFNAML, ZC
      USE Globals, only : DFLAG, DBNAME, DBFNAM, RMSTAT
      USE Extern, only : SETIN
      USE Files, only: RMCLOS
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


   SUBROUTINE DBLOAD(*)
      !!
      !!  THIS ROUTINE IS THE DRIVER FOR LOADING DATA VALUES IN THE
      !!  RIM DATA BASE.
      !!
      !! :  LOAD REL_NAME <FROM FILE_NAME> <USING FILENAME>
      USE Parameters
      USE Globals, only : DFLAG, DMFLAG, PIFLAG
      USE Extern, only : SETIN, PRMSET
      USE Formater, only : TYPER, LXFMT
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, IDL, KWS, EQKEYW, IDI, LXSREC
      USE Message, only : WARN
      USE Parser, only: LODREC
      USE Text, only : BLANK, STRASC
      USE DateTime, only : RMDATE

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'files.inc'
      INCLUDE 'buffer.inc'
      INCLUDE 'prom.inc'
      INCLUDE 'dclar1.inc'
      CHARACTER*(ZFNAML) FN, DFN
      !
      ! PARSING DATA FOR QUERY COMMANDS
      !
      INTEGER, PARAMETER :: QKEYL=2

      CHARACTER*(ZKEYWL) QKEYS(QKEYL)
      INTEGER :: SC, JI, JU, I, ISTAT, L, FOR, FPTR, STATUS, SVM, TYP, FMT, FMTLEN, NFOR, KQ1, KQ2
      INTEGER :: QPTRS(2,QKEYL)

      INTEGER PARSE, LOCREL, LOCPRM, LOCATT
      !
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      !  MAKE SURE THE DATABASE CAN BE MODIFIED
      !
      IF (.NOT.DMFLAG) THEN
         CALL WARN(8)
         GO TO 999
      ENDIF
      !
      QKEYS(1) = 'FROM'
      QKEYS(2) = 'USING'
      !
      !  PARSE THE COMMAND
      !
      SC = PARSE(QKEYS,QKEYL,QPTRS)
      JI = QPTRS(1,1)
      JU = QPTRS(1,2)
      IF ( (JI.NE.0 .AND. QPTRS(2,1).NE.2) .OR. (JU.NE.0 .AND. QPTRS(2,2).NE.2)) THEN
         CALL WARN(4)
         GOTO 999
      ENDIF
      !
      !  LOOK FOR THE RELATION NAME
      !
      CALL LXSREC(2,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.NE.0) GOTO 850
      CALL RELGET(ISTAT)
      IF(ISTAT.NE.0) GO TO 850
      !
      !  CHECK FOR AUTHORITY.
      !
      L = LOCPRM(RNAME,2)
      IF(L.NE.0) THEN
         CALL WARN(9,RNAME)
         GO TO 999
      ENDIF
      !
      ! IF PROG INTERFACE THEN RETURN NOW
      !
      IF (PIFLAG) THEN
         CALL RMPII
         GOTO 999
      ENDIF
      !
      ! CHECK IF INPUT FROM FILE
      !
      IF (JI.NE.0) THEN
         CALL STRASC(DFN,ASCREC(IDP(JI+1)),IDL(JI+1))
         IF (KWS(JI+1).EQ.'TERMINAL') JI = 0
      ENDIF
      IF (JU.NE.0 .AND. JI.EQ.0) THEN
         CALL MSG('E','FORMATTED LOADING REQUIRES A DATA FILE.',' ')
         GOTO 999
      ENDIF

      !
      ! CHECK IF FORMATTED
      !
97    IF (JU.NE.0) THEN
         ! GET THE FILE WITH THE FORMAT
         CALL STRASC(FN,ASCREC(IDP(JU+1)),IDL(JU+1))
         IF (KWS(JU+1).EQ.'TERMINAL') FN = ZTRMIN
98       CALL SETIN(FN)
         !
         !    ALLOCATE A BLOCK FOR THE FORMAT
         !        1) ATTCOL
         !        2) LINE NUMBER
         !        3) STARTING COLUMN NUMBER
         !        4) FIELD LENGTH PER ITEM (FROM FORMAT SPEC)
         !        5) FORMAT
         !        6) ITEM POSITION (LOADFM CALCULATES THIS)
         !
         !    LOOK FOR FORMAT CARD
99       CALL LODREC
         IF (.NOT.EQKEYW(1,'FORMAT')) THEN
            CALL MSG('E','A ''FORMAT'' BLOCK WAS EXPECTED ON ''' // FN // '''',' ')
            GOTO 999
         ENDIF
         CALL BLKDEF(9,6,NATT+1)
         FOR = BLKLOC(9)

         ! LOAD FORMAT SPECIFICATIONS
         FPTR = FOR - 1
100      CALL LODREC
         IF (EQKEYW(1,'END')) GOTO 200

         ! LINE IS:  LINE#, COLUMN#, ATTRIBUTE, FORMAT

         ! POSITION  (ITEMS 1 & 2)
         IF (IDI(1).LE.0 .OR. IDI(2).LE.0) THEN
            CALL MSG('E','POSITIONS MUST BE POSITIVE INTEGERS.',' ')
            GOTO 999
         ENDIF
         BUFFER(2+FPTR) = IDI(1)
         BUFFER(3+FPTR) = IDI(2)

         ! ATTRIBUTE (ITEM 3)
         IF (TOKTYP(3,KXNAME)) THEN
            CALL LXSREC(3,ANAME,ZC)
            IF (LOCATT(ANAME,NAME).NE.0) THEN
               CALL WARN(3,ANAME,NAME)
               GOTO 999
            ENDIF
            CALL ATTGET(STATUS)
            BUFFER(1+FPTR) = ATTCOL
         ELSE
            BUFFER(1+FPTR) = 0
         ENDIF

         ! FORMAT
         CALL TYPER(ATTYPE,SVM,TYP)
         CALL LXFMT(4,TYP,FMT,FMTLEN)
         IF (FMT.EQ.0) GOTO 999
         BUFFER(4+FPTR) = FMTLEN
         BUFFER(5+FPTR) = FMT
         BUFFER(6+FPTR) = 0

         FPTR = FPTR + 6
         GOTO 100

200      NFOR = (FPTR - FOR + 1) / 6
         IF (NFOR.LE.0) THEN
            CALL MSG('E','YOU HAVE NOT SPECIFIED ANY FORMATS.',' ')
            GOTO 999
         ENDIF
      ENDIF
      !
      ! IF INPUT FROM FILE  -  OPEN NOW
      !
      IF (JI.NE.0 .AND. DFN.NE.FN) CALL SETIN(DFN)
      !
      !  SET THE PROMPT CHARACTER TO L (LOAD)
      !
      CALL PRMSET('SET','RIM_LOAD:')
      !
      CALL MSG(' ','LOADING TABLE ''','+')
      CALL AMSG(NAME,-ZC,'+')
      CALL MSG(' ','''',' ')
      !
      !  DO THE LOADING
      !
      CALL BLKDEF(11,ZTUPAL,NATT)
      !
      !  FILL UP THIS MATRIX WITH DATA FROM TUPLEA.
      !
      I = LOCATT(BLANK,NAME)
      KQ2 = BLKLOC(11)
      DO I=1,NATT
         CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 800
         CALL BLKMOV(BUFFER(KQ2),ATTNAM,ZTUPAL)
         KQ2 = KQ2 + ZTUPAL
800      CONTINUE
      END DO
      CALL BLKDEF(10,1,MAXCOL)
      KQ1 = BLKLOC(10)
      KQ2 = BLKLOC(11)
899   IF (JU.NE.0) THEN
         CALL LOADFM(BUFFER(KQ1),BUFFER(KQ2),BUFFER(FOR),NFOR)
         CALL BLKCLR(9)
      ELSE
         CALL LOADIT(BUFFER(KQ1),BUFFER(KQ2))
      ENDIF
      CALL BLKCLR(11)
      !
      !  UPDATE THE DATE OF LAST MODIFICATION.
      !
      RDATE = RMDATE()
      CALL RELPUT
      CALL BLKCLR(10)
      !
      ! MAY BE A SECOND LOAD COMMAND
      !
      !CCCC IF(EQKEYW(1,'LOAD')) GO TO 400
      !
      !  END OF LOADING.
      !
      CALL MSG(' ','END DATA LOADING',' ')
      !
      !  RESET THE PROMPT CHARACTER
      !
      CALL PRMSET('RESET',' ')
      GOTO 999
      !
      ! UNRECOGNIZED RELATION NAME.
850   CALL WARN(1,RNAME)
      !
999   RETURN 1
   END SUBROUTINE DBLOAD


   SUBROUTINE BUILD()
      !!
      !!  PURPOSE:  BUILD A KEY INDEX FOR AN ATTRIBUTE IN A RELATION
      !!
      !! SYNTAX:  BUILD KEY FOR <ATTRIBUTE> IN <RELATION>
      !!
      USE Parameters
      USE Globals, only : DFLAG, RMSTAT
      USE Formater, only : TYPER
      USE Lexer, only : KWS, ITEMS, LXSREC
      USE Message, only : WARN

      INCLUDE 'rmatts.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'buffer.inc'
      INCLUDE 'start.inc'
      INCLUDE 'files.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'srtcom.inc'
      INCLUDE 'dclar1.inc'

      INTEGER :: COLUMN, I, IP, ISTAT, L, LENGTH, NKSORT, SVM, TYP, ITUP

      INTEGER :: LOCREL, LOCPRM, LOCATT

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
      !  SCAN THE COMMAND FOR PROPER SYNTAX.
      !
      IF(KWS(2).NE.'KEY') GO TO 950
      IF(KWS(3).NE.'FOR') GO TO 950
      IF(KWS(5).NE.'IN' ) GO TO 950
      IF(ITEMS.GT.6) GO TO 950
      !
      !  FIND THE ATTRIBUTE IN THE SPECIFIED RELATION.
      !
      CALL LXSREC(6,RNAME,ZC)
      CALL LXSREC(4,ANAME,ZC)
      IF(LOCREL(RNAME).NE.0) THEN
         CALL WARN(1,RNAME)
         GO TO 999
      ENDIF
      !
      !  CHECK FOR MODIFY PERMISSION.
      !
      IF(LOCPRM(RNAME,2).NE.0) THEN
         CALL WARN(9,RNAME)
         GO TO 999
      ENDIF
      !
      !  FIND THE ATTRIBUTE IN THE RELATION.
      !
      IF(LOCATT(ANAME,RNAME).NE.0) THEN
         CALL WARN(3,ANAME,RNAME)
         GO TO 999
      ENDIF
      !
      !  DON'T DO IF ATTRIBUTE IS ALREADY A KEY.
      !
      CALL ATTGET(ISTAT)
      IF(ATTKEY.NE.0) THEN
         CALL MSG(' ','ATTRIBUTE ','+')
         CALL AMSG(ANAME,-ZC,'+')
         CALL MSG(' ',' IS ALREADY A KEY.',' ')
         GO TO 999
      ENDIF
      !
      !  DON'T DO IF REAL OR DOUBLE
      !
      CALL TYPER(ATTYPE,SVM,TYP)
      IF(TYP.EQ.KZREAL .OR. TYP.EQ.KZDOUB) THEN
         CALL MSG(' ','REAL OR DOUBLE COLUMNS MAY NOT BE KEYED.',' ')
         GO TO 999
      ENDIF
      !
      !  DETERMINE THE COLUMN TO BE USED FOR THIS ATTRIBUTE.
      !
      COLUMN = ATTCOL
      !
      !  INITIALIZE THE BTREE FOR THIS ELEMENT.
      !
      CALL BTINIT(ATTKEY)
      START = ATTKEY
      CALL ATTPUT(ISTAT)
      !
      !  SORT THE KEY VALUES IF THERE ARE MORE THAN 100 OF THEM
      !
      IF(NTUPLE.GT.100) GO TO 700
      !
      !   SCAN THROUGH ALL THE DATA FOR THIS RELATION.
      !
500   IF(NID.EQ.0) GO TO 900
      CID = NID
      CALL GETDAT(1,NID,ITUP,LENGTH)
      IF(NID.LT.0) GO TO 900
      IP = ITUP + COLUMN - 1
      ! CHECK FOR A VARIABLE LENGTH ATTRIBUTE.
      IF(ATTWDS.EQ.0) IP = BUFFER(IP) + ITUP + 1
      IF(BUFFER(IP).EQ.NULL) GO TO 500
      CALL BTADD(BUFFER(IP),CID,ATTYPE)
      GO TO 500
      !
      !  SORT KEY VALUES BEFORE BUILDING THE B-TREE
      !
700   LENGTH = 2
      NSOVAR = 1
      NKSORT = 3
      LIMTU = ALL9S
      SORTYP(1) = .TRUE.
      VARPOS(1) = 1
      L = 2
      IF(ATTYPE.EQ.KZTEXT) L = 4
      IF(ATTYPE.EQ.KZINT ) L = 1
      IF(ATTYPE.EQ.KZIVEC) L = 1
      IF(ATTYPE.EQ.KZIMAT) L = 1
      VARTYP(1) = L
      VARLEN(1) = 1
      OFFSET = 0
      CALL SORT(NKSORT)
      !
      !  READ THE SORTED KEY VALUES AND BUILD THE BTREE
      !
      CALL GTSORT(IP,1,-1,LENGTH)
      !
800   CALL GTSORT(IP,1,1,LENGTH)
      IF(RMSTAT.NE.0) GO TO 900
      IF(BUFFER(IP).EQ.NULL) GO TO 800
      CALL BTADD(BUFFER(IP),BUFFER(IP+1),ATTYPE)
      GO TO 800
      !
      !  ALL DONE.
      !
      !  RESTORE THE START TO THE BTREE TABLE.
      !
900   I = LOCATT(ANAME,RNAME)
      CALL ATTGET(ISTAT)
      ATTKEY = START
      CALL ATTPUT(ISTAT)
      CALL MSG(' ','BUILD KEY COMPLETED.',' ')
      RMSTAT = 0
      GO TO 999
      !
      !  SYNTAX ERROR.
      !
950   CALL WARN(4)
      !
      !  RETURN
      !
999   RETURN
   END SUBROUTINE BUILD


   SUBROUTINE CHGDAT(*)
      !!
      !! PROCESS CHANGE DATA COMMAND
      !!
      USE Parameters
      USE Globals, only : DFLAG
      USE Lexer, only : LXSREC
      USE Message, only : WARN
      USE DateTime, only : RMDATE

      INCLUDE 'selcom.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'files.inc'
      INCLUDE 'srtcom.inc'
      INCLUDE 'buffer.inc'
      !
      LOGICAL :: SELREL, SELATT, SELWHR
      INCLUDE 'dclar1.inc'
      !
      !
      ! PARSING DATA FOR CHANGE COMMAND
      !
      INTEGER, PARAMETER  :: QKEYL=3
      CHARACTER*(ZKEYWL) QKEYS(QKEYL)
      INTEGER :: QPTRS(2,QKEYL)
      INTEGER I, IFLAG, ISTAT, J, JT, JW, KQ1, KQ11, KQ12, SC, STATUS

      INTEGER PARSE, LOCPRM, LOCATT
      !
      ! ---------------------------------------------
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      !
      QKEYS(1) = 'TO'
      QKEYS(2) = 'IN'
      QKEYS(3) = 'WHERE'
      !
      !
      !  PARSE THE COMMAND
      !
      SC = PARSE(QKEYS,QKEYL,QPTRS)

      JT = QPTRS(1,1)
      J = QPTRS(1,2)
      JW = QPTRS(1,3)
      !
      ! GET RELATION INFO
      !
      IF (.NOT.SELREL(QPTRS(1,2),QPTRS(2,2))) GOTO 999
      !CC   CALL BLKDSP('QUERY SELREL (TUPLER)',NAME,'ZZZZIIIII')
      CALL RELGET(STATUS)
      I = LOCPRM(NAME,2)
      IF (I.NE.0) THEN
         CALL WARN(8)
         GOTO 999
      ENDIF
      !
      ! GET ATTRIBUTE INFO
      !
      CALL LXSREC(2,ANAME,ZC)
      I = LOCATT(ANAME,NAME)
      IF (I.NE.0) THEN
         CALL WARN(3,ANAME,NAME)
         GOTO 999
      ENDIF
      CALL ATTGET(ISTAT)
      IF (ISTAT.NE.0) GOTO 999
      !
      !  CALL CHANGE TO FINISH PROCESSING THE COMMAND.
      !
      CALL BLKDEF(7,MAXCOL,1)
      CALL BLKDEF(8,MAXCOL,1)
      CALL BLKDEF(9,MAXCOL,1)
      KQ1 = BLKLOC(7)
      KQ11 = BLKLOC(8)
      KQ12 = BLKLOC(9)
      RDATE = RMDATE()
      CALL CHANGE(BUFFER(KQ1),BUFFER(KQ11),IFLAG,BUFFER(KQ12))
      !
999   RETURN 1
   END SUBROUTINE CHGDAT


   SUBROUTINE CHGPSW(*)
      !!
      !! CHANGE A PASSWORD
      !!
      !!
      USE Parameters
      USE Globals, only : DFLAG, USERID, OWNER, IFMOD
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
      USE Lexer, only: LXSREC
      USE Message, only : WARN
      USE Text, only : BLANK

      INCLUDE 'files.inc'
      INCLUDE 'tupler.inc'
      !
      INTEGER :: I, ISTAT, L
      INTEGER :: RNAME(Z)

      INTEGER LOCREL, LOCPRM
      LOGICAL :: NE

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
      ! CHECK FOR PERMISSION
      !
      IF (NE(OWNER,USERID)) THEN
         CALL WARN(8)
         GOTO 999
      ENDIF
      !
      ! DO WHATEVER
      !
      IF (EQKEYW(2,'OWNER'))   THEN
         IF (.NOT.EQKEYW(3,'TO')) GOTO 800
         IF (.NOT.TOKTYP(4,KXNAME)) GOTO 810
         CALL LXSREC(4,OWNER,ZC)
         IFMOD = .TRUE.
         GOTO 999
      ENDIF

      IF (EQKEYW(2,'RPW') .OR. EQKEYW(2,'MPW')) THEN
         IF (.NOT.EQKEYW(3,'TO')) GOTO 800
         IF (.NOT.EQKEYW(5,'FOR')) GOTO 800
         IF(ITEMS.NE.6) GO TO 800
         CALL LXSREC(6,RNAME,ZC)
         I = LOCREL(RNAME)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME)
            GO TO 999
         ENDIF
         L = LOCPRM(RNAME,2)
         !CC      IF(L.NE.0) GO TO 999
         IF(.NOT.TOKTYP(4,KXNAME)) GOTO 810
         CALL RELGET(ISTAT)
         !
         !    CHANGE THE PASSWORD.
         !
         IF(EQKEYW(2,'RPW')) THEN
            CALL LXSREC(4,RPW,ZC)
         ELSE
            CALL LXSREC(4,MPW,ZC)
         ENDIF
         CALL RELPUT
         GOTO 999
      ENDIF
      !
      ! ERRORS
      !
800   CALL WARN(4,BLANK,BLANK)
      GOTO 999
810   CALL WARN(7,ASCREC(IDP(4)))
      GOTO 999
      !
      !---- EXIT
      !
999   RETURN 1
   END SUBROUTINE CHGPSW


   SUBROUTINE DELROW(*)
      !!
      !! DELETE ROWS FROM A RELATION
      !!
      USE Parameters
      USE Globals, only : DFLAG, DMFLAG, DBNAME, RMSTAT
      Use Lexer, only: KXNAME, TOKTYP, ITEMS, EQKEYW, LXSREC
      USE Message, only : WARN
      USE Text, only : BLANK

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'attble.inc'
      INCLUDE 'buffer.inc'
      LOGICAL :: NE
      LOGICAL :: EQ
      LOGICAL :: SELWHR
      INCLUDE 'dclar1.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'whcom.inc.f90'

      INTEGER :: I, KQ1, L

      INTEGER LOCREL, LOCPRM

      !
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! MAKE SURE THE DATABASE MAY BE MODIFIED
      !
      IF(.NOT.DMFLAG) THEN
         CALL WARN(RMSTAT,DBNAME)
         GO TO 999
      ENDIF
      !
      ! CHECK THE COMMAND SYNTAX
      !
      IF(.NOT.EQKEYW(3,'FROM') .OR. &
         .NOT.EQKEYW(5,'WHERE') .OR. &
         .NOT.TOKTYP(4,KXNAME)) THEN
         CALL WARN(4,BLANK,BLANK)
         GOTO 999
      ENDIF
      !
      !  FIND THE RELATION NAME IN THE RELATION TABLE.
      !
      CALL LXSREC(4,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
         CALL WARN(1,RNAME,BLANK)
         GOTO 999
      ENDIF
      !
      ! CHECK FOR MODIFY PERMISSION
      !
      L = LOCPRM(RNAME,2)
      IF(L.NE.0) GO TO 999
      !
      !
      !  EVALUATE THE WHERE CLAUSE.
      !
      NBOO = 0
      LIMTU = ALL9S
      IF (.NOT.SELWHR(5,ITEMS-4)) GOTO 999
      IF(RMSTAT.NE.0) GO TO 999
      !
      !  CALL DELETE TO FINISH PROCESSING THE COMMAND.
      !
      CALL BLKDEF(7,MAXCOL,1)
      KQ1 = BLKLOC(7)
      CALL DELETE(BUFFER(KQ1))
      CALL BLKCLR(7)
      !
999   RETURN 1
   END SUBROUTINE DELROW


   SUBROUTINE REMKEY(*)
      !!
      !! REMOVE A KEY (MAKE ATTRIBUTE NON-KEYED)
      !!
      USE Parameters
      USE Globals, only : DFLAG, DMFLAG, DBNAME, USERID, OWNER
      USE Globals, only : RMSTAT
      Use Lexer, only: KXNAME, TOKTYP, ITEMS, EQKEYW, LXSREC
      USE Message, only: WARN
      USE Text, only : BLANK

      ! :  REMOVE KEY FOR ATTRIBUTE IN RELATION

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'attble.inc'

      INCLUDE 'dclar1.inc'
      INCLUDE 'rmatts.inc'

      INTEGER :: I, ISTAT, L

      INTEGER LOCREL, LOCPRM, LOCATT
      LOGICAL :: NE, EQ
      !
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! MAKE SURE THE DATABASE MAY BE MODIFIED
      !
      IF(.NOT.DMFLAG) THEN
         CALL WARN(RMSTAT,DBNAME)
         GO TO 999
      ENDIF
      !
      ! ONLY THE OWNER CAN DO THIS
      !
      IF (NE(OWNER,USERID)) THEN
         CALL WARN(8)
         GOTO 999
      ENDIF
      !
      ! CHECK THE COMMAND SYNTAX
      !
      IF(ITEMS.NE.6 .OR. .NOT.EQKEYW(3,'FOR') .OR. &
         .NOT.EQKEYW(5,'IN') .OR. &
         .NOT.TOKTYP(3,KXNAME) .OR. .NOT.TOKTYP(6,KXNAME)) THEN
         CALL WARN(4)
         GOTO 999
      ENDIF
      !
      !  FIND THE RELATION NAME IN THE RELATION TABLE.
      !
      CALL LXSREC(6,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
         CALL WARN(1,RNAME,BLANK)
         GOTO 999
      ENDIF
      !
      !
      L = LOCPRM(RNAME,2)
      IF(L.NE.0) GO TO 999
      !
      !  CHANGE THE ATTRIBUTE TABLE.
      !
      CALL LXSREC(4,ANAME,ZC)
      I = LOCATT(ANAME,RNAME)
      IF(I.NE.0) THEN
         CALL WARN(3,ANAME,RNAME)
         GOTO 999
      ENDIF
      CALL ATTGET(ISTAT)
      ATTKEY = 0
      CALL ATTPUT(ISTAT)
      !
999   RETURN 1
   END SUBROUTINE REMKEY


   SUBROUTINE REMLNK(*)
      !!
      !! REMOVE A LINK FROM THE DATABASE
      !!
      USE Parameters
      USE Globals, only : DFLAG, DMFLAG, DBNAME, USERID, OWNER, IFMOD
      USE Globals, only : RMSTAT
      USE Lexer, only : ITEMS, LXSREC, LXSREC
      USE Message, only: WARN

      INCLUDE 'tuplel.inc'
      INCLUDE 'lnktbl.inc'
      LOGICAL :: NE
      LOGICAL :: EQ
      INCLUDE 'rmatts.inc'

      INTEGER :: I, ISTAT
      INTEGER :: LKNAM(Z)

      INTEGER LOCLNK
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! MAKE SURE THE DATABASE MAY BE MODIFIED
      !
      IF(.NOT.DMFLAG) THEN
         CALL WARN(RMSTAT,DBNAME)
         GO TO 999
      ENDIF
      !
      ! ONLY THE OWNER CAN DO THIS
      !
      IF (NE(OWNER,USERID)) THEN
         CALL WARN(8)
         GOTO 999
      ENDIF
      !
      IF(ITEMS.NE.3) GO TO 999
      CALL LXSREC(3,LKNAM,ZC)
      !
      !  FIND THE LINK NAME IN THE LINK TABLE.
      !
      I = LOCLNK(LKNAM)
      IF(I.NE.0) THEN
         CALL MSG('E','LINK ''','+')
         CALL AMSG(LKNAM,-ZC,'+')
         CALL MSG(' ',''' IS NOT IN THE DATABASE.',' ')
         GOTO 999
      ENDIF
      !
      !
      !  CHANGE THE LINK TABLE.
      !
      CALL LNKGET(ISTAT)
      IF (LLROW.EQ.0) GO TO 999
      !
      !  CHANGE THE TUPLE STATUS FLAG TO DELETED.
      !
      LNKTBL(1,LLROW) = 0-LNKTBL(1,LLROW)
      LNKMOD = 1
      !
      ! DONE
      !
800   CALL MSG(' ','LINK ''','+')
      CALL AMSG(LKNAM,-ZC,'+')
      CALL MSG(' ',''' HAS BEEN REMOVED.',' ')
      IFMOD = .TRUE.
      !
999   RETURN 1
   END SUBROUTINE REMLNK


   SUBROUTINE REMREL(*)
      !!
      !! REMOVE A RELATION FROM THE DATABASE
      !!
      USE Parameters
      USE Globals, only : DFLAG, DMFLAG, DBNAME, USERID, OWNER, IFMOD
      USE Globals, only : RMSTAT
      USE Extern, only : PRMSET
      USE Lexer, only: KWS, ITEMS, EQKEYW, LXSREC
      USE Message, only: WARN
      USE Parser, only: LODREC
      USE Text, only: BLANK

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'reltbl.inc'
      INCLUDE 'attble.inc'
      INCLUDE 'files.inc'
      INCLUDE 'rmatts.inc'
      !
      INCLUDE 'dclar1.inc'

      INTEGER :: I, ISTAT, L, R

      INTEGER LOCREL, LOCPRM, LOCATT
      LOGICAL NE, EQ
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! MAKE SURE THE DATABASE MAY BE MODIFIED
      !
      IF(.NOT.DMFLAG) THEN
         CALL WARN(RMSTAT,DBNAME)
         GO TO 999
      ENDIF
      !
      ! ONLY THE OWNER CAN DO THIS
      !
      IF (NE(OWNER,USERID)) THEN
         CALL WARN(8)
         GOTO 999
      ENDIF
      !
      R = 2
      IF (EQKEYW(2,'TABLE')) R = 3

      IF(ITEMS.NE.R) THEN
         CALL WARN(4)
         GOTO 999
      ENDIF
      CALL LXSREC(R,RNAME,ZC)
      !
      !  FIND THE RELATION NAME IN THE RELATION TABLE.
      !
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
         CALL WARN(1,RNAME)
         GOTO 999
      ENDIF
      !
      !
      L = LOCPRM(RNAME,2)
      IF(L.NE.0) GO TO 999
      !
      ! IF ONLINE ASK FOR VERIFICATION
      !
      IF (CONNI) THEN
         CALL MSG(' ','ARE YOU SURE (YES/NO) ?',' ')
         CALL PRMSET('SET','?')
         CALL LODREC
         CALL PRMSET('RESET',' ')
         IF (ITEMS.NE.1 .OR. KWS(1).NE.'YES') THEN
            CALL MSG(' ','TABLE NOT REMOVED',' ')
            GOTO 999
         ENDIF
      ENDIF
      !
      !  CHANGE THE RELATION TABLE.
      !
      CALL RELGET(ISTAT)
      IF(LRROW.NE.0) THEN
         RELTBL(1,LRROW) = -RELTBL(1,LRROW)
         RELMOD = 1
      ENDIF
      !
      !  CHANGE THE ATTRIBUTE TABLE.
      !
      I = LOCATT(BLANK,RNAME)
      IF(I.NE.0) GO TO 800
200   CALL ATTGET(ISTAT)
      IF(ISTAT.NE.0) GO TO 800
      CALL ATTDEL(ISTAT)
      IF(ISTAT.NE.0) GO TO 800
      GO TO 200
      !
      ! DONE
      !
800   CALL MSG(' ','TABLE ''','+')
      CALL AMSG(RNAME,-ZC,'+')
      CALL MSG(' ',''' HAS BEEN REMOVED FROM THE DATABASE.',' ')
      IFMOD = .TRUE.
      !
999   RETURN 1
   END SUBROUTINE REMREL


   SUBROUTINE RMQERY(*,QCOM)
      !!
      !! PROG INTERFACE DRIVER FOR QUERY OF THE RIM DATA BASE.
      !! ( THIS IS SIMILAR TO QUERY, BUT DOES NOT LOAD THE
      !!   SELATT ROUTINES AND COMMONS )
      !!
      !! ALSO NO SORTING (9/28/89)
      !!
      USE Parameters
      USE Globals, only : DFLAG, MRINDX, RMSTAT
      USE Message, only: WARN

      INCLUDE 'selcom.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'files.inc'
      INCLUDE 'srtcom.inc'
      !
      LOGICAL :: SELREL, SELWHR, SELSRT
      INTEGER :: PARSE
      !
      CHARACTER(len=*) :: QCOM
      INTEGER :: I, IDUMMY, IERR, IP, J, JS, JW, LENGTH, NKSORT, SC
      !
      ! PARSING DATA FOR QUERY COMMANDS
      !
      INTEGER, PARAMETER :: QKEYL=3
      CHARACTER*(ZKEYWL) QKEYS(QKEYL)
      INTEGER :: QPTRS(2,QKEYL)
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
      QKEYS(1) = 'FROM'
      QKEYS(2) = 'WHERE'
      QKEYS(3) = 'SORTED'
      !
      !  PARSE THE COMMAND
      !
      SC = PARSE(QKEYS,QKEYL,QPTRS)
      J = QPTRS(1,1)
      JW = QPTRS(1,2)
      JS = QPTRS(1,3)
      NS = 0
      !
      !
      ! GET RELATION INFO
      !
      IF (.NOT.SELREL(QPTRS(1,1),QPTRS(2,1))) GOTO 900
      !
      CALL RELGET(I)
      IF (I.NE.0) THEN
         RMSTAT = 20
         GOTO 999
      ENDIF
      !
      !  NO ATTRIBUTE INFO
      !
      !  EVALUATE THE WHERE CLAUSE.
      !
      NBOO = 0
      LIMTU = ALL9S
      IF(JW.EQ.0) GO TO 500
      IF (.NOT.SELWHR(JW,QPTRS(2,2))) GOTO 900
      IF(RMSTAT.NE.0) GO TO 900
      !
      !  SEE IF ANY TUPLES SATISFY THE WHERE CLAUSE.
      !
      CALL RMLOOK(IDUMMY,1,1,LENGTH)
      IF(RMSTAT.NE.0) GOTO 900
      ! BACKSPACE TO BEFORE FIRST FOUND ROW
      NID = CID
      IVAL = IVAL - 1
      LIMVAL = 0
      IF(NS.EQ.3) NS = 2
      !
      !  SEE IF SORTING IS NEEDED OR ASKED FOR.
      !
500   IF(JS.NE.0) THEN
         !
         !    CHECK PI LIMITS FOR SORTED RETRIEVALS
         !
         IF (MRINDX.LT.1 .OR. MRINDX.GT.3) THEN
            RMSTAT = 70
            GOTO 900
         ENDIF
         !
         !    SORT THE DATA
         !
         IF (.NOT.SELSRT(JS,QPTRS(2,3))) GOTO 900
         IF(IERR.EQ.1) GO TO 900
         NKSORT = 1
         CALL SORT(NKSORT)
         NS = 1
         !
         !    OPEN THE SORT FILE TO SETUP FOR RMGET
         !
         LENGTH = NCOL
         CALL GTSORT(IP,1,-1,LENGTH)
         !
         !    USER'S RMGET WILL GET THE RECORDS
         !
      ENDIF
      !
      ! INIT SOME OTHER VARIABLES
      !
      CALL RMPII
      !
      ! ADIOS
      !
900   CONTINUE
999   RETURN  1
   END SUBROUTINE RMQERY


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
      USE Extern, only: SETOUT, SETIN
      USE Formater, only: LXFMT
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
      USE Globals, only : DFLAG, DBNAME, USERID, CASEIG, ARBCHS, ARBCHM, KRMINF, KRMRNF, KMSSVL, KMSSVT, KNAPVL, KNAPVT
      USE Formater, only : FMTDEC
      USE Lexer, only: ITEMS, EQKEYW, LXSREC
      USE Macros, only: MACPTR, MACPTR, MACTXT, MACLEN
      USE DateTime, only: RMTIME, RMDATE, KRMDTF, KRMTMF
      USE Macros, only: MACNUM, MACNAM
      USE Text, only : BLANK
      USE Utils, only : NDIGIT

      INCLUDE 'rmatts.inc'
      INCLUDE 'files.inc'
      INCLUDE 'msgcom.inc'
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
