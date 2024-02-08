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

   public DBOPCL
   public RMZIP
   public XHIBIT

contains

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

END MODULE Rim
