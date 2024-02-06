MODULE Rim
   implicit none
   private

   !
   !  RIM FORTRAN INTERFACE STATUS COMMON
   !

   INTEGER, public :: RMSTAT
   !
   !  VARIABLE DEFINITIONS
   !     RMSTAT--STATUS FLAG
   !               -1  NO MORE DATA AVAIABLE FOR RETRIEVAL
   !                0  OK - OPERATION SUCCESSFULL
   !               10  DATABASE FILES DO NOT CONTAIN A RIM DATABASE
   !               11  DATABASE NAME DOES NOT MATCH FILE CONTENTS
   !               12  INCOMPATABLE DATABASE FILES (DATE,TIME,ETC)
   !               13  DATABASE IS ATTACHED IN READ ONLY MODE
   !               14  DATABASE IS BEING UPDATED
   !               15  DATABASE FILES ARE NOT LOCAL FILES
   !               20  UNDEFINED RELATION
   !               30  UNDEFINED ATTRIBUTE
   !               40  MORE THAN 10 AND/OR OPERATORS IN THE WHERE CLAUSE
   !               41  ILLEGAL "LIMIT EQ N" CONDITION
   !               42  UNRECOGNIZED BOOLEAN COMPARISON
   !               43  EQS ONLY AVAILABLE FOR TEXT ATTRIBUTES
   !               44  ILLEGAL USE OF MIN/MAX IN THE WHERE CLAUSE
   !               45  UNRECOGNIZED AND/OR OPERATOR
   !               46  COMPARED ATTRIBUTES MUST BE THE SAME TYPE/LENGTH
   !               47  LISTS ARE VALID ONLY FOR EQ NE AND CONTAINS
   !               48  ILLEGAL ROW SPECIFICATION
   !               50  RMFIND NOT CALLED
   !               60  RMGET NOT CALLED
   !               70  RELATION REFERENCE NUMBER OUT OF RANGE
   !               80  VARIABLE LENGTH ATTRIBUTES MAY NOT BE SORTED
   !               81  THE NUMBER OF SORTED ATTRIBUTES IS TOO LARGE
   !               89  SORT SYSTEM ERROR (SHOULD NEVER GET THIS)
   !               90  UNAUTHORIZED RELATION ACCESS
   !              100  ILLEGAL VARIABLE LENGTH TUPLE DEFINITION (LOAD/PUT)
   !              110  UNRECOGNIZED RULE RELATIONS
   !              111  MORE THAN 10 RULES PER RELATION
   !              112  UNABLE TO PROCESS RULES
   !              2XX  TUPLE VIOLATES RULE XX
   !
   !         THE FOLLOWING CODES SHOULD NOT BE ENCOUNTERED IN NORMAL USE
   !
   !             1001  BUFFER SIZE PROBLEM - BLKCHG,BLKDEF
   !             1002  UNDEFINED BLOCK - BLKLOC
   !             1003  CANNOT FIND A LARGER BTREE VALUE - BTADD,PUTDAT
   !             1004  CANNOT FIND BTREE BLOCK - BTPUT
   !
   !             21XX  RANDOM FILE ERROR XX ON FILE1
   !             22XX  RANDOM FILE ERROR XX ON FILE2
   !             23XX  RANDOM FILE ERROR XX ON FILE3
   !             24XX  RANDOM FILE ERROR XX ON FILE4
   !


   public RMCONS
   public RMINIT

contains

   SUBROUTINE RMCONS
      !
      ! INITIALIZATION OF CONSTANTS
      ! CALLED AT RIM STARTUP - open database
      !
      ! MANY OF THESE ARE SYSTEM OR INSTALLATION DEPENDENT
      !
      !------------------------------------------------------------
      !

      USE Parameters
      USE Globals, only: KMSSVL, KMSSVT, KNAPVL, KNAPVT, USERID
      USE Globals, only: ARBCHS, ARBCHM, USERID
      USE Globals, only: KZHPDB, KZHPRL, KZHPKY, KZHPSK, KZHPTX
      USE Globals, only: KDBHDR
      USE Globals, only : Globals_Initialise => Initialise
      USE DateTime, only : DateTime_Initialise, DTFENC
      USE Lexer, only : Lexer_Initialise => Initialise
      USE Text, only : Text_Initialise, ASCTXT, ASCCHR, NONE
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
      DO I = 1, ZCARDN
         CRDRLL(I) = 0
      END DO
      LXEOC = 0

      ! /LXLCOM/
      CALL Lexer_Initialise

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

      USE Parameters
      USE Files, only: FILE1, LENBF1, LF1REC, CAREC, CRREC, CLREC
      USE Files, only: FILE2, LENBF2, CURBLK, MODFLG, FILE3, LENBF3
      USE Files, only: MAXIC
      USE Text, only : BLANK
      USE Utils, only : ZEROIT, ZMOVE

      !
      ! RUN-TIME INITIALIZATION (CALLED WHEN DATABASE IS OPENED)
      !
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

      INTEGER :: I

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
      DO I=1,3
         CURBLK(I) = 0
         MODFLG(I) = 0
      END DO

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

END MODULE Rim
