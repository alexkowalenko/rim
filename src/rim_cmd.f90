SUBMODULE (RIM) RIM_CMD
   !! RIM commands for external RIM program

   implicit none

contains

   SUBROUTINE REPORT(*)
      !!
      !! PROCESS REPORT COMMAND
      !!
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, RMSTAT, NOUTR
      USE Extern, only: SETOUT, MSG
      USE Lexer, only : ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW
      USE Message, only: WARN
      USE RM_Text, only : STRASC

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
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, USERID, OWNER, DBNAME, IFMOD, DBFNAM
      USE RM_Globals, only : RMSTAT
      USE DateTime, only : RMDATE
      USE Extern, only: PRMSET, AMSG, MSG
      USE Lexer, only: ITEMS, EQKEYW, LXSREC
      USE Message, only : WARN
      USE Parser, only: LODREC, LODELE, LODREL, LODLNK, LODPAS
      USE System, only: SYSDBG
      USE RM_Text, only : NONE
      USE Utils, only : ZMOVE

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
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, DBNAME, USERID, OWNER, RMSTAT, NOUTR, ULPP
      USE RM_BufferData, only: BUFFER, IREL
      USE DateTime, only: RMTIME, RMDATE
      USE Extern, only: SETOUT, DMSG, AMSG, MSG
      USE Lexer, only : ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW, LFIND
      USE Lexer, only : LXSREC
      USE Message, only: WARN
      USE RM_Text, only : STRASC
      USE Utils, only : ZMOVE

      INCLUDE 'rmatts.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'dclar1.inc'

      CHARACTER(len=3) :: MODE
      CHARACTER(len=ZFNAML) :: FN
      INTEGER :: NWORDS, TP, NN, SAVLPP, NOGO, ICNTR, IPERM, PTR, I, STAT, J, IERR, IN, ISTAT, KK, KQ2, NUM
      LOGICAL :: ALL,PERM,LHASH

      INTEGER LOCREL, BLKLOC
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


   SUBROUTINE DELDUP(*)
      !!
      !! DELETE DUPLICATES ROUTINE
      !!
      ! METHOD -
      !          1. SORT TUPLES ALONG ATTRIBUTES OR ALL
      !          2. LOOP ON SORTED TUPLES, DELETING SUCCESSIVE
      !                  DUPLICATES
      !          3. WHEN DONE RESET RSTART AND NTUPLE, PRINT MESSAGE,
      !              AND RETURN
      !
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, RMSTAT
      USE RM_BufferData, only: BUFFER
      USE RM_Buffer, only: DELDAT
      USE Extern, only: IMSG, MSG
      USE Lexer, only : ITEMS, LFIND, LXSREC
      USE Message, only : WARN
      USE RM_Text, only : BLANK

      INCLUDE 'start.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'srtcom.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'rmatts.inc'
      !
      LOGICAL :: IFALL
      INTEGER :: COLUMN, I, II, IID, IP, ISTAT, J, JP1, JP2, KQ1, L, LENGTH, ND, NJ, NKSORT, NSORTW, NUMKEY, NW
      INCLUDE 'dclar1.inc'
      LOGICAL :: SELREL
      INTEGER :: LOCATT, BLKLOC

      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      !
      ! LOCATE WORD 'FROM' OR 'IN'
      !
      J = LFIND(1,ITEMS,'FROM')
      IF(J.EQ.0) J = LFIND(1,ITEMS,'IN')
      NJ = 2
      IF (J.EQ.0) NJ = 0
      !
      ! GET RELATION INFO
      !
      IF (.NOT.SELREL(J,NJ)) GOTO 999

      IFALL = .FALSE.
      NKSORT = 1
      MAXTU = ALL9S
      LIMTU = ALL9S
      NBOO = 0
      LIMVAL = 0
      NS = 0
      IF (J.EQ.3)THEN
         !
         !    SET UP SORT ARRAYS FOR SORTING ON ENTIRE TUPLE
         !
         NSOVAR = 1
         VARPOS(1) = 1
         VARLEN(1) = 0
         SORTYP(1) = .TRUE.
         VARTYP(1) = 1
         OFFSET = 0
         IFALL = .TRUE.
         NKSORT = 4
         ! AND GO DO THE SORT
         GOTO 250
      ENDIF
      !
      ! SET UP FOR SPECIFIED ATTRIBUTES
      !
      II = ITEMS - 2
      NSOVAR = 0
      OFFSET = 0
      DO I=3,II
         CALL LXSREC(I,ANAME,ZC)
         IF(LOCATT(ANAME,NAME).NE.0)THEN
            CALL WARN(3,ANAME,NAME)
            GO TO 999
         ENDIF
         CALL ATTGET(ISTAT)
         !
         !     GOT ATTRIBUTE - FILL SORTVAR LIST
         !
         NSOVAR = NSOVAR + 1
         IF(NSOVAR.GT.NSORTW) THEN
            CALL MSG('E','YOU HAVE SPECIFIED TOO MANY COLUMNS.',' ')
            NSOVAR = NSORTW
            GO TO 999
         ENDIF
         !
         !     ADD TO SORT LIST ARRAYS
         !
         VARPOS(NSOVAR) = ATTCOL
         VARLEN(NSOVAR) = ATTWDS
         SORTYP(NSOVAR) = .TRUE.
         IF(ATTYPE.EQ.KZINT .OR.ATTYPE.EQ.KZIVEC.OR.ATTYPE.EQ.KZIMAT) &
            L = 1
         IF(ATTYPE.EQ.KZREAL.OR.ATTYPE.EQ.KZRVEC.OR.ATTYPE.EQ.KZRMAT) &
            L = 2
         IF(ATTYPE.EQ.KZDOUB.OR.ATTYPE.EQ.KZDVEC.OR.ATTYPE.EQ.KZDMAT) &
            L = 3
         IF(ATTYPE.EQ.KZTEXT) L = 4
         VARTYP(NSOVAR) = L
      END DO
      !
      ! PERFORM THE SORT
      !
250   CALL SORT(NKSORT)
      !
      ! COMPARE THE TUPLES
      !
      ! GET BUFFERS FOR SAVED TUPLE AND READ TUPLE
      !
      CALL BLKDEF(6,MAXCOL,1)
      KQ1 = BLKLOC(6)
      CALL BLKDEF(7,MAXCOL,1)
      IP  = BLKLOC(7)
      !
      ! CHECK FOR ANY KEY ATTRIBUTES
      !
      J = LOCATT(BLANK,NAME)
      NUMKEY = 0
      !
252   CALL ATTGET(ISTAT)
      IF(ISTAT.EQ.0) THEN
         IF(ATTKEY.NE.0) NUMKEY=NUMKEY + 1
         GO TO 252
      ENDIF
      !
      !  RETRIEVE THE SORTED TUPLES
      !
      ! (OPEN THE SORT FILE)
      LENGTH = NCOL
      NS = 1
      CALL GTSORT(IP,3,-1,LENGTH)
      !
      ! (READ THE FIRST RECORD)
      ! (GTSORT RETURNS 'CID', 'NID' AND 'IVAL'
      !          IN 'RIMPTR' COMMON)
      !
      CALL GTSORT(BUFFER(IP),3,0,LENGTH)
      ND = 0
      !
      !  SAVE THE ACTIVE TUPLE IN THE BUFFER
      !
300   CALL BLKMOV(BUFFER(KQ1),BUFFER(IP),LENGTH)
      !
      !  GET NEXT TUPLE FROM SORT FILE
      !
400   CALL GTSORT(BUFFER(IP),3,0,LENGTH)
      IF(CID.EQ.0) GO TO 600
      IF(RMSTAT.NE.0) GO TO 600
      !
      ! COMPARE ATTRIBUTES
      !
      IF (.NOT.IFALL)THEN
         !
         ! COMPARE ON SELECTED ATTRIBUTES
         !
         DO I = 1,NSOVAR
            L = VARPOS(I) - 1
            IF (VARLEN(I).NE.0)THEN
               !
               !       FIXED LENGTH ATTR COMPARE
               !
               DO J = 1,VARLEN(I)
                  IF(BUFFER(IP+L).NE.BUFFER(KQ1+L)) GO TO 300
                  L = L + 1
               END DO
               GO TO 490
            ELSE
               !
               !       VARIABLE LENGTH ATTR COMPARE
               !
               JP1 = BUFFER(IP+L) + IP - 1
               JP2 = BUFFER(KQ1+L) + KQ1 - 1
               IF(BUFFER(JP1).NE.BUFFER(JP2)) GO TO 300
               NW = BUFFER(JP1) + 1
               DO J = 1,NW
                  IF(BUFFER(JP1+J).NE.BUFFER(JP2+J)) GO TO 300
               END DO
            ENDIF
490         CONTINUE
         END DO

         ! (A DUP -- DELE IT)
         GO TO 550
      ELSE
         !
         ! COMPARE ALL ATTRIBUTES
         !
         DO I = 1,LENGTH
            IF(BUFFER(IP-1+I).NE.BUFFER(KQ1-1+I)) GO TO 300
         END DO
      ENDIF
      !
      !
      !  DELETE DUPLICATE RECORD
      !
550   CALL DELDAT (1,CID)
      IF(NUMKEY.EQ.0)GOTO 580
      !
      ! PROCESS ANY KEY ATTRIBUTES
      !
      J = LOCATT(BLANK,NAME)
      !
      ! FOR EACH ATTRIBUTE
560   CALL ATTGET(ISTAT)
      IF (ISTAT.EQ.0) THEN
         IF(ATTKEY.EQ.0) GO TO 560
         ! PROCESS IF KEY
         COLUMN = ATTCOL
         IF(ATTWDS.EQ.0) COLUMN = BUFFER(IP+ATTCOL-1) + 2
         START = ATTKEY
         CALL BTREP(BUFFER(IP+COLUMN-1),0,CID,ATTYPE)
         GO TO 560
      ENDIF

580   CONTINUE
      IF (CID .EQ. IID) IID = NID
      ND = ND + 1
      GO TO 400
      !
      ! UPDATE RELATION INFORMATION
      !
600   CONTINUE
      CALL RELGET(ISTAT)
      NTUPLE = NTUPLE - ND
      CALL RELPUT
      !
      CALL MSG(' ',' ','+')
      CALL IMSG(ND,5,'+')
      CALL MSG(' ',' ROWS WERE DELETED.',' ')
      CALL BLKCLR(7)
      CALL BLKCLR(6)
      !
999   RETURN 1
   END SUBROUTINE DELDUP


   SUBROUTINE RFORMT(*)
      !!
      !! REFORMAT AN ATTRIBUTE
      !!
      USE RM_Parameters
      USE RM_Globals, only : DFLAG
      USE Extern, only: IMSG, MSG
      USE Formater, only: LXFMT
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
      USE Lexer, only: LXSREC
      USE Message, only: WARN
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'dclar1.inc'

      INTEGER :: FMTLEN, I, IFLAG, NEWFMT, NUMT, STATUS
      LOGICAL :: NE,EQ
      INTEGER :: LOCREL, LOCATT, LOCPRM

      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! CHECK SYNTAX
      !
      IF(.NOT.EQKEYW(3,'TO')) GO TO 900
      IF((ITEMS.GT.4).AND.(.NOT.EQKEYW(5,'IN'))) GO TO 900
      IF((ITEMS.NE.4).AND.(ITEMS.NE.6)) GO TO 900
      IF( .NOT.TOKTYP(2,KXNAME) ) THEN
         CALL WARN(7,ASCREC(IDP(2)))
         GOTO 999
      ENDIF
      CALL LXSREC(2,ANAME1,ZC)
      !
      ! LOOK FOR RELATION
      !
      CALL ZMOVE(RNAME1,BLANK)
      IFLAG = 0
      IF (EQKEYW(5,'IN')) THEN
         IFLAG = 1
         CALL LXSREC(6,RNAME1,ZC)
         !  CHECK THAT RELATION EXISTS
         I = LOCREL(RNAME1)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME1,BLANK)
            GO TO 999
         ENDIF
      ENDIF
      !
      ! SEE IF ANAME1 EXISTS
      !
      I = LOCATT(ANAME1,RNAME1)
      IF(I.NE.0) GO TO 910
      CALL ATTGET(STATUS)
      IF(STATUS.NE.0) GO TO 910
      !
      ! GET NEW FORMAT
      !
      CALL LXFMT(4,ATTYPE,NEWFMT,FMTLEN)
      IF (NEWFMT.EQ.0) GOTO 999
      !
      ! REFORMAT ATTRIBUTES
      !
200   I = LOCATT(ANAME1,RNAME1)
      NUMT = 0
210   CALL ATTGET(STATUS)
      IF(STATUS.NE.0) GO TO 300
      !
      ! CHECK FOR PERMISSION
      !
      I = LOCREL(RELNAM)
      I = LOCPRM(RELNAM,2)
      IF(I.EQ.0) GO TO 220
      IF(IFLAG.NE.0) GO TO 930
      GO TO 210
      !
220   NUMT = NUMT + 1
      ATTFOR = NEWFMT
      CALL ATTPUT(STATUS)
      IF(IFLAG.EQ.0) GO TO 210
      !
300   IF (IFLAG.NE.0) THEN
         CALL MSG(' ',' ','+')
         CALL IMSG(NUMT,5,'+')
         CALL MSG(' ',' TABLES MODIFIED.',' ')
      ENDIF
      GOTO 999
      !
      ! BAD SYNTAX
      !
900   CALL WARN(4)
      GO TO 999
      !
      ! ANAME1 NOT THERE
      !
910   CALL WARN(3,ANAME1,RNAME1)
      GO TO 999
      !
930   CALL WARN(8)
      GO TO 999
      !
      ! ALL DONE
      !
999   CONTINUE
      RETURN 1
   END SUBROUTINE RFORMT


   SUBROUTINE RNAMEA(*)
      !!
      !! RENAME AN ATTRIBUTE
      !!
      ! : RENAME <COLUMN> NAME TO NEW_NAME <IN TABLE>
      !
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, USERID, USERID
      USE Extern, only : AMSG, MSG
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
      Use Lexer, only: LXSREC
      USE Message, only: WARN
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tuplel.inc'
      INCLUDE 'dclar1.inc'
      INTEGER :: I, II, IFLAG, ILEN, ITYPE, NUM, STATUS, STOK

      LOGICAL NE,EQ
      INTEGER LOCREL, LOCATT, LOCPRM, LOCLNK
      !
      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      ! CHECK SYNTAX
      !
      STOK = 2
      IF (EQKEYW(2,'ATTRIBUTE') .OR. EQKEYW(2,'COLUMN')) STOK = 3
      IF(.NOT.EQKEYW(STOK+1,'TO')) GO TO 900
      IF((ITEMS.GT.2+STOK).AND.(.NOT.EQKEYW(3+STOK,'IN'))) GO TO 900
      IF((ITEMS.NE.2+STOK).AND.(ITEMS.NE.4+STOK)) GO TO 900
      IF( .NOT.TOKTYP(STOK,KXNAME) ) THEN
         CALL WARN(7,ASCREC(IDP(STOK)))
         GOTO 999
      ENDIF
      IF( .NOT.TOKTYP(2+STOK,KXNAME) ) THEN
         CALL WARN(7,ASCREC(IDP(2+STOK)))
         GOTO 999
      ENDIF
      CALL LXSREC(STOK,ANAME1,ZC)
      CALL LXSREC(2+STOK,ANAME2,ZC)
      !
      ! LOOK FOR RELATION
      !
      CALL ZMOVE(RNAME1,BLANK)
      IFLAG = 0
      IF (EQKEYW(3+STOK,'IN')) THEN
         IFLAG = 1
         CALL LXSREC(4+STOK,RNAME1,ZC)
         !  CHECK THAT RELATION EXISTS
         I = LOCREL(RNAME1)
         IF(I.NE.0) THEN
            CALL WARN(1,RNAME1,BLANK)
            GO TO 999
         ENDIF
      ENDIF
      !
      ! SEE IF ANAME1 EXISTS
      !
      I = LOCATT(ANAME1,RNAME1)
      IF(I.NE.0) GO TO 910
      !
      ! SEE IF ANAME2 ALREADY EXISTS
      !
      I = LOCATT(ANAME2,BLANK )
      IF(I.NE.0) GO TO 200
      !
      ! EXISTS - CHECK TYPE AND LENGTH
      !
      CALL ATTGET(STATUS)
      ILEN = ATTLEN
      ITYPE = ATTYPE
      I = LOCATT(ANAME1,RNAME1)
      CALL ATTGET(STATUS)
      IF(ILEN.NE.ATTLEN) GO TO 920
      IF(ITYPE.NE.ATTYPE) GO TO 920
      !
      ! NOW CHECK THAT OLD AND NEW DON'T COHABITATE IN SAME RELATION
      !
      NUM = 0
120   NUM = NUM + 1
      I = LOCATT(ANAME1,RNAME1)
      DO II=1,NUM
         CALL ATTGET(STATUS)
         IF(STATUS.NE.0) GO TO 200
      END DO
      I = LOCATT(ANAME2,RELNAM)
      IF(I.NE.0) GO TO 120
      CALL MSG('E','COLUMN ''','+')
      CALL AMSG(ANAME2,-ZC,'+')
      CALL MSG(' ',''' ALREADY EXISTS IN TABLE ''','+')
      CALL AMSG(RELNAM,-ZC,' ')
      CALL MSG(' ','''',' ')
      GO TO 999
      !
      ! RENAME ATTRIBUTE
      !
200   I = LOCATT(ANAME1,RNAME1)
210   CALL ATTGET(STATUS)
      IF(STATUS.NE.0) GO TO 300
      !
      ! CHECK FOR PERMISSION
      !
      I = LOCREL(RELNAM)
      I = LOCPRM(RELNAM,2)
      IF(I.NE.0) THEN
         IF(IFLAG.NE.0) GO TO 930
         GO TO 210
      ENDIF
      CALL ZMOVE(ATTNAM,ANAME2)
      CALL ATTPUT(STATUS)
      IF(IFLAG.EQ.0) GO TO 210
      !
      ! ALSO RENAME IN THE LINKS TABLES
      !
300   IF (LOCLNK(BLANK).NE.0) GOTO 800
400   CALL LNKGET(STATUS)
      IF (STATUS.NE.0) GOTO 800
      IF (EQ(A1NAME,ANAME1) .AND. &
         (EQ(R1NAME,RNAME1).OR.IFLAG.EQ.0)) &
         CALL ZMOVE(A1NAME,ANAME2)
      IF (EQ(A2NAME,ANAME1) .AND. &
         (EQ(R2NAME,RNAME1).OR.IFLAG.EQ.0)) &
         CALL ZMOVE(A2NAME,ANAME2)
      CALL LNKPUT(STATUS)
      GOTO 400
      !
800   CALL MSG(' ','COLUMN ''','+')
      CALL AMSG(ANAME1,-ZC,'+')
      CALL MSG(' ',''' RENAMED TO ''','+')
      CALL AMSG(ANAME2,-ZC,'+')
      CALL MSG(' ',''',',' ')
      GOTO 999
      !
      ! BAD SYNTAX
      !
900   CALL WARN(4)
      GO TO 999
      !
      ! ANAME1 NOT THERE
      !
910   CALL WARN(3,ANAME1,RNAME1)
      GO TO 999
      !
      ! TYPE/LENGTH DIFFERS
      !
920   CALL MSG('E','COLUMN ''','+')
      CALL AMSG(ANAME2,-ZC,'+')
      CALL MSG(' ',''' HAS DIFFERENT TYPE OR LENGTH THAN ''','+')
      CALL AMSG(ANAME1,ZC,'+')
      CALL MSG(' ','''.',' ')
      GO TO 999
      !
930   CALL WARN(8)
      GO TO 999
      !
      ! ALL DONE
      !
999   CONTINUE
      RETURN 1
   END SUBROUTINE RNAMEA


   SUBROUTINE RNAMEL(*)
      !!
      !! RENAME A LINK IN THE DATABASE
      !!
      USE RM_Parameters
      USE RM_Globals, only: DFLAG, DMFLAG, DBNAME, USERID, OWNER
      USE RM_Globals, only: RMSTAT
      USE Extern, only : AMSG, MSG
      USE Lexer, only: ITEMS, EQKEYW, LXSREC
      USE Message, only: WARN
      USE Utils, only : ZMOVE

      INCLUDE 'tuplel.inc'
      INCLUDE 'lnktbl.inc'

      INCLUDE 'rmatts.inc'
      !
      INTEGER :: I, ISTAT
      INTEGER :: LKNAM(Z), NLKNAM(Z)

      INTEGER LOCLNK
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
      IF(ITEMS.NE.5) GO TO 999
      IF (.NOT.EQKEYW(4,'TO'))  THEN
         CALL WARN(4)
         GOTO 999
      ENDIF

      CALL LXSREC(3,LKNAM,ZC)
      CALL LXSREC(5,NLKNAM,ZC)
      !
      !  CHECK THAT THE NEW NAME DOES NOT EXIST
      !
      I = LOCLNK(NLKNAM)
      IF(I.EQ.0) THEN
         CALL MSG('E','LINK ''','+')
         CALL AMSG(NLKNAM,-ZC,'+')
         CALL MSG(' ',''' IS ALREADY IN THE DATABASE.',' ')
         GOTO 999
      ENDIF
      !
      !  FIND THE OLD LINK NAME IN THE LINK TABLE.
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
      CALL ZMOVE(LNAME,NLKNAM)
      CALL LNKPUT(ISTAT)
      IF (ISTAT.EQ.0) THEN
         CALL MSG(' ','LINK ''','+')
         CALL AMSG(LKNAM,-ZC,'+')
         CALL MSG(' ',''' HAS BEEN RENAMED TO ''','+')
         CALL AMSG(NLKNAM,-ZC,'+')
         CALL MSG(' ','''',' ')
      ENDIF
      !
999   RETURN 1
   END SUBROUTINE RNAMEL


   SUBROUTINE SELECT(*)
      !!
      !! PROCESS SELECT COMMAND
      !!
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, RMSTAT, NOUTR
      USE Extern, only: SETOUT, MSG
      USE Lexer, only : ASCREC, IDP, IDL, KWS
      USE Message, only: WARN
      USE RM_Text, only : STRASC

      INCLUDE 'selcom.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'srtcom.inc'
      !
      LOGICAL :: EQKEYW
      LOGICAL :: SELREL, SELATT, SELWHR, SELSRT
      INTEGER :: PARSE
      CHARACTER*(ZFNAML) FN
      !
      ! PARSING DATA FOR QUERY COMMANDS
      !
      INTEGER, PARAMETER :: QKEYL=4
      CHARACTER*(ZKEYWL) QKEYS(QKEYL)
      INTEGER :: QPTRS(2,QKEYL)
      INTEGER :: IDUMMY, J, JS, JW, JT, LENGTH, NKSORT, SC, STAT
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
      QKEYS(3) = 'SORT'
      QKEYS(4) = 'TO'
      !
      !
      !  PARSE THE COMMAND
      !
      SC = PARSE(QKEYS,QKEYL,QPTRS)
      !CC   CALL BLKDSP('QUERY PARSE',QPTRS(1,1),'IIIIIIII')
      IF (SC.LT.3) THEN
         CALL WARN(4)
         GOTO 999
      ENDIF
      J = QPTRS(1,1)
      JW = QPTRS(1,2)
      JS = QPTRS(1,3)
      JT = QPTRS(1,4)
      NS = 0
      NSOVAR = 0
      !
      ! GET RELATION INFO
      !
      IF (.NOT.SELREL(QPTRS(1,1),QPTRS(2,1))) GOTO 900
      !
      ! GET ATTRIBUTE INFO
      !
      IF (.NOT.SELATT(2,SC-2)) GOTO 900
      !
      ! EVALUATE THE WHERE CLAUSE.
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
      IF(RMSTAT.NE.0)THEN
         CALL MSG('W','NO ROWS SATISFY YOUR SELECTION CRITERIA.',' ')
         GO TO 900
      ENDIF
      ! BACKSPACE TO BEFORE FIRST FOUND ROW
      NID = CID
      IVAL = IVAL - 1
      LIMVAL = 0
      IF(NS.EQ.3) NS = 2
      !
      ! CHECK SORT CLAUSE
      !
500   IF (JS.NE.0) THEN
         ! SORT IS REQUESTED
         IF (SFUNCT) THEN
            CALL MSG('E', &
               'FUNCTION SORT IS ON THE INDEPENDENT COLUMN.',' ')
            CALL MSG(' ','YOU MAY NOT SPECIFY A SORT CLAUSE.',' ')
            GOTO 900
         ENDIF
         IF (.NOT.SELSRT(JS,QPTRS(2,3))) GOTO 900
         NKSORT = 1
         CALL SORT(NKSORT)
         NS = 1
      ELSE
         ! SORT NOT REQUESTED - MAY DO ONE ANYWAY
         IF (SFUNCT) THEN
            OFFSET = 0
            NKSORT = 1
            CALL SORT(NKSORT)
            NS = 1
         ENDIF
      ENDIF
      !
      ! SEE IF AN OUTPUT FILE HAS BEEN SPECIFIED
      !
      IF (JT.NE.0) THEN
         CALL STRASC(FN,ASCREC(IDP(JT+1)),IDL(JT+1))
         IF (KWS(JT+1).EQ.'TERMINAL') FN = ZTRMOU
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         IF (STAT.NE.0) GOTO 999
      ENDIF
      !
      ! CALL THE REPORT ROUTINE
      !
      CALL SELRPT
      GOTO 900
      !
      ! ADIOS
      !
900   NUMATT = 0
      IF (JT.NE.0) THEN
         CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
         CALL MSG(' ','SELECT COMPLETED.',' ')
      ENDIF
999   RETURN  1
   END SUBROUTINE SELECT



   SUBROUTINE JOIREL(*)
      !!
      !!  THIS ROUTINE FINDS THE JOIN OF TWO RELATIONS BASED UPON JOINING
      !!  TWO ATTRIBUTES.  THE RESULT FROM THIS PROCESS IS A
      !!  RELATION WHICH HAS TUPLES CONTRUCTED FROM BOTH RELATIONS
      !!  WHERE THE SPECIFIED ATTRIBUTES MATCH AS REQUESTED.
      !!
      !!  THE SYNTAX FOR THE JOIN COMMAND IS:
      !!
      !!  JOIN REL1 USING ATT1 WITH REL2 USING ATT2 FORMING REL3 WHERE EQ
      !!
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, DMFLAG
      USE RM_BufferData, only: BUFFER
      USE Extern, only: IMSG, AMSG, MSG
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, KWS, ITEMS, EQKEYW, LXSREC
      USE DateTime, only : RMDATE
      USE Message, only : WARN
      USE RM_Text, only : BLANK, NONE
      USE Utils, only : ZMOVE

      INCLUDE 'rmatts.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'whcom.inc.f90'
      !
      INTEGER :: PTABLE
      LOGICAL :: EQ
      LOGICAL :: NE
      INCLUDE 'dclar1.inc'
      INCLUDE 'dclar3.inc'
      !  LOCAL ARRAYS AND VARIABLES :
      !
      INCLUDE 'ptbl.inc'
      ! PTABLE (MATRIX 7) USED TO CONTROL POINTERS
      ! PTBL1-- ATTRIBUTE NAME
      ! PTBL2-- ATTRIBUTE LOCATION IN RELATION 1
      ! PTBL3-- ATTRIBUTE LOCATION IN RELATION 2
      ! PTBL4-- ATTRIBUTE LOCATION IN RELATION 3
      ! PTBL5-- LENGTH IN WORDS
      ! PTBL6-- ATTRIBUTE TYPE
      !

      INTEGER :: I, I1, I2, ICT, ISTAT, J, K, KATT3, KEYCOL, KEYLEN, KEYTYP, KQ1, KQ3, NATT1, NATT2, NATT3
      INTEGER :: NCOL1, NCOL2, NCOL3, NK, NWORDS

      INTEGER LOCBOO, LOCREL, LOCPRM, LOCATT, BLKLOC

      !
      ! CHECK FOR A DATABASE
      !
      IF (.NOT.DFLAG) THEN
         CALL WARN(2)
         GOTO 999
      ENDIF
      !
      !
      ! MAKE SURE THE DATABASE MAY BE MODIFIED
      !
      IF(.NOT.DMFLAG) THEN
         CALL WARN(8)
         GO TO 999
      ENDIF
      !
      !  EDIT COMMAND SYNTAX
      !
      CALL BLKCLN
      IF(.NOT.EQKEYW(3,'USING'))   GO TO 900
      IF(.NOT.EQKEYW(5,'WITH'))    GO TO 900
      IF(.NOT.EQKEYW(7,'USING'))   GO TO 900
      IF(.NOT.EQKEYW(9,'FORMING')) GO TO 900
      !
      !  SET DEFAULT WHERE CONDITION (EQ OR NK = 2)
      !
      NK = 2
      IF(ITEMS.LE.10) GO TO 50
      !
      !  CHECK WHERE COMPARISON.
      !
      IF(.NOT.EQKEYW(11,'WHERE')) GO TO 900
      NK = LOCBOO(KWS(12))
      IF(NK.EQ.0) GO TO 900
      IF(NK.EQ.1) GO TO 900
      !
      !  KEYWORD SYNTAX OKAY
      !
50    CALL LXSREC(2,RNAME1,ZC)
      I = LOCREL(RNAME1)
      IF(I.NE.0) THEN
         ! MISSING FIRST RELATION.
         CALL WARN(1,RNAME1)
         GO TO 999
      ENDIF
      !
      !  SAVE DATA ABOUT RELATION 1
      !
      I1 = LOCPRM(RNAME1,1)
      IF(I1.NE.0) THEN
         CALL WARN(9,RNAME1)
         GO TO 999
      ENDIF
      NCOL1 = NCOL
111   NATT1 = NATT
      CALL ZMOVE(RPW1,RPW)
      CALL ZMOVE(MPW1,MPW)
      !
      !  CHECK THE COMPARISON ATTRIBUTE.
      !
      CALL LXSREC(4,ANAME1,ZC)
      I = LOCATT(ANAME1,RNAME1)
      IF(I.NE.0) THEN
         CALL WARN(3,ANAME1,RNAME1)
         GO TO 999
      ENDIF
      !
      ! CHECK SECOND RELATION
      !
      CALL LXSREC(6,RNAME2,ZC)
      I = LOCREL(RNAME2)
      IF(I.NE.0) THEN
         ! MISSING SECOND RELATION.
         CALL WARN(1,RNAME2)
         GO TO 999
      ENDIF
      !
      !  SAVE DATA ABOUT RELATION 2
      !
      I2 = LOCPRM(RNAME2,1)
      IF(I2.NE.0) THEN
         CALL WARN(9,RNAME2)
         GO TO 999
      ENDIF
      NCOL2 = NCOL
      NATT2 = NATT
      CALL ZMOVE(RPW2,RPW)
      CALL ZMOVE(MPW2,MPW)
      !
      !  CHECK THE COMPARISON ATTRIBUTE.
      !
      CALL LXSREC(8,ANAME2,ZC)
      I = LOCATT(ANAME2,RNAME2)
      IF(I.NE.0) THEN
         CALL WARN(3,ANAME2,RNAME2)
         GO TO 999
      ENDIF
      !
      !  CHECK FOR LEGAL RNAME3
      !
      IF(.NOT.TOKTYP(10,KXNAME)) THEN
         CALL WARN(7,ASCREC(IDP(10)))
         GO TO 999
      ENDIF
      !
      !  CHECK FOR DUPLICATE RELATION 3
      !
      CALL LXSREC(10,RNAME3,ZC)
      I = LOCREL(RNAME3)
      IF(I.EQ.0) THEN
         CALL WARN(5,RNAME3)
         GO TO 999
      ENDIF
      !
      !  CHECK USER READ SECURITY
      !
      IF((I1.NE.0).OR.(I2.NE.0)) GO TO 999
      !
      !  RELATION NAMES OKAY -- CHECK THE ATTRIBUTES
      !
      !  SET UP PTABLE IN BLOCK 7
      !
      CALL BLKDEF(7,PTBLL,NATT1+NATT2)
      PTABLE = BLKLOC(7)
      NATT3 = 0
      ICT = 1
      !
      !  STORE DATA FROM RELATION 1 IN PTABLE
      !
      I = LOCATT(BLANK,RNAME1)
      DO I=1,NATT1
         CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 500
         NATT3 = NATT3 + 1
         CALL ZMOVE(BUFFER(PTABLE),ATTNAM)
         BUFFER(PTABLE+PTBL2-1) = ATTCOL
         BUFFER(PTABLE+PTBL4-1) = ICT
         NWORDS = ATTWDS
         BUFFER(PTABLE+PTBL5-1) = ATTLEN
         IF(NWORDS.EQ.0) NWORDS = 1
         ICT = ICT + NWORDS
         BUFFER(PTABLE+PTBL6-1) = ATTYPE
         BUFFER(PTABLE+PTBL7-1) = ATTFOR
         PTABLE = PTABLE + PTBLL
500      CONTINUE
      END DO
      !
      !  STORE DATA FROM RELATION 2 IN PTABLE
      !
      KATT3 = NATT3
      I = LOCATT(BLANK,RNAME2)
      DO I=1,NATT2
         CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 550
         !
         !  FIRST CHECK TO SEE IF ATTRIBUTE IS ALREADY IN PTABLE.
         !
         KQ1 = BLKLOC(7) - PTBLL
         DO J=1,KATT3
            KQ1 = KQ1 + PTBLL
            IF(BUFFER(KQ1+PTBL3-1).NE.0) GO TO 520
            IF (EQ(BUFFER(KQ1),ATTNAM)) THEN
               CALL MSG('W','COLUMN ''','+')
               CALL AMSG(ATTNAM,-ZC,'+')
               CALL MSG(' ',''' IS DUPLICATED.  ','+')
               CALL MSG(' F','YOU SHOULD RENAME IT.',' ')
               GO TO 530
            ENDIF
520         CONTINUE
         END DO
         !
         !  ADD THE DATA TO PTABLE.
         !
530      NATT3 = NATT3 + 1
         CALL ZMOVE(BUFFER(PTABLE),ATTNAM)
         BUFFER(PTABLE+PTBL3-1) = ATTCOL
         BUFFER(PTABLE+PTBL4-1) = ICT
         NWORDS = ATTWDS
         BUFFER(PTABLE+PTBL5-1) = ATTLEN
         IF(NWORDS.EQ.0) NWORDS = 1
         ICT = ICT + NWORDS
         BUFFER(PTABLE+PTBL6-1) = ATTYPE
         BUFFER(PTABLE+PTBL7-1) = ATTFOR
         PTABLE = PTABLE + PTBLL
550      CONTINUE
      END DO
      ICT = ICT - 1
      !
      !  PTABLE IS CONSTRUCTED
      !
      !  NOW CREATE ATTRIBUTE AND RELATION TABLES AND THE RELATION
      !
      IF(ICT.GT.MAXCOL) GO TO 930
      !
      !  SET UP THE WHERE CLAUSE FOR THE JOIN.
      !
      I = LOCATT(ANAME2,RNAME2)
      CALL ATTGET(ISTAT)
      IF(ATTWDS.GT.300) GO TO 940
      KEYCOL = ATTCOL
      KEYTYP = ATTYPE
      KEYLEN = ATTLEN
      NBOO = 1
      BOO(1) = WHAND
      I = LOCATT(ANAME1,RNAME1)
      CALL ATTGET(ISTAT)
      KATTP(1) = ATTCOL
      KATTL(1) = ATTLEN
      !
      !  MAKE SURE THE ATTRIBUTE TYPES MATCH.
      !
      IF(KEYTYP.NE.ATTYPE) GO TO 920
      IF(KEYLEN.NE.ATTLEN) GO TO 910
      KATTY(1) = ATTYPE
      IF(KEYTYP.EQ.KZIVEC) KATTY(1) = KZINT
      IF(KEYTYP.EQ.KZRVEC) KATTY(1) = KZREAL
      IF(KEYTYP.EQ.KZDVEC) KATTY(1) = KZDOUB
      IF(KEYTYP.EQ.KZIMAT) KATTY(1) = KZINT
      IF(KEYTYP.EQ.KZRMAT) KATTY(1) = KZREAL
      IF(KEYTYP.EQ.KZDMAT) KATTY(1) = KZDOUB
      KOMTYP(1) = NK
      KOMPOS(1) = 1
      KOMLEN(1) = 1
      KOMPOT(1) = 1
      KSTRT = ATTKEY
      IF(NK.NE.2) KSTRT = 0
      MAXTU = ALL9S
      LIMTU = ALL9S
      !
      !  SET UP RELATION TABLE.
      !
      CALL ZMOVE(NAME,RNAME3)
      RDATE = RMDATE()
      NCOL = ICT
      NCOL3 = ICT
      NATT = NATT3
      NTUPLE = 0
      RSTART = 0
      REND = 0
      CALL ZMOVE(RPW,RPW1)
      CALL ZMOVE(MPW,MPW1)
      IF(EQ(RPW,NONE).AND.NE(RPW2,NONE)) CALL ZMOVE(RPW,RPW2)
      IF(EQ(MPW,NONE).AND.NE(MPW2,NONE)) CALL ZMOVE(MPW,MPW2)
      CALL RELADD
      !
      CALL ATTNEW(NAME,NATT)
      PTABLE = BLKLOC(7)
      DO K=1,NATT3
         CALL ZMOVE(ATTNAM,BUFFER(PTABLE))
         CALL ZMOVE(RELNAM,NAME)
         ATTCOL = BUFFER(PTABLE+PTBL4-1)
         ATTLEN = BUFFER(PTABLE+PTBL5-1)
         ATTYPE = BUFFER(PTABLE+PTBL6-1)
         ATTFOR = BUFFER(PTABLE+PTBL7-1)
         ATTKEY = 0
         CALL ATTADD
         PTABLE = PTABLE + PTBLL
      END DO
      !
      !  CALL JOIN TO CONSTRUCT MATN3
      !
      CALL BLKDEF(8,MAXCOL,1)
      KQ3 = BLKLOC(8)
      PTABLE = BLKLOC(7)
      I = LOCREL(RNAME2)
      CALL JOIN(RNAME1,RNAME3,BUFFER(KQ3),NCOL3,NATT3,BUFFER(PTABLE), &
         KEYCOL,KEYTYP)
      GO TO 999
      !
      !
      !  SYNTAX ERROR IN JOIN COMMAND
      !
900   CALL WARN(4)
      !
      !
      !  MISMATCHED DATA TYPES.
      !
910   CALL MSG('E','JOIN COLUMNS HAVE DIFFERENT LENGTHS.',' ')
      GO TO 999
      !
920   CALL MSG('E','JOIN COLUMNS HAVE DIFFERENT TYPES.',' ')
      GO TO 999
      !
930   CALL WARN(15)
      GO TO 999
      !
940   CALL MSG('E','JOIN COLUMN IS TOO LONG.',' ')
      GO TO 999
      !
      !  DONE WITH JOIN
      !
999   CALL BLKCLR(7)
      CALL BLKCLR(8)
      RETURN 1
   END SUBROUTINE JOIREL


   SUBROUTINE PJECT(*)
      !!
      !! PERFORM PHYSICAL PROJECTIONS ON EXISTING RELATIONS.
      !!
      !! PROJECT RNAME2 FROM RNAME1 USING ATTR1 ATTR2...ATTRN
      !!   ... WHERE CONDITION
      !!
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, DMFLAG, RMSTAT
      USE RM_BufferData, only: BUFFER
      USE RM_Buffer, only: ADDDAT
      USE DateTime, only : RMDATE
      USE Extern, only: IMSG, MSG
      Use Lexer, only: KXNAME, TOKTYP, IDP, ITEMS, EQKEYW, LFIND
      Use Lexer, only: LXSREC
      USE Message, only : WARN
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INCLUDE 'rimptr.inc'
      INCLUDE 'whcom.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc.f90'
      !
      !
      INTEGER :: STATUS
      INTEGER :: ATNCOL
      LOGICAL :: PJALL
      INTEGER :: TEMP(Z)
      INCLUDE 'dclar1.inc'

      INTEGER :: I, IERR, IPOINT, J, K, KK, KKX, KNEW, KQ, KQ7, KQ8, LENF, LENGTH, LENT
      INTEGER :: LPAG, MEND, MSTART, NOATTS, NOCOLS, UC, UE
      LOGICAL :: XTMP

      INTEGER LOCREL, LOCPRM, LOCATT, SELWHR, BLKLOC
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
         CALL WARN(8)
         GO TO 999
      ENDIF
      !
      ! CHECK SYNTAX AND RELATION NAMES
      !
      CALL BLKCLN
      IF(.NOT.EQKEYW(3,'FROM')) THEN
         CALL WARN(4,BLANK,BLANK)
         GOTO 999
      ENDIF
      UC = LFIND(5,ITEMS-4,'USING')
      CALL LXSREC(4,RNAME1,ZC)
      I = LOCREL(RNAME1)
      LENF = NCOL
      IF(I.NE.0) THEN
         ! RNAME1 DOES NOT EXIST
         CALL WARN(1,RNAME1)
         GO TO 999
      ENDIF
      !
      !
      IF(.NOT.TOKTYP(2,KXNAME)) THEN
         ! !CALL WARN(7,ASCTXT(IDP(2)),0) ! ASCTXT is a subroutine ???
         TEMP(1) = KXNAME
         CALL WARN(7, TEMP)
         GO TO 999
      ENDIF
      CALL LXSREC(2,RNAME2,ZC)
      I = LOCREL(RNAME2)
      IF(I.EQ.0) THEN
         ! DUPLICATE RELATION NAME ENCOUNTERED
         CALL WARN(5,RNAME2)
         GO TO 999
      ENDIF
      !
      !  CHECK USER READ SECURITY
      !
      I = LOCREL(RNAME1)
      I = LOCPRM(RNAME1,1)
      IF(I.NE.0) THEN
         CALL WARN(9,RNAME1)
         GO TO 999
      ENDIF
      NS = 0
      NID = RSTART
      !
      !  SET UP THE WHERE CLAUSE
      !
      K = LFIND(5,ITEMS-4,'WHERE')
      NBOO = 0
      LIMTU = ALL9S
      RMSTAT = 0
      KKX = K
      IF(K.NE.0) XTMP = SELWHR(K,ITEMS-K+1)
      IF(RMSTAT.NE.0) GO TO 999
      !
      !  CHECK THE ATTRIBUTES AND BUILD POINTER ARRAY - POS. 7
      !
      CALL BLKDEF(7,LENF,1)
      KQ7 = BLKLOC(7) - 1
      NOCOLS = 0
      UE = ITEMS
      IF(K.NE.0) UE = K - 1
      IF (UC.EQ.0) THEN
         !
         !    ALL ATTRIBUTES
         !
         PJALL = .TRUE.
         NOATTS = NATT
      ELSE
         !
         !    SELECTED ATTRIBUTES - CHECK THEM
         !
         IERR = 0
         DO I=UC+1,UE
            CALL LXSREC(I,ANAME,ZC)
            IF(LOCATT(ANAME,NAME).NE.0) THEN
               CALL WARN(3,ANAME,NAME)
               IERR = 1
            ENDIF
            IF(IERR.EQ.1) GO TO 999
         END DO
         PJALL = .FALSE.
         NOATTS = UE - UC
      ENDIF
      CALL ATTNEW(RNAME2,NOATTS)
      !
      DO I=1,NOATTS
         IF(PJALL) THEN
            STATUS = LOCATT(BLANK,NAME)
            DO J=1,I
               CALL ATTGET(STATUS)
               IF(STATUS.NE.0) GO TO 160
            END DO
         ELSE
            CALL LXSREC(I+UC,ANAME,ZC)
            IERR = LOCATT(ANAME,NAME)
            CALL ATTGET(STATUS)
         ENDIF
         !
         ATNCOL = NOCOLS + 1
         IF(ATTWDS.GT.0) THEN
            !
            !    FIXED LENGTH
            !
            KQ = KQ7 + ATTCOL
            DO KK=1,ATTWDS
               NOCOLS = NOCOLS + 1
               BUFFER(KQ) = NOCOLS
               KQ = KQ + 1
            END DO
         ELSE
            !
            !    VARIABLE LENGTH
            !
            NOCOLS = NOCOLS + 1
            BUFFER(KQ7+ATTCOL) = -NOCOLS
         ENDIF
         CALL ZMOVE(RELNAM,RNAME2)
         ATTCOL = ATNCOL
         ATTKEY = 0
         CALL ATTADD
160      CONTINUE
      END DO
      !
      !  SET UP RELTBLE
      !
      CALL ZMOVE(NAME,RNAME2)
      RDATE = RMDATE()
      NCOL = NOCOLS
      NATT = NOATTS
      NTUPLE = 0
      RSTART = 0
      REND = 0
      CALL RELADD
      !
      ! 1 IS INPUT BUFFER, 2 IS OUTPUT BUFFER, 8 IS OUTPUT TUPLE
      !
      LPAG = MAXCOL + 2
      CALL BLKDEF(8,LPAG,1)
      KQ8 = BLKLOC(8)
      !
      ! LOOP THRU THOSE TUPLES
      !
      RMSTAT = 0
      I = LOCREL(RNAME1)
      KNEW = 0
      MSTART = 0
      MEND = 0
      !
170   CALL RMLOOK(IPOINT,1,1,LENGTH)
      IF (RMSTAT.EQ.0) THEN
         CALL PRJTUP(BUFFER(KQ7+1),LENF,NOCOLS,BUFFER(IPOINT), &
            BUFFER(KQ8),LENT)
         CALL ADDDAT(2,MEND,BUFFER(KQ8),LENT)
         IF(MSTART.EQ.0)MSTART = MEND
         KNEW = KNEW + 1
         GO TO 170
      ENDIF
      !
      I = LOCREL(RNAME2)
      CALL RELGET(STATUS)
      NTUPLE = KNEW
      RSTART = MSTART
      REND = MEND
      CALL RELPUT
      CALL MSG(' ','PROJECT COMPLETED,','+')
      CALL IMSG(KNEW,6,'+')
      CALL MSG(' ',' ROWS GENERATED.',' ')
      !
      !
999   CALL BLKCLR(7)
      CALL BLKCLR(8)
      RETURN 1
   END SUBROUTINE PJECT


   SUBROUTINE TUPLRC(OPCODE,*)
      !!
      !! PERFORM TUPLE RELATIONAL CALCULUS
      !!
      !! OPCODE MAY BE: UNION, INTERSECT, OR SUBTRACT
      !!
      !! FORMATS:
      !!
      !! UNION     REL1 WITH REL2 FORMING REL3 USING ATTR1 ATTR2...ATTR-N
      !! INTERSECT REL1 WITH REL2 FORMING REL3 USING ATTR1 ATTR2...ATTR-N
      !! SUBTRACT  REL2 FROM REL1 FORMING REL3 USING ATTR1 ATTR2...ATTR-N
      !!
      USE RM_Parameters
      USE RM_Globals, only : DFLAG, DMFLAG
      USE RM_BufferData, only: BUFFER
      USE DateTime, only : RMDATE
      USE Extern, only: MSG
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW, LXSREC
      USE Message, only: WARN
      USE RM_Text, only : BLANK, NONE
      USE Utils, only : ZMOVE

      CHARACTER(len=*), intent(in) :: OPCODE

      INTEGER, PARAMETER :: OPIS=1, OPUN=2, OPSB=3
      !
      !
      INCLUDE 'rmatts.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'whcom.inc.f90'
      !
      INCLUDE 'ptbl.inc'
      INCLUDE 'dclar1.inc'
      INCLUDE 'dclar3.inc'

      INTEGER :: I, I1, I2, ICHK1, ICHK2, ICT, IJ, ISTAT, J, K, KATT3, KEYCOL, KQ1, KQ3, NATT1, NATT2, NATT3
      INTEGER :: NCOL1, NCOL2, NCOL3, NTUP1, NTUP2, NTUPSQ, OP, KEYTYP, NATTT, NWORDS

      INTEGER :: PTABLE
      LOGICAL :: EQ
      LOGICAL :: NE
      LOGICAL :: GETKCA, REQKCA
      INTEGER LOCREL, LOCPRM, LOCATT, BLKLOC

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
         CALL WARN(8)
         GO TO 999
      ENDIF
      !
      ! GET OPCODE TO INTEGER
      !
      OP = 0
      IF (OPCODE.EQ.'INTERSECT') OP = OPIS
      IF (OPCODE.EQ.'UNION')     OP = OPUN
      IF (OPCODE.EQ.'SUBTRACT')  OP = OPSB
      IF (OP.EQ.0) GOTO 900

      !
      !  EDIT COMMAND SYNTAX
      !
      CALL BLKCLN
      NS = 0
      IF(.NOT.EQKEYW(3,'WITH') .AND. .NOT.EQKEYW(3,'FROM')) GO TO 900
      IF(.NOT.EQKEYW(5,'FORMING')) GO TO 900
      IF(ITEMS.GT.6 .AND. .NOT.EQKEYW(7,'USING')) GO TO 900
      !
      !  KEYWORD SYNTAX OKAY
      !
      IF (OP.NE.OPSB) THEN
         CALL LXSREC(2,RNAME1,ZC)
         CALL LXSREC(4,RNAME2,ZC)
      ELSE
         CALL LXSREC(4,RNAME1,ZC)
         CALL LXSREC(2,RNAME2,ZC)
      ENDIF
      I = LOCREL(RNAME1)
      IF(I.NE.0) THEN
         ! MISSING FIRST RELATION.
         CALL WARN(1,RNAME1)
         GO TO 999
      ENDIF
      !
      !  SAVE DATA ABOUT RELATION 1
      !
      I1 = LOCPRM(RNAME1,1)
      IF(I1.NE.0) THEN
         CALL WARN(9,RNAME1)
         GO TO 999
      ENDIF
      NCOL1 = NCOL
      NATT1 = NATT
      CALL ZMOVE(RPW1,RPW)
      CALL ZMOVE(MPW1,MPW)
      NTUP1 = NTUPLE
      I = LOCREL(RNAME2)
      IF(I.NE.0) THEN
         !  MISSING SECOND RELATION.
         CALL WARN(1,RNAME2)
         GO TO 999
      ENDIF
      !
      !  SAVE DATA ABOUT RELATION 2
      !
      I2 = LOCPRM(RNAME2,1)
      IF(I2.NE.0) THEN
         CALL WARN(9,RNAME2)
         GO TO 999
      ENDIF
      NCOL2 = NCOL
      NATT2 = NATT
      CALL ZMOVE(RPW2,RPW)
      CALL ZMOVE(MPW2,MPW)
      NTUP2 = NTUPLE
      NTUPSQ = (NTUP1+1)*(NTUP2+1)
      !
      !  CHECK FOR LEGAL RNAME3
      !
      IF(.NOT.TOKTYP(6,KXNAME)) THEN
         CALL WARN(7,ASCREC(IDP(6)))
         GO TO 999
      ENDIF
      !
      !  CHECK FOR DUPLICATE RELATION 3
      !
      CALL LXSREC(6,RNAME3,ZC)
      I = LOCREL(RNAME3)
      IF(I.EQ.0) THEN
         CALL WARN(5,RNAME3)
         GO TO 999
      ENDIF
      !
      !  CHECK USER READ SECURITY
      !
      IF((I1.NE.0).OR.(I2.NE.0)) GO TO 999
      !
      !  RELATION NAMES OKAY -- CHECK THE ATTRIBUTES
      !
      !  SET UP PTABLE IN BUFFER 7
      !
      NATTT = NATT1 + NATT2
      IF (OP.EQ.OPSB) NATTT = NATT1
      CALL BLKDEF(7,PTBLL,NATTT)
      PTABLE = BLKLOC(7)
      NATT3 = 0
      IF(ITEMS.EQ.6) GO TO 500
      !
      !  OPERATE ON SOME OF THE ATTRIBUTES
      !
      IF(ITEMS-7.GT.NATTT) THEN
         CALL MSG('E','TOO MANY COLUMNS SPECIFIED.',' ')
         GO TO 999
      ENDIF
      IJ = 1
      DO I=8,ITEMS
         !
         !  RETRIEVE ATTRIBUTE LENGTH FOR OLD ATTRIBUTE
         !
         !
         ! GET THE ATTRIBUTE DATA
         !
         CALL LXSREC(I,ANAME,ZC)
         ICHK1 = LOCATT(ANAME,RNAME1)
         IF(ICHK1.EQ.0) GO TO 300
         ICHK2 = LOCATT(ANAME,RNAME2)
         IF(OP.NE.OPSB .AND. ICHK2.EQ.0) GO TO 300
         !
         !  ATTRIBUTE WAS NOT FOUND
         !
         CALL WARN(3,ANAME,BLANK)
         GO TO 999
         !
         !  ATTRIBUTE IS OKAY -- SET UP PTABLE
         !
300      CALL ATTGET(ISTAT)
         NATT3 = NATT3 + 1
         CALL ZMOVE(BUFFER(PTABLE),ANAME)
         BUFFER(PTABLE+PTBL4-1) = IJ
         NWORDS = ATTWDS
         BUFFER(PTABLE+PTBL5-1) = ATTLEN
         IF(NWORDS.EQ.0) NWORDS = 1
         IJ = IJ + NWORDS
         BUFFER(PTABLE+PTBL6-1) = ATTYPE
         BUFFER(PTABLE+PTBL7-1) = ATTFOR
         IF(ICHK1.EQ.0) THEN
            BUFFER(PTABLE+PTBL2-1) = ATTCOL
            ICHK2 = LOCATT(ANAME,RNAME2)
         ELSE
            BUFFER(PTABLE+PTBL2-1) = 0
         ENDIF
         IF(ICHK2.EQ.0) THEN
            CALL ATTGET(ISTAT)
            BUFFER(PTABLE+PTBL3-1) = ATTCOL
         ELSE
            BUFFER(PTABLE+PTBL3-1) = 0
         ENDIF
         PTABLE = PTABLE + PTBLL
         !
      END DO
      ICT = IJ - 1
      GO TO 555
      !
      !  OPERATION IS ON ALL ATTRIBUTES
      !
500   CONTINUE
      ICT = 1
      !
      !  STORE DATA FROM RELATION 1 IN PTABLE
      !
      I = LOCATT(BLANK,RNAME1)
      DO I=1,NATT1
         CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 515
         NATT3 = NATT3 + 1
         CALL ZMOVE(BUFFER(PTABLE),ATTNAM)
         BUFFER(PTABLE+PTBL2-1) = ATTCOL
         BUFFER(PTABLE+PTBL4-1) = ICT
         NWORDS = ATTWDS
         BUFFER(PTABLE+PTBL5-1) = ATTLEN
         IF(NWORDS.EQ.0) NWORDS = 1
         ICT = ICT + NWORDS
         BUFFER(PTABLE+PTBL6-1) = ATTYPE
         BUFFER(PTABLE+PTBL7-1) = ATTFOR
         PTABLE = PTABLE + PTBLL
515      CONTINUE
      END DO
      !
      !  STORE DATA FROM RELATION 2 IN PTABLE
      !
      KATT3 = NATT3
      I = LOCATT(BLANK,RNAME2)
      DO I=1,NATT2
         CALL ATTGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 550
         !
         !  FIRST CHECK TO SEE IF ATTRIBUTE IS ALREADY IN PTABLE.
         !
         KQ1 = BLKLOC(7) - PTBLL
         DO J=1,KATT3
            KQ1 = KQ1 + PTBLL
            IF(BUFFER(KQ1+PTBL3-1).NE.0) GO TO 520
            IF(EQ(BUFFER(KQ1),ATTNAM)) GO TO 530
520         CONTINUE
         END DO
         !
         !  NOT THERE -- PUT IT IN.
         !
         IF (OP.EQ.OPSB) GOTO 550
         NATT3 = NATT3 + 1
         CALL ZMOVE(BUFFER(PTABLE),ATTNAM)
         BUFFER(PTABLE+PTBL3-1) = ATTCOL
         BUFFER(PTABLE+PTBL4-1) = ICT
         NWORDS = ATTWDS
         BUFFER(PTABLE+PTBL5-1) = ATTLEN
         IF(NWORDS.EQ.0) NWORDS = 1
         ICT = ICT + NWORDS
         BUFFER(PTABLE+PTBL6-1) = ATTYPE
         BUFFER(PTABLE+PTBL7-1) = ATTFOR
         PTABLE = PTABLE + PTBLL
         GO TO 550
         !
         !  ALREADY THERE -- CHANGE THE 2ND POINTER
         !
530      CONTINUE
         BUFFER(KQ1+PTBL3-1) = ATTCOL
550      CONTINUE
      END DO
      ICT = ICT - 1
      !
      !  DONE LOADING PTABLE
      !
      !  SEE IF THERE ARE ANY COMMON ATTRIBUTES.
      !
555   PTABLE = BLKLOC(7)
      DO I = 1,NATT3
         IF((BUFFER(PTABLE+2).NE.0) &
            .AND.(BUFFER(PTABLE+3).NE.0)) GO TO 600
         PTABLE = PTABLE + PTBLL
      END DO
      !
      !  NO COMMON ATTRIBUTES
      !
      CALL MSG('E','TABLES HAVE NO COMMON ATTRIBUTES.',' ')
      GO TO 999
      !
      !  PTABLE IS CONSTRUCTED
      !
      !  NOW CREATE ATTRIBUTE AND RELATION TABLES AND THE RELATION
      !
600   CONTINUE
      IF(ICT.GT.MAXCOL) GO TO 910
      !
      !  SET UP THE WHERE CLAUSE FOR THE OPERATION
      !  THIS IS A DUMMY WHERE CLAUSE USED ONLY BY THE KEY PROCESSING
      !  PTABLE IS NOW POINTING AT THE FIRST COMMON ATTRIBUTE.
      !
      KEYCOL = BUFFER(PTABLE+PTBL3-1)
      KEYTYP = BUFFER(PTABLE+PTBL6-1)
      NBOO = -1
      KATTL(1) = BUFFER(PTABLE+PTBL5-1)
      KATTY(1) = KEYTYP
      IF(KEYTYP.EQ.KZIVEC) KATTY(1) = KZINT
      IF(KEYTYP.EQ.KZRVEC) KATTY(1) = KZREAL
      IF(KEYTYP.EQ.KZDVEC) KATTY(1) = KZDOUB
      IF(KEYTYP.EQ.KZIMAT) KATTY(1) = KZINT
      IF(KEYTYP.EQ.KZRMAT) KATTY(1) = KZREAL
      IF(KEYTYP.EQ.KZDMAT) KATTY(1) = KZDOUB
      KOMPOS(1) = 1
      KSTRT = 0
      MAXTU = ALL9S
      LIMTU = ALL9S
      !
      !  SET UP RELATION TABLE.
      !
      CALL ZMOVE(NAME,RNAME3)
      RDATE = RMDATE()
      NCOL = ICT
      NCOL3 = ICT
      NATT = NATT3
      NTUPLE = 0
      RSTART = 0
      REND = 0
      CALL ZMOVE(RPW,RPW1)
      CALL ZMOVE(MPW,MPW1)
      IF (OP.NE.OPSB) THEN
         IF(EQ(RPW,NONE)) CALL ZMOVE(RPW,RPW2)
         IF(EQ(MPW,NONE)) CALL ZMOVE(MPW,MPW2)
      ENDIF
      CALL RELADD
      !
      CALL ATTNEW(NAME,NATT)
      PTABLE = BLKLOC(7)
      DO K=1,NATT3
         CALL ZMOVE(ATTNAM,BUFFER(PTABLE))
         CALL ZMOVE(RELNAM,NAME)
         ATTCOL = BUFFER(PTABLE+PTBL4-1)
         ATTLEN = BUFFER(PTABLE+PTBL5-1)
         ATTYPE = BUFFER(PTABLE+PTBL6-1)
         ATTFOR = BUFFER(PTABLE+PTBL7-1)
         ATTKEY = 0
         CALL ATTADD
         PTABLE = PTABLE + PTBLL
      END DO
      !
      ! INTERSECT USES COMMON ATTRIBUTE IN RELATION 1
      ! SUBTRACT USES COMMON ATTRIBUTE IN RELATION 2
      ! UNION USES BOTH
      !
      IF (OP.EQ.OPSB) GOTO 800
      !
      ! LOOK FOR A KEYED COMMON ATTRIBUTE IN RELATION 1
      !
      REQKCA = NTUPSQ .GT. 2500
      IF (GETKCA(PTABLE,NATT3,RNAME1,REQKCA)) THEN
         KSTRT = ATTKEY
         NS = 2
         KATTL(1) = BUFFER(PTABLE+PTBL5-1)
         KATTY(1) = BUFFER(PTABLE+PTBL6-1)
         KEYCOL = BUFFER(PTABLE+PTBL3-1)
      ENDIF
      !
      ! CALL PROPER OPERATOR ROUTINE
      !
      CALL BLKDEF(8,MAXCOL,1)
      KQ3 = BLKLOC(8)
      PTABLE = BLKLOC(7)
      I = LOCREL(RNAME2)
      IF (OP.EQ.OPIS) THEN
         CALL ISECT(RNAME1,RNAME3,BUFFER(KQ3), &
            NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP)
         GOTO 999
      ELSE IF (OP.EQ.OPUN) THEN
         CALL UNION(RNAME1,RNAME3,BUFFER(KQ3), &
            NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP,1)
      ENDIF
      !
      ! GET A KEYED COMMON ATTRIBUTE IN RELATION 2
      !
800   CALL BLKCLR(8)
      KSTRT = 0
      NS = 0
      KATTL(1) = 0
      KATTY(1) = 0
      KEYCOL = 0
      !
      REQKCA = NTUPSQ .GT. 2500
      IF (GETKCA(PTABLE,NATT3,RNAME2,REQKCA)) THEN
         KSTRT = ATTKEY
         NS = 2
         KATTL(1) = BUFFER(PTABLE+PTBL5-1)
         KATTY(1) = BUFFER(PTABLE+PTBL6-1)
         KEYCOL = BUFFER(PTABLE+PTBL2-1)
      ENDIF
      !
      !  CALL UNION2 TO CONSTRUCT THE REST OF MATN3
      !
      CALL BLKDEF(8,MAXCOL,1)
      KQ3 = BLKLOC(8)
      PTABLE = BLKLOC(7)
      I = LOCREL(RNAME1)
      IF (OP.EQ.OPUN) THEN
         CALL UNION(RNAME2,RNAME3,BUFFER(KQ3), &
            NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP,2)
      ELSE IF (OP.EQ.OPSB) THEN
         CALL SUBTRC(RNAME2,RNAME3,BUFFER(KQ3), &
            NCOL3,NATT3,BUFFER(PTABLE),KEYCOL,KEYTYP)
      ENDIF
      GO TO 999
      !
      !  SYNTAX ERROR
      !
900   CALL WARN(4)
      !
      !
      !  TUPLE LENGTH EXCEEDS MAXCOL
      !
910   CALL WARN(15)
      !
      ! DONE
      !
999   CALL BLKCLR(7)
      CALL BLKCLR(8)
      RETURN 1
   END SUBROUTINE TUPLRC

   SUBROUTINE LSTREL(*)

      USE RM_Parameters
      USE RM_Globals, only : DFLAG, DBNAME, USERID, OWNER, KARRC, KARDT
      USE DateTime, only: RMTIME, RMDATE
      USE Extern, only: DMSG, IMSG, AMSG, MSG
      USE Formater, only: FMTDEC
      USE Lexer, only: ITEMS, EQKEYW, LXSREC
      USE Message, only : WARN
      USE RM_Text, only : BLANK, NONE
      USE Utils, only : ZMOVE

      !
      ! SUMMARIZE THE USERS DEFINITION OF A RELATION
      !
      !
      INCLUDE 'rmatts.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tuplel.inc'

      INTEGER :: STATUS, I, NC, MCHAR, NLINK, NP, NREL, NWORDS, TDAY, TTIM, NCHAR

      LOGICAL :: ALLREL
      CHARACTER(len=4) :: KEY,CRPW,CMPW
      INTEGER :: FMTSTR(3)

      LOGICAL :: EQ, NE
      INTEGER :: LOCREL, LOCATT, LOCLNK
      INCLUDE 'dclar1.inc'
      !
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
      TDAY = RMDATE()
      TTIM = RMTIME()
      I = LOCREL(BLANK)
      NP = 0
      IF(I.NE.0) THEN
         CALL MSG('W','THERE ARE NO TABLES.',' ')
         GO TO 999
      ENDIF
      !
      IF(ITEMS.GT.2) THEN
         CALL WARN(4)
         GOTO 999
      ENDIF
      IF(ITEMS.EQ.2) GO TO 1000
      !
      !   LISTREL (WITH NO RELATION SPECIFIED)
      !
100   CALL RELGET(STATUS)
      IF(STATUS.NE.0) THEN
         IF (NP.EQ.0) CALL WARN(8)
         GOTO 999
      ENDIF
      !
      ! DONT LISTREL RULE RELATIONS
      !
      IF(EQ(NAME,KARDT)) GO TO 100
      IF(EQ(NAME,KARRC)) GO TO 100
      !
      !   VALIDATE USER
      !
      IF(EQ(USERID,OWNER)) GO TO 150
      IF(EQ(RPW,NONE)) GO TO 150
      IF(EQ(RPW,USERID)) GO TO 150
      IF(EQ(MPW,USERID)) GO TO 150
      GO TO 100
150   CONTINUE
      IF(NP.EQ.0) THEN
         ! WRITE OUT HEADER
         CALL MSG('R','  TABLE NAME      ' // &
            '   ROWS' // '  LAST MODIFIED',' ')
         CALL MSG('R','  ----------------' // &
            ' ------' // '  ------------ ',' ')
         NP = 1
      ENDIF
      CALL MSG('R','  ','+')
      CALL AMSG(NAME,ZC,'+')
      CALL IMSG(NTUPLE,7,'+')
      CALL MSG('R','  ','+')
      CALL DMSG(RDATE,0,' ',KZDATE)
      GO TO 100
      !
      !   LISTREL A SPECIFIC RELATION
      !
1000  IF(EQKEYW(2,'*')) THEN
         ALLREL = .TRUE.
         CALL ZMOVE(RNAME,BLANK)
      ELSE
         ALLREL = .FALSE.
         CALL LXSREC(2,RNAME,ZC)
      ENDIF
      NREL = 0
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
         !  REQUESTED RELATION DOES NOT EXIST
         IF (ALLREL) CALL MSG('E','THERE ARE NO TABLES.',' ')
         IF (.NOT.ALLREL) CALL WARN(1,RNAME,BLANK)
         GOTO 999
      ENDIF
      !
1100  IF(ALLREL) THEN
         CALL RELGET(STATUS)
         IF((NREL.EQ.0).AND.(STATUS.NE.0)) THEN
            CALL WARN(8)
            GOTO 999
         ENDIF
         IF(STATUS.NE.0) GO TO 999
      ENDIF
      !
      !   CHECK PERMISSION
      !
      IF(EQ(USERID,OWNER)) GO TO 1300
      IF(EQ(RPW,NONE)) GO TO 1300
      IF(EQ(RPW,USERID)) GO TO 1300
      IF(EQ(MPW,USERID)) GO TO 1300
      IF(ALLREL) GO TO 1100
      CALL WARN(8)
      GO TO 999
1300  CONTINUE
      !
      !  PRINT HEADER.
      !
      NREL = NREL + 1
      CRPW = 'NONE'
      CMPW = 'NONE'
      IF(NE(RPW,NONE)) CRPW = 'YES'
      IF(NE(MPW,NONE)) CMPW = 'YES'
      !
      CALL MSG('R','      DATABASE : ','+')
      CALL AMSG(DBNAME,ZC,'+')
      CALL MSG(' ','   READ PASSWORD : ' // CRPW,'+')
      CALL MSG(' ','    LAST MOD : ','+')
      CALL DMSG(RDATE,0,' ',KZDATE)
      CALL MSG('R','         TABLE : ','+')
      CALL AMSG(NAME,ZC,'+')
      CALL MSG(' ',' MODIFY PASSWORD : ' // CMPW,' ')
      CALL MSG('R',' ',' ')
      !
      CALL MSG('R','  NAME            ' // '  TYPE' // &
         '      LENGTH  ' // '   FORMAT    ' // ' KEY',' ')
      CALL MSG('R','  ----------------' // '  ----' // &
         ' -------------' // ' ----------- ' // ' ---',' ')
      !
      !  FIND AND PRINT ATTRIBUTE DESCRIPTIONS
      !
      I = LOCATT(BLANK,NAME)
      IF(I.NE.0) THEN
         CALL MSG('R','NO COLUMNS',' ')
         GOTO 1800
      ENDIF
1500  CALL ATTGET(STATUS)
      IF(STATUS.NE.0) GO TO 1600
      KEY = ' '
      IF(ATTKEY.NE.0) KEY = 'YES'
      !
      !  PRINT ATTRIBUTE INFO
      !
      CALL MSG('R','  ','+')
      CALL AMSG(ATTNAM,ZC,'+')
      CALL MSG(' ','  ' //  RMTYPT(ATTYPE),'+')

      NCHAR = ATTCHA
      NWORDS = ATTWDS
      IF(ATTYPE.EQ.KZDOUB) NWORDS = NWORDS / 2
      IF(ATTYPE.EQ.KZDVEC) NWORDS = NWORDS / 2
      IF(ATTYPE.EQ.KZDMAT) NWORDS = NWORDS / 2

      IF(ATTYPE.EQ.KZTEXT) THEN
         IF (NCHAR.EQ.0) THEN
            CALL MSG(' ','      VAR      ','+')
         ELSE
            CALL IMSG(NCHAR,5,'+')
            CALL MSG(' ',' CHARS    ','+')
         ENDIF
         GO TO 1590
      ENDIF
      !
      IF(ATTYPE.EQ.KZIMAT) GO TO 1520
      IF(ATTYPE.EQ.KZRMAT) GO TO 1520
      IF(ATTYPE.EQ.KZDMAT) GO TO 1520
      IF (NWORDS.EQ.0) THEN
         CALL MSG(' ','      VAR      ','+')
      ELSE
         CALL IMSG(NWORDS,5,'+')
         CALL MSG(' ',' WORDS    ','+')
      ENDIF
      GO TO 1590

1520  CONTINUE
      IF(NWORDS.NE.0) THEN
         NC = NWORDS / NCHAR
         CALL IMSG(NCHAR,5,'+')
         CALL MSG(' ',' BY ','+')
         CALL IMSG(NC,5,'+')
         CALL MSG(' ',' ','+')
         GO TO 1590
      ENDIF
      !
      IF(NCHAR.NE.0) THEN
         NC = NWORDS / NCHAR
         CALL IMSG(NCHAR,5,'+')
         CALL MSG(' ',' BY VAR   ','+')
         GO TO 1590
      ENDIF
      !
      CALL MSG(' ',' VAR BY VAR    ','+')
      !
      ! FORMAT
      !
1590  IF (ATTFOR.NE.0) THEN
         CALL FMTDEC(ATTFOR,ATTYPE,FMTSTR,12)
         CALL AMSG(FMTSTR,12,'+')
      ELSE
         CALL MSG(' ','            ','+')
      ENDIF
      !
      ! COMPLETE THE LINE
      CALL MSG(' ',' ' // KEY,' ')
      GOTO 1500

      !
      !  FIND AND PRINT LINKS
      !
1600  I = LOCLNK(BLANK)
      NLINK = 0
      CALL MSG(' ',' ',' ')
1610  CALL LNKGET(STATUS)
      IF(STATUS.NE.0) GO TO 1690
      IF (NE(NAME,R1NAME) .AND. NE(NAME,R2NAME)) GOTO 1610
      !
      NLINK = NLINK + 1
      CALL MSG('R','  LINK ','+')
      CALL AMSG(LNAME,-ZC,'+')
      CALL MSG(' ',' FROM ','+')
      CALL AMSG(R1NAME,-ZC,'+')
      CALL MSG(' ','(','+')
      CALL AMSG(A1NAME,-ZC,'+')
      CALL MSG(' ',') TO ','+')
      CALL AMSG(R2NAME,-ZC,'+')
      CALL MSG(' ','(','+')
      CALL AMSG(A2NAME,-ZC,'+')
      CALL MSG(' ',')',' ')
      GOTO 1610

1690  IF(NLINK.EQ.0) CALL MSG('R','  NO LINKS',' ')

1700  CALL MSG('R',' ',' ')
      CALL MSG('R','    CURRENT NUMBER OF ROWS = ','+')
      CALL IMSG(NTUPLE,5,' ')
      CALL MSG('R',' ',' ')
1800  IF(ALLREL) GO TO 1100
      GO TO 999
      !
      !  ALL DONE.
      !
999   RETURN  1
   END SUBROUTINE LSTREL



   SUBROUTINE RNAMER(*)
      !
      ! SUBROUTINE TO RENAME A RELATION
      !
      USE RM_Parameters
      USE RM_Globals, only : DFLAG
      USE Extern, only : AMSG, MSG
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
      Use Lexer, only: LXSREC
      USE Message, only: WARN
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INCLUDE 'rmatts.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tuplel.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'attble.inc'
      INCLUDE 'dclar1.inc'

      INTEGER :: I, ISTAT, STATUS

      LOGICAL NE,EQ
      INTEGER LOCREL, LOCPRM, LOCATT, LOCLNK
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
      IF(ITEMS.NE.5) GO TO 900
      IF(.NOT.EQKEYW(4,'TO')) GO TO 900
      IF(.NOT.TOKTYP(3,KXNAME)) THEN
         CALL WARN(7,ASCREC(IDP(3)))
         GOTO 999
      ENDIF
      IF(.NOT.TOKTYP(5,KXNAME)) THEN
         CALL WARN(7,ASCREC(IDP(5)))
         GOTO 999
      ENDIF
      CALL LXSREC(5,RNAME1,ZC)
      I = LOCREL(RNAME1)
      IF(I.EQ.0) THEN
         !  NEW NAME IS A DUPLICATE.
         CALL WARN(5,RNAME1)
         GO TO 999
      ENDIF
      CALL LXSREC(3,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
         CALL WARN(1,RNAME)
         GOTO 999
      ENDIF
      I = LOCPRM(NAME,2)
      IF(I.NE.0) THEN
         ! FAILS MODIFY PERMISSION
         CALL WARN(8)
         GO TO 999
      ENDIF
      !
      !  CHANGE EVERYTHING NEEDED FOR THE RELATION.
      !
      CALL RELGET(ISTAT)
      CALL LXSREC(5,RNAME1,ZC)
      CALL ZMOVE(NAME,RNAME1)
      CALL RELPUT
      I = LOCATT(BLANK,RNAME)
      IF(I.NE.0) GO TO 300
      !
200   CALL ATTGET(ISTAT)
      IF (ISTAT.EQ.0) THEN
         CALL ZMOVE(RELNAM,RNAME1)
         CALL ATTPUT(ISTAT)
         GO TO 200
      ENDIF
      !
      ! ALSO RENAME IN THE LINKS TABLES
      !
300   IF (LOCLNK(BLANK).NE.0) GOTO 800
400   CALL LNKGET(STATUS)
      IF (STATUS.NE.0) GOTO 800
      IF (EQ(R1NAME,RNAME )) CALL ZMOVE(R1NAME,RNAME1)
      IF (EQ(R2NAME,RNAME )) CALL ZMOVE(R2NAME,RNAME1)
      CALL LNKPUT(STATUS)
      GOTO 400
      !
      !
800   CALL MSG(' ','TABLE ''','+')
      CALL AMSG(RNAME,-ZC,'+')
      CALL MSG(' ',''' RENAMED TO ''','+')
      CALL AMSG(RNAME1,-ZC,'+')
      CALL MSG(' ',''',',' ')
      GOTO 999
      !
      ! SYNTAX ERROR
      !
900   CALL WARN(4)
      !
999   RETURN 1
   END SUBROUTINE RNAMER



   MODULE SUBROUTINE RMZIP(*)
      !!
      !!  PURPOSE:  PROCESS ZIP COMMAND  (CALL SYSTEM FUNCTION)
      !!
      USE RM_Globals, only : DFLAG, DBFNAM
      USE Extern, only: IMSG, MSG
      USE Files, only: RMCLOS
      USE Lexer, only: KXTEXT, TOKTYP, ASCREC, IDP, IDL, ITEMS
      USE Message, only: WARN
      USE RM_Text, only : STRASC
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

      USE RM_Globals, only: LIBFLG, TOL, HXFLAG, PCENT, RUCK, DBFNAM, KZHPSK, KZHPRL, KZHPKY, DFLAG, RMSTAT
      USE RM_BufferData, only: BUFFER
      USE Extern, only: IMSG, AMSG, MSG
      USE Files, only: RMCLOS
      USE Lexer, only: KWS, ITEMS
      USE Message, only: WARN
      !USE RIM, only: DBOPEN
      USE RM_Text, only: ASCTXT

      INCLUDE 'syspar.inc'

      INCLUDE 'rmatts.inc'
      INCLUDE 'tupler.inc'
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


   MODULE SUBROUTINE RIMCMD
      !!
      !! RIM COMMAND DISPATCHER
      !!
      USE RM_Parameters
      USE RM_Globals, only: HXFLAG, FFFLAG
      USE Extern, only : SETIN, MSG
      USE Files, only: RMCLOS
      USE Lexer, only: KXKEYW, TOKTYP, KWS, ITEMS, EQKEYW
      USE Parser, only: LODREC, MACDEF
      USE System, only : SYSCOM

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
      IF (EQKEYW(1,'BUILD'))   THEN
         CALL BUILD
         GOTO 100
      END IF
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
