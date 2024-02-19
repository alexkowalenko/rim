MODULE Parser
   implicit none
   private

   PUBLIC LODREC
   public MACDEF
   public LODELE
   public LODREL
   public LODLNK
   public LODPAS

contains

   SUBROUTINE MACEXP(M)
      !!
      !! --- TEXT PARSING ROUTINE
      !!
      !! EXPANDS A MACRO (M) DEFINITION AT ITEMS
      !!
      USE RM_Parameters
      USE Cards, only : LXEOC
      USE Lexer, only : ASCREC, IDP, IDL, ITEMS, NXTTOK
      USE Macros, only : MACNRG, MACWPT, MACWRK, MACPTR, MACTXT, MACLEN
      USE RM_Text, only : ABLANK, STRMOV

      INTEGER, intent(in) :: M

      INTEGER :: MI, NARGS, XARGS, EOR, I, CH, ITM

      MI = ITEMS
! ACCUMULATE ARGUMENTS
      NARGS = MACNRG(M)
      XARGS = 0
      IF (NARGS.GT.0) THEN
         DO I = 1, NARGS
            CALL NXTTOK(EOR)
            IF (EOR.NE.0) GOTO 110
            XARGS = I
         END DO
      ENDIF

! MOVE TO WORK AREA

110   CH = ABLANK
      IF (LXEOC.NE.0 .OR. XARGS.NE.NARGS) CH = 0
      MACWPT = MACWPT - 1
      CALL PUTT(MACWRK(1),MACWPT,CH)
      LXEOC = 0

      DO I = MACLEN(M),1,-1
         CALL GETT(MACTXT(MACPTR(M)),I,CH)
         IF (CH.GT.31) THEN
            MACWPT = MACWPT - 1
            CALL PUTT(MACWRK(1),MACWPT,CH)
         ELSEIF (CH.LE.XARGS) THEN
            ITM = MI + CH
            CALL STRMOV(ASCREC(IDP(ITM)),1,IDL(ITM), &
               MACWRK(1),MACWPT-IDL(ITM))
            MACWPT = MACWPT - IDL(ITM)
         ENDIF
      END DO
      ITEMS = MI - 1
      RETURN
   END SUBROUTINE MACEXP


   SUBROUTINE LODREC
      !!
      !!  SUBROUTINE TO READ A FREE-FIELD FORMAT INPUT RECORD.
      !!
      USE RM_Parameters
      USE Cards, only : READCD
      USE Extern, only : SETIN, AMSG, MSG
      USE RM_Globals, only : TRACE, RMSTAT, ECHO, CONNI, NINT, BATCH, INEOF
      USE Files, only: RMCLOS
      USE Lexer, only: KXTEXT, KXNAME, IDT, TOKTYP, ASCREC, ASCNXT, IDP, IDL, KWS, ITEMS, EQKEYW, LXSREC, NXTTOK, LXCMNT, TOKDSP
      USE Macros, only: LOCMAC, MACNUM
      USE System, only : SystemExit, SystemTrap
      USE RM_Text, only: EQSIGN, GTSIGN

      CHARACTER(len=2) :: PKW
      INTEGER :: MACTST(Z)
      INTEGER :: EOR, I, M

      !
      ! ACCUMULATE TOKENS UNTIL END-OF-RECORD
      !
      !
10    ITEMS = 0
      ASCNXT = 1
90    PKW = '  '
100   CALL NXTTOK(EOR)
      IF (EOR.EQ.0) THEN
         !
         !    CHECK FOR LIGATURES
         !
         !    '*(' IS START OF COMMENT
         !
         IF (KWS(ITEMS).EQ.'(' .AND. PKW.EQ.'*') THEN
            CALL LXCMNT(EOR)
            IF (EOR.NE.0) GOTO 200
            GOTO 90
         ENDIF
         !
         !       '>=', '<=' , AND '<>' ARE SINGLE TOKENS
         !
         IF (KWS(ITEMS).EQ.'=' .AND. &
            (PKW.EQ.'>'.OR.PKW.EQ.'<')) THEN
            ITEMS = ITEMS - 1
            KWS(ITEMS)(2:2) = '='
            CALL PUTT(ASCREC(IDP(ITEMS)),2,EQSIGN)
            IDL(ITEMS) = 2
         ENDIF
         IF (KWS(ITEMS).EQ.'>' .AND. &
            (PKW.EQ.'<')) THEN
            ITEMS = ITEMS - 1
            KWS(ITEMS)(2:2) = '>'
            CALL PUTT(ASCREC(IDP(ITEMS)),2,GTSIGN)
            IDL(ITEMS) = 2
         ENDIF
         PKW = KWS(ITEMS)
         !
         !    CHECK FOR MACROS
         !
         IF (MACNUM.EQ.0) GOTO 100
         IF (.NOT.TOKTYP(ITEMS,KXNAME)) GOTO 100
         IF (EQKEYW(1,'MACRO')) GOTO 100

         CALL LXSREC(ITEMS,MACTST,ZC)
         M = LOCMAC(MACTST)
         IF (M.NE.0) CALL MACEXP(M)
         GOTO 100
      ENDIF
      !
200   CONTINUE
      !
      ! ON EOF: REREAD IF READING THE TERMINAL
      !         SUPPLY 'END' IF READING THE ALTERNATE FILE
      !         ELSE IS ERROR
      !
900   IF (INEOF.NE.0) THEN
         IF (CONNI) THEN
            INEOF = 0
            GOTO 10
         ELSE IF (BATCH .AND. NINT.EQ.ZNINT) THEN
            CALL RMCLOS
            CALL MSG('E','EOF REACHED ON THE BATCH INPUT FILE',' ')
            CALL SystemTrap('CLEAR')
            CALL SystemExit
            CALL EXIT(1)
         ELSE
            ITEMS = 2
            KWS(1) = 'END'
            KWS(2) = '*EOF*'
            CALL SETIN(ZTRMIN)
         ENDIF
      ENDIF
      !
      ! REREAD ON NULL INPUT OR ERROR
      !
      IF (ITEMS.EQ.0 .OR. EOR.LT.0) THEN
         IF (READCD.LT.0) THEN
            RMSTAT = 4
            RETURN
         ENDIF
         READCD = 0
         GOTO 10
      ENDIF
      !
      ! POSSIBLY ECHO INPUT
      !
      IF (ECHO) THEN
         DO I = 1, ITEMS
            CALL MSG(' ',' ','+')
            IF (IDT(I).EQ.KXTEXT) CALL MSG(' ','''','+')
            CALL AMSG(ASCREC(IDP(I)),IDL(I),'+')
            IF (IDT(I).EQ.KXTEXT) CALL MSG(' ','''','+')
         END DO
         CALL MSG(' ',' ',' ')
      ENDIF
      IF (TRACE.GT.20) CALL TOKDSP

      !
      ! EXIT WITH GOOD TOKENS
      !
      RETURN
   END SUBROUTINE LODREC


   SUBROUTINE MACDEF(*)
      !!
      !! COMMAND ROUTINE TO DEFINE A MACRO
      !!
      ! SYNTAX:  MACRO <NAME> = <TEXT> ARG# <TEXT> ...
      !          MACRO <NAME> CLEAR
      !
      ! *  =  RETURN STATEMENT
      !
      !-----------------------------------------------------
      !
      USE RM_Parameters
      USE Extern, only: MSG
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW, IDI, LXSREC
      USE Macros, only: LOCMAC, MACNUM, MACLEN, MACPTR, MACTXT, MACNTX, MACNAM, MACNRG
      USE Message, only : WARN
      USE RM_Text, only : STRMOV, BLANK, ABLANK
      USE Utils, only : ZMOVE

      INTEGER :: M, NW, ST, I, WP, PTR, NARG
      INTEGER :: MAC(Z)

      IF (ITEMS.LT.3) GOTO 8000
      IF (.NOT.TOKTYP(2,KXNAME)) GOTO 8000
      IF (ITEMS.EQ.3 .AND. KWS(3).EQ.'=') GOTO 9000
      CALL LXSREC(2,MAC,ZC)
      !
      ! IF NEW MACRO THEN FIND SLOT IN MACNAM
      ! ELSE DELETE OLD DEFINITION AND REUSE
      !
      M = LOCMAC(MAC)
      IF (M.EQ.0) THEN
         M = LOCMAC(BLANK)
         IF (M.EQ.0) THEN
            IF (MACNUM.GE.ZMXMAC) THEN
               CALL MSG('E','TOO MANY MACRO DEFINITIONS',' ')
               GOTO 9000
            ENDIF
            MACNUM = MACNUM + 1
            M = MACNUM
         ENDIF
      ELSE
         NW = (MACLEN(M)-1)/ZCW + 1
         ST = MACPTR(M)
         CALL BLKMOV(MACTXT(ST),MACTXT(ST+NW),MACNTX-ST-NW)
         DO I = 1, MACNUM
            IF (MACPTR(I).GT.ST) MACPTR(I) = MACPTR(I) - NW
         END DO
         MACNTX = MACNTX - NW
         CALL ZMOVE(MACNAM(1,M),BLANK)
         MACPTR(M) = 0
      ENDIF

      IF (EQKEYW(3,'CLEAR')) GOTO 9000
      IF (KWS(3).NE.'=') GOTO 8000

      CALL ZMOVE(MACNAM(1,M),MAC)
      !
      ! COPY REPLACEMENT TEXT TO MACRO AREA
      ! EACH TOKEN WILL BE SURROUNDED BY A SPACE
      !
      WP = MACNTX
      PTR = 0
      NARG = 0
      DO I = 4, ITEMS
         ! PTR = PTR + 1
         IF (PTR.GT.ZMXMTX*ZCW-2) GOTO 8200
         ! CALL PUTT(MACTXT(WP),PTR,ABLANK)
         IF (IDI(I).GT.0 .AND. IDI(I).LT.32) THEN
            ! INTEGER IS PARAMETER NUMBER
            PTR = PTR + 1
            CALL PUTT(MACTXT(WP),PTR,IDI(I))
            IF (IDI(I).GT.NARG) NARG = IDI(I)
         ELSE
            ! IS REPLACEMENT TEXT
            IF (PTR+IDL(I).GT.ZMXMTX*ZCW-1) GOTO 8200
            CALL STRMOV(ASCREC(IDP(I)),1,IDL(I),MACTXT(WP),PTR+1)
            PTR = PTR + IDL(I)
         ENDIF
         ! PTR = PTR + 1
         ! CALL PUTT(MACTXT(WP),PTR,ABLANK)
      END DO
      MACNRG(M) = NARG
      MACPTR(M) = WP
      MACLEN(M) = PTR
      MACNTX = WP + (PTR-1)/ZCW + 1
      GOTO 9000

8000  CALL WARN(4)
      GOTO 9000
8200  CALL MSG('E','TOO MUCH MACRO TEXT',' ')

9000  RETURN 1
   END SUBROUTINE MACDEF


   SUBROUTINE LODELE(NUMELE)
      !!
      !! LOAD THE ELEMENT DATA INTO THE SCRATCH AREA
      !!
      !!  PARAMETERS:
      !!     NUMELE--NUMBER OF NEWLY DEFINED ATTRIBUTES
      !!
      USE RM_Parameters
      USE RM_BufferData, only: BUFFER
      USE Extern, only: MSG
      USE Formater, only : TYPER, LXFMT
      USE Lexer, only : KXINT, KXNAME, TOKTYP, ITEMS, EQKEYW, IDI
      USE Lexer, only : LFIND, LXSREC
      USE Message, only : WARN
      USE Utils, only : HTOI

      INTEGER, intent(in out) :: NUMELE

      INCLUDE 'rmatts.inc'
      !
      ! TEMPORARY ATTRIBUTE STORAGE
      INCLUDE 'tmpa.inc'
      !
      INCLUDE 'dclar1.inc'

      INTEGER :: COLUMN, FMT, FMTLEN, FMTPOS, KEY, KEYPOS, KQ1, LENGTH, LTEMS, NWORDS, ROWS, STYP, IATTV, VTYP
      INTEGER BLKLOC

      !
      !  READ AN ELEMENT RECORD.
      !
100   CALL LODREC
      IF(ITEMS.EQ.1) GOTO 900
      !
      !  EDIT ELEMENT INPUT.
      !
200   IATTV = 0
      IF(EQKEYW(2,'REAL')) IATTV = KZREAL
      IF(EQKEYW(2,'TEXT')) IATTV = KZTEXT
      IF(EQKEYW(2,'DATE')) IATTV = KZDATE
      IF(EQKEYW(2,'TIME')) IATTV = KZTIME
      IF(EQKEYW(2,'INTEGER')) IATTV = KZINT
      IF(EQKEYW(2,'DOUBLE')) IATTV = KZDOUB
      IF(EQKEYW(2,'RVEC')) IATTV = KZRVEC
      IF(EQKEYW(2,'IVEC')) IATTV = KZIVEC
      IF(EQKEYW(2,'DVEC')) IATTV = KZDVEC
      IF(EQKEYW(2,'RMAT')) IATTV = KZRMAT
      IF(EQKEYW(2,'IMAT')) IATTV = KZIMAT
      IF(EQKEYW(2,'DMAT')) IATTV = KZDMAT
      IF(IATTV.EQ.0) THEN
         CALL MSG('E','UNKNOWN DATA TYPE SPECIFIED.',' ')
         GO TO 800
      ENDIF
      !
      !  MAKE SURE THAT THE ATTRIBUTE NAME IS VALID
      !
      IF(.NOT.TOKTYP(1,KXNAME)) THEN
         CALL LXSREC(1,ANAME,ZC)
         CALL WARN(7,ANAME)
         GO TO 800
      ENDIF
      !
      ! SCAN LINE
      !
      LENGTH = 1
      IF(EQKEYW(2,'TEXT')) LENGTH = ZC
      ROWS = 1
      COLUMN = 1
      FMT = 0
      KEY = 0
      LTEMS = ITEMS
      KEYPOS = LFIND(3,ITEMS-2,'KEY')
      FMTPOS = LFIND(3,ITEMS-2,'FORMAT')
      IF (KEYPOS.NE.0) LTEMS = KEYPOS - 1
      IF (FMTPOS.NE.0) LTEMS = MIN(LTEMS,FMTPOS-1)
      !
      IF(LTEMS.EQ.2) GO TO 700
      IF(LTEMS.EQ.3) GO TO 500
      IF(LTEMS.EQ.4) GO TO 600
      CALL WARN(4)
      GO TO 800
      !
      !  ITEMS = 3.
      !
      !500   IF((IDI(3).GT.0).AND.(IDI(3).LE.9999)) THEN
500   IF(IDI(3).GT.0) THEN
         LENGTH = IDI(3)
         ROWS = LENGTH
         GO TO 700
      ENDIF
      IF(EQKEYW(3,'VAR')) THEN
         LENGTH = 0
         ROWS = 0
         COLUMN = 0
         GO TO 700
      ENDIF
      CALL MSG('E','LENGTH MUST BE A POSITIVE INTEGER.',' ')
      GOTO 800
      !
      !
      !  ITEMS = 4.
      !
600   IF((TOKTYP(3,KXINT)).AND.(IDI(3).GT.0)) THEN
         LENGTH = IDI(3)
         ROWS = LENGTH
         IF((TOKTYP(4,KXINT)).AND.(IDI(4).GT.0)) THEN
            COLUMN = IDI(4)
            GOTO 670
         ENDIF
         IF(EQKEYW(4,'VAR')) THEN
            COLUMN = 0
            GOTO 670
         ENDIF
      ENDIF
      IF(EQKEYW(3,'VAR')) THEN
         LENGTH = 0
         ROWS = 0
         IF(EQKEYW(4,'VAR')) THEN
            COLUMN = 0
            GOTO 670
         ENDIF
      ENDIF
      CALL MSG('E','LENGTH MUST BE A POSITIVE INTEGER.',' ')
      GO TO 800
      !
      !
670   IF(.NOT. (EQKEYW(2,'RMAT') .OR. &
         EQKEYW(2,'IMAT') .OR. &
         EQKEYW(2,'DMAT')))THEN
         CALL MSG('E', &
            'YOU ENTERED ROWS AND COLUMNS BUT TYPE NOT MATRIX.',' ')
         GO TO 800
      ENDIF
      !
      ! SET KEY AND FORMAT
      !
700   IF (KEYPOS.NE.0) KEY = 1
      IF (FMTPOS.NE.0) THEN
         CALL TYPER(IATTV,VTYP,STYP)
         CALL LXFMT(FMTPOS+1,STYP,FMT,FMTLEN)
         IF (FMT.EQ.0) THEN
            GOTO 800
         ENDIF
      ENDIF
      !
      !  STORE THE ELEMENT IN JUNK.
      !
      NUMELE = NUMELE + 1
      CALL BLKCHG(10,TMPAL,NUMELE)
      KQ1 = BLKLOC(10)
      KQ1 = KQ1 + (TMPAL*(NUMELE-1))
      CALL LXSREC(1,BUFFER(KQ1),ZC)
      BUFFER(KQ1+TMPA2-1) = IATTV
      IF(EQKEYW(2,'DOUBLE')) LENGTH = LENGTH * 2
      BUFFER(KQ1+TMPA3-1) = LENGTH
      BUFFER(KQ1+TMPA4-1) = KEY
      BUFFER(KQ1+TMPA5-1) = FMT
      !
      !  GET MORE DATA.
      !
      IF(IATTV.EQ.KZTEXT) THEN
         ! SPECIAL PACKING FOR TEXT ATTRIBUTES.
         NWORDS = ((LENGTH - 1) / ZCW) + 1
         IF(LENGTH.EQ.0) NWORDS = 0
         CALL HTOI(LENGTH,NWORDS,BUFFER(KQ1+TMPA3-1))
         GO TO 100
      ENDIF
      !
      IF(IATTV.EQ.KZDATE) GO TO 100
      IF(IATTV.EQ.KZINT ) GO TO 100
      IF(IATTV.EQ.KZREAL) GO TO 100
      IF(IATTV.EQ.KZDOUB) GO TO 100
      !
      !  PROCESS VECTOR AND MATRIX ITEMS.
      !
      IF(IATTV.EQ.KZDVEC) COLUMN = 2
      IF(IATTV.EQ.KZDMAT) COLUMN = COLUMN * 2
      CALL HTOI(ROWS,ROWS*COLUMN,BUFFER(KQ1+TMPA3-1))
      GO TO 100
      !
      !
800   CALL MSG(' ','STILL DEFINING COLUMNS',' ')
      GOTO 100
      !
      !  DONE.
      !
900   CONTINUE
      !
      !C    IF (DBGATT) THEN
      !    CALL MSG(' ','INTERNAL ATTRIBUTE TABLE',' ')
      !    DO 950 I = BLKLOC(10),BLKLOC(10)+(TMPAL*(NUMELE-1)),TMPAL
      !950     CALL BLKDSP('ATTRIBUTE',BUFFER(I),'ZIIII')
      !CC   ENDFI
      !
      RETURN
   END SUBROUTINE LODELE


   SUBROUTINE LODREL(NUMELE)
      !!
      !! LOAD THE RELATION DESCRIPTION
      !!
      USE RM_Parameters
      USE RM_BufferData, only: BUFFER
      USE Extern, only: MSG
      Use Lexer, only: KXNAME, TOKTYP, ITEMS, LXSREC
      USE Message, only : WARN

      INTEGER, intent(in) :: NUMELE

      INCLUDE 'rmatts.inc'
      !
      INCLUDE 'dclar1.inc'

      INTEGER :: ERROR, I, JUNK

      INTEGER LOCREL, BLKLOC
      !
      !  READ RELATION DATA.
      !
100   CALL LODREC
      IF(ITEMS.EQ.1) GO TO 999
      IF(ITEMS.LT.3) THEN
         ! SYNATX ERROR
         CALL WARN(4)
         GO TO 800
      ENDIF
      !
      !  CHECK FOR VALID RELATION NAME.
      !
      IF(.NOT.TOKTYP(1,KXNAME)) THEN
         CALL MSG('E','THE TABLE NAME IS NOT VALID.',' ')
         GO TO 800
      ENDIF
      CALL LXSREC(1,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.EQ.0) THEN
         CALL WARN(5,RNAME)
         GOTO 800
      ENDIF
      !
      !  CHECK ATTRIBUTE NAMES.
      !
      JUNK = 1
      IF(NUMELE.GT.0) JUNK = BLKLOC(10)
      CALL CHKATT(BUFFER(JUNK),NUMELE,ERROR)
      IF (ERROR.EQ.0) GO TO 100
      !
      !
800   CALL MSG(' ','STILL DEFINING TABLES',' ')
      GOTO 100
      !
      !  END RELATION PROCESSING.
      !
999   CONTINUE
      RETURN
   END SUBROUTINE LODREL


   SUBROUTINE LODLNK
      !
      ! LOADS LINK DEFINITIONS.
      !

      USE RM_Parameters
      USE Extern, only: MSG
      Use Lexer, only: KXNAME, TOKTYP, ITEMS, EQKEYW, LXSREC
      USE Message, only : WARN
      USE Utils, only : ZMOVE

      INCLUDE 'rmatts.inc'

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tuplel.inc'
      !
      INCLUDE 'dclar1.inc'

      INTEGER :: FLEN, FTYP, I, I1, I2
      INTEGER :: LKNAM(Z)

      INTEGER LOCLNK, LOCATT
      !
      ! READ LINK DEFINITIONS
      !
100   CONTINUE
      CALL LODREC
      IF(ITEMS.EQ.1) GO TO 999
      !
      ! ASSUME LINK DEFINITION - CHECK SYNTAX
      !
      IF (ITEMS.NE.9            .OR. &
         .NOT.EQKEYW(2,'FROM') .OR. &
         .NOT.EQKEYW(4,'IN')   .OR. &
         .NOT.EQKEYW(6,'TO')   .OR. &
         .NOT.EQKEYW(8,'IN')) THEN
         CALL WARN(4)
         GOTO 800
      ENDIF
      !
      ! CHECK FOR VALID LINK NAME.
      !
200   IF(.NOT.TOKTYP(1,KXNAME)) THEN
         CALL MSG('E','THE LINK NAME IS NOT VALID.',' ')
         GO TO 800
      ENDIF
      !
      CALL LXSREC(1,LKNAM,ZC)
      I = LOCLNK(LKNAM)
      IF(I.EQ.0) THEN
         CALL MSG('E','THAT LINK HAS ALREADY BEEN DEFINED.',' ')
         GO TO 800
      ENDIF
      !
      ! CHECK RELATION AND ATTRIBUTE NAMES
      !
      CALL LXSREC(3,A1NAME,ZC)
      CALL LXSREC(5,R1NAME,ZC)
      IF (LOCATT(A1NAME,R1NAME).NE.0) THEN
         CALL WARN(3,A1NAME,R1NAME)
         GOTO 800
      ENDIF
      CALL ATTGET(I1)
      FTYP = ATTYPE
      FLEN = ATTLEN

      CALL LXSREC(7,A2NAME,ZC)
      CALL LXSREC(9,R2NAME,ZC)
      IF (LOCATT(A2NAME,R2NAME).NE.0) THEN
         CALL WARN(3,A2NAME,R2NAME)
         GOTO 800
      ENDIF
      CALL ATTGET(I2)
111   IF (ATTYPE.NE.FTYP .OR. ATTLEN.NE.FLEN) THEN
         CALL MSG('E', &
            'COLUMNS DO NOT HAVE THE SAME TYPE AND LENGTH.',' ')
         GOTO 800
      ENDIF

      CALL ZMOVE(LNAME,LKNAM)
      CALL LNKADD
      GOTO 100
      !
      !
800   CALL MSG(' ','STILL DEFINING LINKS',' ')
      GOTO 100
      !
      !  END RELATION PROCESSING.
      !
999   CONTINUE
      RETURN
   END SUBROUTINE LODLNK


   SUBROUTINE LODPAS(ERROR)
      !!
      !! PROCESS PASSWORDS DEFINITIONS
      !!         PASSWORD KEYWORDS MAY BE ABBREVIATED
      !!
      !! SYNTAX: READ PASSWORD FOR <REL> IS <PASSWORD>
      !!         RPW FOR <REL> IS <PASSWORD>
      !!         WRITE PASSWORD FOR <REL> IS <PASSWORD>
      !!         WPW FOR <REL> IS <PASSWORD>
      !!
      !!         <REL> CAN BE * FOR ALL RELATIONS
      !!
      USE RM_Parameters
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, ITEMS, EQKEYW
      USE Lexer, only: LXSREC
      USE Message, only : WARN
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE

      INTEGER, intent(in out) :: ERROR

      INCLUDE 'tupler.inc'

      INCLUDE 'dclar1.inc'
      INCLUDE 'dclar3.inc'

      INTEGER :: I, ISTAT, PWTP, STOK

      INTEGER LOCREL
      !
      !  READ A PASSWORD.
      !
100   CONTINUE
      CALL LODREC
      IF (ITEMS.EQ.1) GOTO 999
      PWTP = 0
      IF (EQKEYW(1,'READ')) THEN
         IF (.NOT.EQKEYW(2,'PASSWORD')) GOTO 700
         PWTP = 1
         STOK = 3
         GOTO 200
      ENDIF
      IF (EQKEYW(1,'MODIFY')) THEN
         IF (.NOT.EQKEYW(2,'PASSWORD')) GOTO 700
         PWTP = 2
         STOK = 3
         GOTO 200
      ENDIF
      IF (EQKEYW(1,'RPW')) THEN
         PWTP = 1
         STOK = 2
         GOTO 200
      ENDIF
      IF (EQKEYW(1,'MPW')) THEN
         PWTP = 2
         STOK = 2
         GOTO 200
      ENDIF
      IF(PWTP.EQ.0)  GO TO 700
      !
200   IF(.NOT.EQKEYW(STOK,'FOR')) GOTO 700
      IF(.NOT.EQKEYW(STOK+1,'*')) CALL LXSREC(STOK+1,RNAME,ZC)
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
         CALL WARN(1,RNAME,BLANK)
         GO TO 100
      ENDIF
      !
      IF(.NOT.EQKEYW(STOK+2,'IS')) GO TO 700
      !
      !  STORE THE PASSWORD.
      !
500   CALL RELGET(ISTAT)
      IF(ISTAT.NE.0) GO TO 100
      IF(.NOT.TOKTYP(ITEMS,KXNAME)) THEN
         CALL WARN(7,ASCREC(IDP(ITEMS)))
         ERROR = ERROR + 1
         GO TO 100
      ENDIF
      CALL LXSREC(STOK+3,RPW1,ZC)
      IF(PWTP.EQ.1) CALL ZMOVE(RPW,RPW1)
      IF(PWTP.EQ.2) CALL ZMOVE(MPW,RPW1)
      CALL RELPUT
      !
      !  LOOK FOR MORE RELATIONS.
      !
      GO TO 500
      !
      ! SYNTAX ERROR
      !
700   CALL WARN(4)
      GOTO 100
      !
      !  END PASSWORD PROCESSING.
      !
999   CONTINUE
      RETURN
   END SUBROUTINE LODPAS

END MODULE Parser
