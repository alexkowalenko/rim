MODULE Parser
   implicit none
   private

   PUBLIC LODREC
   public MACDEF

contains

   SUBROUTINE MACEXP(M)
      !!
      !! --- TEXT PARSING ROUTINE
      !!
      !! EXPANDS A MACRO (M) DEFINITION AT ITEMS
      !!
      USE Parameters
      USE Cards, only : LXEOC
      USE Lexer, only : ASCREC, IDP, IDL, ITEMS, NXTTOK
      USE Macros, only : MACNRG, MACWPT, MACWRK, MACPTR, MACTXT, MACLEN
      USE Text, only : ABLANK, STRMOV

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
      USE Parameters
      USE Cards, only : READCD
      USE Globals, only : TRACE, RMSTAT
      USE Files, only: RMCLOS
      USE Lexer, only: KXTEXT, KXNAME, IDT, TOKTYP, ASCREC, ASCNXT, IDP, IDL, KWS, ITEMS, EQKEYW, LXSREC, NXTTOK, LXCMNT
      USE Macros, only: LOCMAC, MACNUM
      USE System, only : SystemExit, SystemTrap
      USE Text, only: EQSIGN, GTSIGN

      INCLUDE 'files.inc'
      !
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
      USE Parameters
      Use Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW, IDI, LXSREC
      USE Macros, only: LOCMAC, MACNUM, MACLEN, MACPTR, MACTXT, MACNTX, MACNAM, MACNRG
      USE Message, only : WARN
      USE Text, only : STRMOV, BLANK, ABLANK
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

END MODULE Parser
