MODULE Parser
   implicit none
   private

   PUBLIC LODREC

contains

   SUBROUTINE MACEXP(M)
      !
      ! --- TEXT PARSING ROUTINE
      !
      ! EXPANDS A MACRO (M) DEFINITION AT ITEMS
      !
      !-----------------------------------------------------
      !
      USE Parameters
      USE Lexer, only : ASCREC, IDP, IDL, ITEMS, NXTTOK
      USE Text, only : ABLANK, STRMOV

      INTEGER, intent(in) :: M

      INTEGER :: MI, NARGS, XARGS, EOR, I, CH, ITM

      INCLUDE 'cards.inc'
      INCLUDE 'maccom.inc'

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
      !
      !  SUBROUTINE TO READ A FREE-FIELD FORMAT INPUT RECORD.
      !
      !
      USE Parameters
      USE Globals, only : TRACE
      USE Lexer, only: KXTEXT, KXNAME, IDT, TOKTYP, ASCREC, ASCNXT
      USE Lexer, only: IDP, IDL, KWS, ITEMS, EQKEYW, LXSREC
      USE Macros, only: LOCMAC
      USE Rim, only : RMSTAT
      USE System, only : SystemExit, SystemTrap
      USE Lexer, only : EQSIGN, GTSIGN, NXTTOK, LXCMNT

      INCLUDE 'files.inc'
      INCLUDE 'cards.inc'
      INCLUDE 'maccom.inc'
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

END MODULE Parser
