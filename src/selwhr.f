      FUNCTION SELWHR(ST,NT)

         USE RM_Globals, only : CASEIG, TRACE, RMSTAT
         USE RM_Attributes, only: ATTGET, LOCATT
         USE Extern, only: IMSG, MSG
         USE Formater, only : TYPER
         USE Lexer, only: KXTEXT, KXNAME, TOKTYP, IDL, EQTOK, KWS
         USE Lexer, only: EQKEYW, LXLENW, LXSREC
         USE Message, only: WARN
         USE Utils, only : HTOI, ITOH
         USE RM_Variables, only: LOCVAR

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  PROCESS A RIM WHERE CLAUSE
C
C  RM_Parameters:
C         ST------STARTING TOKEN ('WHERE')
C         NT------NUMBER OF TOKENS
C
         LOGICAL SELWHR
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'pgmcom.inc'
         INCLUDE 'ptrcom.inc'
C
         CHARACTER*(ZKEYWL) KOMPAR
         CHARACTER*3 KMM
         INCLUDE 'dclar1.inc'
         LOGICAL VLIST
         INTEGER OPLIST(2*ZMWHR)
         INTEGER TSTKYS(ZMWHR)
C
         SELWHR = .TRUE.
         NS = 0
         NTUPC = 0
         KMM = ' '
         KSTRT = 0
         MAXTU = 0
         IS = ST
         ET = ST + NT - 1
         IF(NT.LT.2) GO TO 7000
C
C     COMPILE THE CLAUSE
C
         DO 600 I=1, ZMWHR
            KOMPOS(I) = 0
            KOMPOT(I) = 0
            KOMLEN(I) = 0
            KATTP(I) = 0
            KATTL(I) = 0
            KATTY(I) = -1
  600    CONTINUE

         RMSTAT = 0
         NBOO = 0
         BOOP = 0
         OPLP = 1
         OPLIST(1) = 0
         PLVL = 1
         NEXPOT = 1
         NEXPOS = 1
C
C     NEXT ITEM
C
 1000    CONTINUE
         IS = IS + 1
         IF(IS.GT.ET) GO TO 2000
         IF (KWS(IS).EQ.'(') THEN
            PLVL = PLVL + 1
            GOTO 1000
         ENDIF

         IF (EQKEYW(IS,'NOT')) THEN
            OPLP = OPLP + 1
            IF(OPLP.GT.2*ZMWHR) GOTO 7700
            OPLIST(OPLP) = WHNOT + PLVL*10
            GOTO 1000
         ENDIF

         IF(BOOP.GT.ZMWHR) GOTO 7700
C
         BOOP = BOOP + 1
         NBOO = NBOO + 1
         BOO(NBOO) = BOOP
         TSTKYS(BOOP) = 0

         CALL LXSREC(IS,ANAME,ZC)
         I = LOCATT(ANAME,NAME)
         IF (I.EQ.0) CALL ATTGET(I)
         IF (I.EQ.0) THEN
            KATTP(BOOP) = ATTCOL
         ELSE
C        POSSIBLY VARIABLE
            IF (LOCVAR(ANAME).EQ.0) THEN
               KATTP(BOOP) = 0 - ATTCOL
            ELSE
C           UNRECOGNIZED ATTRIBUTE.
               CALL WARN(3,ANAME,NAME)
               GO TO 8000
            ENDIF
         ENDIF
C
         KATTL(BOOP) = ATTLEN
         CALL TYPER(ATTYPE,MATVEC,KATTY(BOOP))
C
C  DETERMINE THE TYPE OF BOOLEAN EXPRESSION.
C
         KOMPAR = KWS(IS+1)
         KOMTYP(BOOP) = LOCBOO(KOMPAR)
         IF(KOMTYP(BOOP).EQ.0) THEN
C        UNRECOGNIZED BOOLEAN COMPARISION.
            CALL MSG('E',
     +        '''' // KOMPAR // ''' IS NOT A VALID COMPARISON.',' ')
            GO TO 8000
         ENDIF
C
C     CHECK FOR FAILS OR EXISTS
C
 1500    IF(KOMTYP(BOOP).LE.1) THEN
            KOMLEN(BOOP) = 1
            IS = IS + 2
            GO TO 1800
         ENDIF
C
C     VALUE OR ATTRIBUTE COMPARISON.
C
 1550    NLIST = 0
         IS = IS + 2
         IF (KWS(IS).EQ.'(') GOTO 1554
         VLIST = .FALSE.
         IF (.NOT.TOKTYP(IS,KXNAME)) GOTO 1555
C
C     MAY BE ATTRIBUTE OR VARIABLE COMPARISON.
C
         ISAVE = ATTYPE
         CALL LXSREC(IS,ANAME,ZC)
         I = LOCATT(ANAME,NAME)
         IF (I.EQ.0) THEN
            IF (KOMTYP(BOOP).EQ.9) GOTO 7510
            CALL ATTGET(I)
            KOMPOS(BOOP) = ATTCOL
         ELSE
            IF (LOCVAR(ANAME).NE.0) GOTO 1555
            KOMPOS(BOOP) = 0 - ATTCOL
         ENDIF
         KOMTYP(BOOP) = KOMTYP(BOOP) + 10
         IF(ATTLEN.NE.KATTL(BOOP)) GO TO 7500
         IF(ATTYPE.NE.ISAVE) GO TO 7500
         IS = IS + 1
         GOTO 1800
C
C     LIST OF VALUES
C     VALID ONLY FOR EQ, AND NE
C
 1554    IF((KOMTYP(BOOP).NE.2).AND.(KOMTYP(BOOP).NE.3)) GO TO 7600
         VLIST = .TRUE.


C     VALUE COMPARISON. MAKE SURE THE VALUE LOOKS GOOD.
C
 1555    CALL ITOH(NR,NW,KATTL(BOOP))
         IF(KATTY(BOOP).EQ.0) NW = 1
         ITYPE = ATTYPE
         IF(KATTY(BOOP).EQ.0) ITYPE = KZINT
         KOMPOS(BOOP) = NEXPOS
         KOMPOT(BOOP) = NEXPOT
         IF(KOMTYP(BOOP).EQ.9) THEN
C
C     'LIKE' STRING MATCH
C
            IF (ATTYPE.NE.KZTEXT .OR. .NOT.TOKTYP(IS,KXTEXT)) THEN
               CALL MSG('E',
     +         '''LIKE'' IS FOR TEXT ONLY.',' ')
               GO TO 8000
            ENDIF
            NW = LXLENW(IS)
            NR = IDL(IS)
            IF (NEXPOS+NW.GT.ZMWHVL) GOTO 7800
            CALL LXSREC(IS,WHRVAL(NEXPOS),NR)
            NEXPOS = NEXPOS + NW
            IS = IS + 1
            IF (NEXPOT.GT.ZMWHLL) GOTO 7800
            CALL HTOI(NR,NW,WHRLEN(NEXPOT))
            NEXPOT = NEXPOT + 1
            NLIST = NLIST + 1
            KOMLEN(BOOP) = NLIST
            GOTO 1800
         ENDIF
C
C     ALL OTHER CASES USE PARVAL TO EXTRACT NEXT VALUE
C
 1560    NWORDS = NW
         NROW = NR
         IF (NEXPOS+NWORDS.GT.ZMWHVL) GOTO 7800
         CALL PARVAL(IS,WHRVAL(NEXPOS),ITYPE,NWORDS,NROW,0,IERR)
         IF(IERR.NE.0) GO TO 8000
         NLIST = NLIST + 1
         NEXPOS = NEXPOS + NWORDS
         IF (NEXPOT.GT.ZMWHLL) GOTO 7800
         CALL HTOI(NROW,NWORDS,WHRLEN(NEXPOT))
         NEXPOT = NEXPOT + 1
         KOMLEN(BOOP) = NLIST
         IF (.NOT.VLIST .AND. KOMTYP(BOOP).EQ.2) THEN
            TSTKYS(BOOP) = ATTKEY

C        CANT DO KEY LOOKUP ON CASE IGNORE
            IF (KATTY(BOOP).EQ.KZTEXT .AND. CASEIG) TSTKYS(BOOP) = 0

            IF (TRACE.GE.4) THEN
               CALL MSG('T','SELWHR (TST KEY) ','+')
               CALL IMSG(KOMPOS(BOOP),4,'+')
               CALL IMSG(BOOP,6,'+')
               CALL IMSG(TSTKYS(BOOP),6,' ')
            ENDIF
         ENDIF
C
C
C     END OF VALUE SCAN
C
 1800    IF (OPLP.GT.0) THEN
            IF (OPLIST(OPLP)/10.GE.PLVL) THEN
               NBOO = NBOO + 1
               BOO(NBOO) = MOD(OPLIST(OPLP),10) * 10000
               OPLP = OPLP - 1
               GOTO 1800
            ENDIF
         ENDIF
         IF(IS.GT.ET) GO TO 2000
         IF (KWS(IS).EQ.')') THEN
            PLVL = PLVL - 1
            IF (PLVL.LT.1) GOTO 7100
            IS = IS + 1
            GOTO 1800
         ENDIF

         IF((KWS(IS).NE.'AND').AND.(KWS(IS).NE.'OR')) THEN
            CALL MSG('E','WHERE CLAUSE ITEMS MUST BE JOINED BY ' //
     1                   '''AND'' OR ''OR''.',' ')
            GOTO 8000
         ENDIF
         OPLP = OPLP + 1
         IF(KWS(IS).EQ.'AND') OPLIST(OPLP) = WHAND + PLVL*10
         IF(KWS(IS).EQ.'OR ') OPLIST(OPLP) = WHOR  + PLVL*10
         GO TO 1000
C
C     CHECK FOR CORRECT PARENTHESES
C
 2000    CONTINUE
         IF (PLVL.NE.1) GOTO 7100
C
C     CHECK FOR KEY PROCESSING POSSIBILITY
C
         OPLP = 0
         DO 2100 I = 1, NBOO
            CP = MOD(BOO(I),10000)
            OP = BOO(I)/10000
            IF (CP.NE.0) THEN
               OPLP = OPLP + 1
               OPLIST(OPLP) = CP
            ELSE IF (OP.EQ.WHNOT) THEN
               OPLIST(OPLP) = 0
            ELSE IF (OP.EQ.WHAND) THEN
               IF (OPLIST(OPLP).NE.0) THEN
                  IF (TSTKYS(OPLIST(OPLP)).NE.0) GOTO 2110
               ENDIF
               OPLP = OPLP - 1
               IF (OPLIST(OPLP).NE.0) THEN
                  IF (TSTKYS(OPLIST(OPLP)).NE.0) GOTO 2110
               ENDIF
            ELSE
               OPLP = OPLP - 1
               OPLIST(OPLP) = 0
            ENDIF
 2100    CONTINUE
 2110    KSTRT = 0
         NS = 0
         IF (OPLP.LE.0) GOTO 7000
         KBOOP = OPLIST(OPLP)
         IF (KBOOP.GT.0) THEN
            IF (TSTKYS(KBOOP).GT.0) THEN
               KSTRT = TSTKYS(KBOOP)
               NS = 2
               IF (TRACE.GE.2) THEN
                  CALL MSG('T','SELWHR (USE KEY) ','+')
                  CALL IMSG(KBOOP,6,'+')
                  CALL IMSG(KSTRT,9,' ')
               ENDIF
            ENDIF
         ENDIF
         GO TO 9999
C
C     ERRORS
C
 7000    CALL MSG('E','I CANNOT UNDERSTAND THE WHERE CLAUSE.',' ')
         CALL MSG('E','CHECK THE SYNTAX.',' ')
         GO TO 8000
C
 7100    CALL MSG('E','THE WHERE CLAUSE PARENTHESES ARE ' //
     1            'NOT BALANCED.',' ')
         GOTO 8000


 7500    CALL MSG('E','COMPARED COLUMNS' //
     +   ' MUST BE THE SAME TYPE AND LENGTH.',' ')
         GO TO 8000
C
 7510    CALL MSG('E','YOU CANNOT COMPARE TWO COLUMNS ' //
     +    'WITH ''LIKE''.',' ')
         GO TO 8000
C
 7600    CALL MSG('E','LISTS ARE ONLY VALID FOR EQ AND NE.' ,' ')
         GO TO 8000
C
 7700    CALL MSG('E','THE WHERE CLAUSE IS TOO COMPLICATED.',' ')
         GOTO 8000
C
 7800    CALL MSG('E','THE WHERE CLAUSE IS TOO LONG.',' ')
C
C
C  UNABLE TO PROCESS THE WHERE CLAUSE.
C
 8000    CONTINUE
         RMSTAT = 4
         SELWHR = .FALSE.
C
C  QUIT.
C
 9999    CONTINUE
         IF(MAXTU.EQ.0) MAXTU = ALL9S
         CALL WHETOL
         RETURN
      END
