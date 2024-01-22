      FUNCTION PGCEXP(ST,NT)
      INCLUDE 'syspar.inc'
C
C     COMPILE AN EXPRESSION (FOR PROGRAM MODE)
C
C         ST------STARTING TOKEN
C         NT------NUMBER OF TOKENS
C
      LOGICAL PGCEXP
C
      INCLUDE 'ascpar.inc'
      INCLUDE 'tokens.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'rimcom.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc'
      INCLUDE 'pgmcom.inc'
      INCLUDE 'expcom.inc'
      INCLUDE 'lxlcom.inc'
C
      CHARACTER*(ZKEYWL) KOMPAR
      LOGICAL EQKEYW, EQTOK, PGSTOR
      INCLUDE 'dclar1.inc'
      LOGICAL LXDATE
 
C     OPLIST CONTAINS OPCODE + LEVEL*100 (NOTE MAX OPCODE = 99)
C
      INTEGER OPLIST(2*ZMEOP)
C
C     EXTRA EXPRESSION OPERATORS
C
      PARAMETER (NGSIGN=94)
      PARAMETER (VMOP=01)
 
C
 
      PGCEXP = .TRUE.
      TYP = ATTYPE
      IS = ST
      ET = ST + NT - 1
      IF(NT.LT.1) GO TO 7000
C
C     COMPILE THE EXPRESSION
C
      DO 10 I=1, ZMEOP
      EXPTP(I) = -1
      EXPVAL(I) = 0
      EXPVRL(I) = 0
10    EXPVTP(I) = 0
      EXPTXN = 1
 
      RMSTAT = 0
      NEOP = 0
      EOPP = 0
      OPLP = 1
      OPLIST(1) = 0
      PLVL = 1
C
C     NEXT ITEM
C
100   IF(IS.GT.ET) GO TO 500
      IF (EQTOK(IS,ASLPAR)) THEN
         PLVL = PLVL + 1
         IS = IS + 1
         GOTO 100
      ENDIF
 
      IF (EQTOK(IS,MNSIGN)) THEN
         IF (TYP.EQ.KZTEXT) GOTO 7200
         OPLP = OPLP + 1
         IF(OPLP.GT.2*ZMEOP) GOTO 7700
         OPLIST(OPLP) = NGSIGN + PLVL*100
         IS = IS + 1
         GOTO 100
      ENDIF
C
C     ATTRIBUTE, VARIABLE, OR VALUE
C
      EOPP = EOPP + 1
      IF(EOPP.GT.ZMEOP) GOTO 7700
      NEOP = NEOP + 1
      EOP(NEOP) = EOPP
      NARGS = 0
 
      IF (.NOT.TOKTYP(IS,KXNAME)) GOTO 300
C
C     MAY BE ATTRIBUTE
C
      CALL LXSREC(IS,ANAME,ZC)
      I = LOCATT(ANAME,NAME)
      IF(I.NE.0) GOTO 250
      CALL ATTGET(I)
      EXPVAL(EOPP) = ATTCOL
      EXPLEN(EOPP) = ATTLEN
      EXPVTP(EOPP) = 0
      CALL TYPER(ATTYPE,SVM,TYP)
      IF (TYP.EQ.KZDATE .OR. TYP.EQ.KZTIME) TYP = KZINT
      EXPTP(EOPP) = TYP
C
C     CHECK FOR VEC OR MAT
C
      IF (SVM.EQ.KZVEC) NARGS = 1
      IF (SVM.EQ.KZMAT) NARGS = 2
      IF (NARGS.EQ.0) GOTO 400
 
      OOOP = NARGS
      IF (TYP.EQ.KZDOUB) OOOP = OOOP + 2
      IF (.NOT.EQTOK(IS+1,ASLPAR) .OR.
     1   .NOT.EQTOK(IS+2+NARGS,ASRPAR)) THEN
         CALL MSG('E','INCORRECTLY SPECIFIED ARRAY ITEM',' ')
         GOTO 8000
         ENDIF
 
      EXPVTP(EOPP) = 1
 
C     ALSO STACK LENGTH INFO
      EOPP = EOPP + 1
      IF(EOPP.GT.ZMEOP) GOTO 7700
      NEOP = NEOP + 1
      EOP(NEOP) = EOPP
      EXPVAL(EOPP) = ATTLEN
      EXPLEN(EOPP) = 1
      EXPVTP(EOPP) = 1
 
      OPLP = OPLP + 1
      OPLIST(OPLP) = OOOP + PLVL*100
      NARGS = NARGS - 1
      IS = IS + 1
      GOTO 100
C
C     MAY BE VARIABLE
C
250   I = LOCVAR(ANAME)
      IF(I.NE.0) GOTO 300
      EXPVAL(EOPP) = 0 - ATTCOL
      EXPVTP(EOPP) = 0
      EXPTP(EOPP) = ATTYPE
      GOTO 400
C
C     VALUE (INT, REAL, TEXT)
C
300   EXPVTP(EOPP) = 1
      IF (TYP.EQ.KZDATE) THEN
         IF (.NOT.LXDATE(IS,EXPVAL(EOPP),KRMDTF,TYP)) THEN
            CALL MSG('E','INVALID DATE VALUE',' ')
            GOTO 8000
         ENDIF
         EXPTP(EOPP) = KZINT
         GOTO 400
      ENDIF
      IF (TYP.EQ.KZTIME) THEN
         IF (.NOT.LXDATE(IS,EXPVAL(EOPP),KRMTMF,TYP)) THEN
            CALL MSG('E','INVALID TIME VALUE',' ')
            GOTO 8000
         ENDIF
         EXPTP(EOPP) = KZINT
         GOTO 400
      ENDIF
      IF (TOKTYP(IS,KXINT)) THEN
         EXPVAL(EOPP) = IDI(IS)
         EXPTP(EOPP) = KZINT
      ELSE IF (TOKTYP(IS,KXREAL)) THEN
         EXPVRL(EOPP) = IDR(IS)
         EXPTP(EOPP) = KZDOUB
      ELSE
         EXPVAL(EOPP) = PGPPTR
         EXPLEN(EOPP) = IDL(IS)
         W = LXLENW(IS)
         IF (.NOT.PGSTOR(ASCREC(IDP(IS)),W)) GOTO 8000
         EXPTP(EOPP) = KZTEXT
      ENDIF
C
C     END OF VALUE SCAN
C
400   IF (NARGS.GT.0) THEN
         NARGS = NARGS - 1
         IS = IS + 1
         GOTO 100
      ENDIF
 
      IF (OPLP.GT.0) THEN
         IF (OPLIST(OPLP)/100.GE.PLVL) THEN
            NEOP = NEOP + 1
            EOP(NEOP) = MOD(OPLIST(OPLP),100) * 10000
            OPLP = OPLP - 1
            GOTO 400
         ENDIF
      ENDIF
 
      IS = IS + 1
      IF(IS.GT.ET) GO TO 500
      IF (EQTOK(IS,ASRPAR)) THEN
         PLVL = PLVL - 1
         IF (PLVL.LT.1) GOTO 7100
         GOTO 400
      ENDIF
 
      CALL GETT(ASCREC(IDP(IS)),1,OP)
      IF(OP.NE.PLSIGN .AND. OP.NE.MNSIGN .AND.
     1   OP.NE.TMSIGN .AND. OP.NE.DVSIGN ) THEN
        CALL MSG('E','UNRECOGNISED OPERATOR',' ')
        GOTO 8000
      ENDIF
      IF(OP.NE.PLSIGN .AND. TYP.EQ.KZTEXT) GOTO 7200
      OPLP = OPLP + 1
      OPLIST(OPLP) = OP + PLVL*100
      IS = IS + 1
      GO TO 100
C
C     CHECK FOR CORRECT PARENTHESES
C
500   CONTINUE
      IF (PLVL.NE.1) GOTO 7100
      GO TO 9999
C
C     ERRORS
C
7000  CALL MSG('E','THE EXPRESSION IS NULL',' ')
      GO TO 8000
C
7100  CALL MSG('E','THE EXPRESSION''S PARENTHESES ARE ' //
     1         'NOT BALANCED.',' ')
      GOTO 8000
C
7200  CALL MSG('E','CONCATENATION (+) IS THE ONLY VALID ' //
     1         'TEXT OPERATOR',' ')
      GOTO 8000
 
 
7700  CALL MSG('E','THE EXPRESSION IS TOO COMPLICATED.',' ')
      GOTO 8000
C
C
C  UNABLE TO PROCESS THE EXPRESSION
C
 8000 CALL MSG(' ','PROCESSING FIELD ','+')
      CALL IMSG(IS,5,' ')
 
      RMSTAT = 4
      PGCEXP = .FALSE.
C
C     DONE
C
 9999 CONTINUE
      RETURN
      END
