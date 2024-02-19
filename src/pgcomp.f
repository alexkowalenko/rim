      SUBROUTINE PGCOMP

         USE RM_Globals, only: PGVARS, INLINE, PGFLAG, RMSTAT
         USE RM_BufferData, only: BUFFER
         USE DateTime, only: RMTIME, RMDATE
         USE Extern, only: IMSG, AMSG, MSG
         USE Lexer, only: KXINT, TOKTYP, KWS, ITEMS, EQKEYW, IDI, LXSREC
         USE Message, only : WARN
         USE Parser, only: LODREC
         USE RM_Text, only : ASCTXT
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     COMPILE A RIM PROGRAM
C
C     THE PROGRAM IS COMPILED AND STORED IN /BUFFER/ IN BLOCK6.
C     VARIABLES ARE ALSO STORED IN /BUFFER/ IN BLOCK5.
C
         INCLUDE 'selcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'srtcom.inc'
C
         INCLUDE 'pgmcom.inc'
         INCLUDE 'expcom.inc'

         INCLUDE 'dclar1.inc'
         LOGICAL PGSTOR, PGBSTO
         LOGICAL SELREL, SELWHR, SELSRT
         LOGICAL VARADD, PGCEXP, PGCPRT
         INTEGER BLKLOC
         INTEGER OP(ZPGLVL), RELSAV(Z,ZPGLVL)
         INTEGER VARNAM(Z)
C
C     PARSING DATA FOR SELECT COMMAND
C
         PARAMETER (QKEYL=3)
         CHARACTER*(ZKEYWL) QKEYS(QKEYL)
         INTEGER QPTRS(2,QKEYL)

         PGFLAG = .TRUE.

C
C     BUFFERS *** BLOCKS 4 - 7 MUST STAY FIXED
C
C     BLOCK 1 CONTAINS FILE 2 DATA         (LEN LENBF2)
C     BLOCK 2 CONTAINS FILE 2 DATA         (LEN LENBF2)
C     BLOCK 3 CONTAINS FILE 2 DATA         (LEN LENBF2)
C     BLOCK 4 CONTAINS GTSORT DATA         (LEN MAXCOL)
C     BLOCK 5 CONTAINS THE VARIABLE LIST   (PGVBLK)
C     BLOCK 6 CONTAINS THE PROGRAM         (PGPBLK)
C     BLOCK 7 WILL CONTAIN IF/WHILE DATA   (WHSAVE)
C
C
         CALL BLKCLN
         DO 2 I = 10, 20
    2    CALL BLKCLR(I)
         CALL BLKDEF(5,PGVARS,1)
         PGVBLK = BLKLOC(5)
         PGVPTR = PGVBLK
         PGVMAX = PGVPTR + PGVARS
         CALL BLKDEF(6,1000,1)
         PGPBLK = BLKLOC(6)
         PGPPTR = PGPBLK
         PGPMAX = PGPPTR + 1000
C
C     SETUP DATE, TIME, PAGE, AND LINE VARIABLES
C
         ATTYPE = KZDATE
         ATTWDS = 1
         ATTCHA = 0
         CALL ASCTXT(ATTNAM,ZC,'REPORT_DATE')
         IF (.NOT.VARADD()) GOTO 8000
         BUFFER(PGVPTR-1) =  RMDATE()
         ATTYPE = KZTIME
         CALL ASCTXT(ATTNAM,ZC,'REPORT_TIME')
         IF (.NOT.VARADD()) GOTO 8000
         BUFFER(PGVPTR-1) = RMTIME()
         ATTYPE = KZINT
         CALL ASCTXT(ATTNAM,ZC,'PAGE_NUMBER')
         IF (.NOT.VARADD()) GOTO 8000
         RPGPTR = PGVPTR - 1
         CALL ASCTXT(ATTNAM,ZC,'LINE_NUMBER')
         IF (.NOT.VARADD()) GOTO 8000
         RLNPTR = PGVPTR - 1

         PGLVL = 0
C
  100    CALL LODREC
C     PGLINE IS REPORT PROGRAM LINE NUMBER * 1000
         PGLINE = INLINE * 1000

         IF (EQKEYW(1,'REMARK'))   GOTO 100
         IF (EQKEYW(1,'SELECT'))   GOTO 1000
         IF (EQKEYW(1,'PRINT'))    GOTO 1200
         IF (EQKEYW(1,'IF'))       GOTO 1400
         IF (EQKEYW(1,'ELSE'))     GOTO 1460
         IF (EQKEYW(1,'WHILE'))    GOTO 1500
         IF (EQKEYW(1,'HEADERS'))  GOTO 1600
         IF (EQKEYW(1,'FOOTERS'))  GOTO 1650
         IF (EQKEYW(1,'NEWPAGE'))  GOTO 1680
         IF (EQKEYW(1,'SET'))      GOTO 1700
         IF (EQKEYW(1,'PROCEDURE'))GOTO 1800
         IF (EQKEYW(1,'END'))   THEN
            IF (PGLVL.EQ.0)         GOTO 5000
            IF (OP(PGLVL).EQ.XISEL) GOTO 1090
            IF (OP(PGLVL).EQ.XIIF ) GOTO 1490
            IF (OP(PGLVL).EQ.XIWH ) GOTO 1590
            IF (OP(PGLVL).EQ.XIPROC)GOTO 1890
            GOTO 5000
         ENDIF

C     MAY BE PROCEDURE INVOCATION
         IF (ITEMS.EQ.1) GOTO 1900

C     ELSE ASSUME THIS IS AN ASSIGNMENT STATEMENT
         GOTO 1300

C
C     SELECT STATEMENT
C
 1000    PGLVL = PGLVL + 1
         OP(PGLVL) = XISEL
         OPLOC(PGLVL) = PGPPTR
         IF (.NOT.PGSTOR(XISEL+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         CALL ZMOVE(RELSAV(1,PGLVL),NAME)
C
C  PARSE THE COMMAND
C
         QKEYS(1) = 'FROM'
         QKEYS(2) = 'WHERE'
         QKEYS(3) = 'SORT'
         NSOVAR = 0
         NS = 0
C
         SC = PARSE(QKEYS,QKEYL,QPTRS)
         IF (SC.NE.2) THEN
            CALL WARN(4)
            GOTO 9000
         ENDIF
         JW = QPTRS(1,2)
         JS = QPTRS(1,3)

         IF (QPTRS(1,1).NE.2)  GOTO 8100
         IF (.NOT.SELREL(2,2))     GOTO 9000
         IF (.NOT.PGSTOR(NAME,Z))  GOTO 8000

C     WHERE CLAUSE
         NBOO = 0
         LIMTU = ALL9S
         IF (JW.NE.0) THEN
            IF (.NOT.SELWHR(JW,QPTRS(2,2))) GOTO 9000
         ENDIF
         IF (.NOT.PGBSTO('WH ')) GOTO 8000
         IF (.NOT.PGSTOR(NS,1))  GOTO 8000
         IF (.NOT.PGSTOR(KSTRT,1))  GOTO 8000

C     SORT CLAUSE
         IF (JS.NE.0) THEN
            IF (.NOT.SELSRT(JS,QPTRS(2,3))) GOTO 9000
         ENDIF
         IF (.NOT.PGBSTO('SRT')) GOTO 8000

C     STORE FORWARD POINTER
 1050    BUFFER(OPLOC(PGLVL)+1) = PGPPTR
         OPLOC(PGLVL) = PGPPTR
         IF (.NOT.PGSTOR(XISELX+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0     ,1)) GOTO 8000
         GOTO 100

C
C     END SELECT
C
 1090    IF (ITEMS.GT.1) THEN
            IF (.NOT.EQKEYW(2,'SELECT')) THEN
               CALL MSG('E','EXPECTED ''END SELECT''',' ')
               GOTO 9000
            ENDIF
         ENDIF
         IF (.NOT.PGSTOR(XISELE+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(OPLOC(PGLVL),1)) GOTO 8000
         BUFFER(OPLOC(PGLVL)+1) = PGPPTR
         CALL ZMOVE(NAME,RELSAV(1,PGLVL))
         PGLVL = PGLVL - 1
         GOTO 100

C
C     PRINT
C
 1200    OPLOC0 = PGPPTR
         IF (.NOT.PGSTOR(XIPRI+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
C
         IF (.NOT.PGCPRT('PRINT ',MAXLIN)) GOTO 9000
C
         BUFFER(OPLOC0+2) = PGPPTR
         IF (.NOT.PGBSTO('SEL')) GOTO 8000
         BUFFER(OPLOC0+1) = PGPPTR
         GOTO 100

C
C     ASSIGNMENT STATEMENT
C
C     FORMAT: VAR <TYPE> = EXPRESSION
C
 1300    OPLOC0 = PGPPTR
         IF (.NOT.PGSTOR(XICMP+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
C
         ATTYPE = KZINT
         ATTCHA = 0
         ATTWDS = 1
         EXPST = 3
         IF (KWS(2).NE.'=') THEN
            IF (KWS(3).NE.'=') GOTO 8000
            IF (EQKEYW(2,'INT' )) THEN
               ATTYPE = KZINT
            ELSE IF (EQKEYW(2,'DATE')) THEN
               ATTYPE = KZDATE
            ELSE IF (EQKEYW(2,'TIME')) THEN
               ATTYPE = KZTIME
            ELSE IF (EQKEYW(2,'REAL')) THEN
               ATTYPE = KZREAL
            ELSE IF (EQKEYW(2,'DOUBLE')) THEN
               ATTYPE = KZDOUB
               ATTWDS = 2
            ELSE IF (IDI(2).GT.0) THEN
               ATTYPE = KZTEXT
               ATTCHA = IDI(2)
               ATTWDS = (ATTCHA - 1) / ZCW + 1
            ELSE
               CALL MSG('E','UNRECOGNISED REPORT STATEMENT',' ')
               GOTO 9000
            ENDIF
            EXPST = 4
         ENDIF

         CALL LXSREC(1,VARNAM,ZC)
         IF (LOCVAR(VARNAM).EQ.0) THEN
            VPTR = ATTCOL - Z - 2
         ELSE
            CALL ZMOVE(ATTNAM,VARNAM)
            VPTR = PGVPTR
            IF (.NOT.VARADD()) GOTO 9000
         ENDIF

         IF (.NOT.PGCEXP(EXPST,ITEMS-EXPST+1)) GOTO 9000
         BUFFER(OPLOC0+2) = PGPPTR
         IF (.NOT.PGSTOR(VPTR,1)) GOTO 8000
         IF (.NOT.PGBSTO('EXP')) GOTO 8000
         BUFFER(OPLOC0+1) = PGPPTR
         GOTO 100

C
C     IF STATEMENT
C
 1400    PGLVL = PGLVL + 1
         OP(PGLVL) = XIIF
         OPLOC(PGLVL) = PGPPTR
         IF (.NOT.PGSTOR(XIIF+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0   ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0   ,1)) GOTO 8000

C     WHERE CLAUSE
         NBOO = 0
         LIMTU = ALL9S
         IF (.NOT.SELWHR(1,ITEMS)) GOTO 9000
         IF (.NOT.PGBSTO('WH ')) GOTO 8000
         BUFFER(OPLOC(PGLVL)+2) = PGPPTR
         GOTO 100
C
C     ELSE
C
 1460    IF (.NOT.PGSTOR(XIIFX+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         BUFFER(OPLOC(PGLVL)+1) = PGPPTR
         GOTO 100
C
C     END IF
C
 1490    IF (ITEMS.GT.1) THEN
            IF (.NOT.EQKEYW(2,'IF')) THEN
               CALL MSG('E','EXPECTED ''END IF''',' ')
               GOTO 9000
            ENDIF
         ENDIF
         IF (BUFFER(OPLOC(PGLVL)+1).EQ.0) THEN
            BUFFER(OPLOC(PGLVL)+1) = PGPPTR
         ELSE
            BUFFER(BUFFER(OPLOC(PGLVL)+1)-1) = PGPPTR
         ENDIF
         PGLVL = PGLVL - 1
         GOTO 100

C
C     WHILE STATEMENT
C
 1500    PGLVL = PGLVL + 1
         OP(PGLVL) = XIWH
         OPLOC(PGLVL) = PGPPTR
         IF (.NOT.PGSTOR(XIWH+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0   ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0   ,1)) GOTO 8000

C     WHERE CLAUSE
         NBOO = 0
         LIMTU = ALL9S
         IF (.NOT.SELWHR(1,ITEMS)) GOTO 9000
         IF (.NOT.PGBSTO('WH ')) GOTO 8000
         BUFFER(OPLOC(PGLVL)+2) = PGPPTR
         GOTO 100
C
C     END WHILE
C
 1590    IF (ITEMS.GT.1) THEN
            IF (.NOT.EQKEYW(2,'WHILE')) THEN
               CALL MSG('E','EXPECTED ''END WHILE''',' ')
               GOTO 9000
            ENDIF
         ENDIF
         IF (.NOT.PGSTOR(XIWHE+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(OPLOC(PGLVL),1)) GOTO 8000
         BUFFER(OPLOC(PGLVL)+1) = PGPPTR
         PGLVL = PGLVL - 1
         GOTO 100

C
C     HEADERS STATEMENT
C
 1600    OPLOC0 = PGPPTR
         IF (.NOT.PGSTOR(XIHDR+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
C
         IF (.NOT.PGCPRT('HEADER',MAXLIN)) GOTO 9000
C
         BUFFER(OPLOC0+2) = PGPPTR
         IF (.NOT.PGSTOR(MAXLIN,1)) GOTO 8000
         IF (.NOT.PGBSTO('SEL')) GOTO 8000
         BUFFER(OPLOC0+1) = PGPPTR
         GOTO 100

C
C     FOOTERS STATEMENT
C
 1650    OPLOC0 = PGPPTR
         IF (.NOT.PGSTOR(XIFTR+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
C
         IF (.NOT.PGCPRT('FOOTER',MAXLIN)) GOTO 9000
C
         BUFFER(OPLOC0+2) = PGPPTR
         IF (.NOT.PGSTOR(MAXLIN,1)) GOTO 8000
         IF (.NOT.PGBSTO('SEL')) GOTO 8000
         BUFFER(OPLOC0+1) = PGPPTR
         GOTO 100

C
C     NEWPAGE STATEMENT
C
 1680    IF (.NOT.PGSTOR(XINEWP+PGLINE ,1)) GOTO 8000
         GOTO 100
C
C     SET STATEMENT
C
 1700    OPLOC0 = PGPPTR
         IF (.NOT.PGSTOR(XISET+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         SETOP = 0
C
C     RM_Parameters WITH KEYWORDS
         IF (EQKEYW(2,'CASE'))    THEN
            SETOP = 1
            IF (EQKEYW(3,'IGNORE')) THEN
               SETVAL = 0
            ELSE IF (EQKEYW(3,'RESPECT')) THEN
               SETVAL = 1
            ELSE
               GOTO 7000
            ENDIF
            IF (.NOT.PGSTOR(SETVAL,1)) GOTO 8000
            GOTO 1790
         ENDIF
C
C     RM_Parameters WITH ASCII-RM_Text ARGS
         IF (EQKEYW(2,'INPUT'))  SETOP = 2
         IF (EQKEYW(2,'OUTPUT')) SETOP = 3
         IF (SETOP.NE.0) THEN
            IF (ITEMS.NE.3) GOTO 7000
            CALL LXSREC(3,ANAME,ZC)
            IF (KWS(3).EQ.'TERMINAL') CALL ASCTXT(ANAME,ZC,KWS(3))
            IF (.NOT.PGSTOR(ANAME,Z)) GOTO 8000
            GOTO 1790
         ENDIF
C
C     RM_Parameters WITH INTEGER ARGS
         IF (EQKEYW(2,'REPORT')) THEN
            IF (ITEMS.LT.4) GOTO 7000
            IF (.NOT.TOKTYP(4,KXINT)) GOTO 7000
            IF (EQKEYW(3,'WIDTH')) SETOP = 4
            IF (EQKEYW(3,'HEIGHT')) SETOP = 5
            IF (SETOP.EQ.0) GOTO 7000
            IF (.NOT.PGSTOR(IDI(4),1)) GOTO 8000
            GOTO 1790
         ENDIF
         GOTO 7000
C
 1790    BUFFER(OPLOC0+2) = SETOP
         BUFFER(OPLOC0+1) = PGPPTR
         GOTO 100

C
C     PROCEDURE STATEMENT
C
 1800    PGLVL = PGLVL + 1
         OP(PGLVL) = XIPROC
         OPLOC(PGLVL) = PGPPTR
         IF (.NOT.PGSTOR(XIPROC+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0    ,1)) GOTO 8000
         CALL ZMOVE(RELSAV(1,PGLVL),NAME)

         IF (EQKEYW(3,'USING')) THEN
            IF (ITEMS.NE.4) GOTO 8100
            IF (.NOT. SELREL(3,2)) GOTO 9000
         ELSE
            IF (ITEMS.NE.2) GOTO 8100
         ENDIF

         ATTYPE = KZPROG
         ATTCHA = 0
         ATTWDS = 1
         CALL LXSREC(2,VARNAM,ZC)
         IF (LOCVAR(VARNAM).NE.0) THEN
            CALL ZMOVE(ATTNAM,VARNAM)
            ATTCOL = PGVPTR + 2 + Z
            IF (.NOT.VARADD()) GOTO 9000
         ENDIF
         BUFFER(ATTCOL) = PGPPTR
         GOTO 100
C
C     END PROCEDURE
C
 1890    IF (ITEMS.GT.1) THEN
            IF (.NOT.EQKEYW(2,'PROCEDURE')) THEN
               CALL MSG('E','EXPECTED ''END PROCEDURE''',' ')
               GOTO 9000
            ENDIF
         ENDIF
         IF (.NOT.PGSTOR(XIPROE+PGLINE ,1)) GOTO 8000
         IF (.NOT.PGSTOR(0     ,1)) GOTO 8000
         BUFFER(OPLOC(PGLVL)+1) = PGPPTR
         CALL ZMOVE(NAME,RELSAV(1,PGLVL))
         PGLVL = PGLVL - 1
         GOTO 100

C
C     PROCEDURE INVOCATION
C
 1900    IF (.NOT.PGSTOR(XIPROX+PGLINE ,1)) GOTO 8000
         CALL LXSREC(1,VARNAM,ZC)
         IF (LOCVAR(VARNAM).NE.0) THEN
            CALL MSG('E','PROCEDURE ''','+')
            CALL AMSG(VARNAM,-ZC,'+')
            CALL MSG('E',''' IS NOT DEFINED',' ')
            GOTO 9000
         ENDIF
         IF (.NOT.PGSTOR(BUFFER(ATTCOL),1)) GOTO 8000
         GOTO 100

C
C
C     END REPORT ETC
C
 5000    CONTINUE
         IF (.NOT.PGSTOR(XIEXIT+PGLINE ,1)) GOTO 8000
 5010    CALL MSG(' ','COMPILATION COMPLETE',' ')
         GOTO 9900
C
C
C     ERRORS AND ETC.
C
 7000    CALL MSG('E','THIS IS AN INVALID SET STATEMENT',' ')
         GOTO 9000

 8000    CALL MSG('E','YOUR PROGRAM IS TOO LARGE',' ')
         GOTO 9000

 8100    CALL WARN(4)
         GOTO 9000

C     ERROR EXIT
 9000    CALL MSG(' ','AT LINE ','+')
         CALL IMSG(INLINE,6,' ')
         RMSTAT = 201
         RETURN

C     GOOD EXIT
 9900    RMSTAT = 0
         RETURN
      END
