      LOGICAL FUNCTION PGCPRT(BLKTYP,MAXLIN)

         USE Lexer, only: KXNAME, TOKTYP, ASCREC, IDP, IDL, KWS, ITEMS
         USE Lexer, only: EQKEYW, IDI, LXLENW, LXSREC

         INCLUDE 'syspar.inc'
C
C     COMPILE A PRINT CLAUSE
C
C     BLKTYP = TYPE OF BLOCK (PRINT, HEADER, FOOTER)
C     MAXLIN = NUMBER OF LINES IN THE BLOCK
C
         CHARACTER*(6) BLKTYP

         INCLUDE 'selcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'files.inc'
         INCLUDE 'dclar1.inc'
C
         INCLUDE 'pgmcom.inc'

         LOGICAL PGSTOR
C
C
         NUMATT = 0
         MAXLIN = 0
C
C     LOAD PRINT STATEMENTS
C
  100    CALL LODREC
         IF (KWS(1).EQ.'END') GOTO 500
         NUMATT = NUMATT + 1
         IF (NUMATT.GT.ZMSEL) THEN
            CALL MSG('E','TOO MANY PRINT ITEMS',' ')
            GOTO 9000
         ENDIF

C     FORMAT: LINE COL ATT/VAR FORMAT
         LIN1(NUMATT) = IDI(1)
         COL1(NUMATT) = IDI(2)
         IF (IDI(1).LE.0 .OR. IDI(2).LE.0) THEN
            CALL MSG('E','LINE AND COLUMN MUST BE POSITIVE INTEGERS',
     +               ' ')
            GOTO 9000
         ENDIF
         IF (IDI(1).GT.MAXLIN) MAXLIN = IDI(1)
         IF (ITEMS.EQ.4) GOTO 200
         IF (ITEMS.EQ.3) GOTO 300
         CALL MSG('E','INVALID SYNTAX FOR A PRINT STATEMENT',' ')
         GOTO 9000

  200    IF (.NOT.TOKTYP(3,KXNAME)) THEN
            CALL MSG('E','ITEM 3 MUST BE A NAME',' ')
            GOTO 9000
         ENDIF
         CALL LXSREC(3,ANAME,ZC)
C
C     CHECK FOR ATTRIBUTE
         IF (LOCATT(ANAME,NAME).NE.0) GOTO 220
         CALL ATTGET(STATUS)
         FP(NUMATT) = ATTCOL
         GOTO 240
C
C     CHECK FOR VARIABLE
  220    IF (LOCVAR(ANAME).NE.0) THEN
            CALL MSG('E','VARIABLE ''','+')
            CALL AMSG(ANAME,-ZC,'+')
            CALL MSG(' ',''' IS NOT DEFINED',' ')
            GOTO 9000
         ENDIF
         FP(NUMATT) = 0 - ATTCOL

  240    CALL LXFMT(4,ATTYPE,FMT,LFMT)
         IF (FMT.EQ.0) GOTO 9000
         IF (ATTYPE.EQ.KZTEXT) FMT = FMT*10000 + FMT
         PGRAPH = FMT/10000

         CALL TYPER(ATTYPE,SVM,TYP)
         ICOL = ATTCHA
         NWORDS = ATTWDS
         IF(TYP.EQ.KZDOUB) NWORDS = NWORDS/2
         FORMT(NUMATT) = FMT
         NUMCOL(NUMATT) = MOD(FMT,100)
         ITEMW(NUMATT) = MOD(FMT,100)
         IF (SVM.NE.KZSCA) THEN
            ITEMW(NUMATT) = ITEMW(NUMATT)
            NUMCOL(NUMATT) = PGRAPH * (ITEMW(NUMATT)+2)  - 2
         ENDIF
         CURPOS(NUMATT) = 0
         ATYPE(NUMATT) = ATTYPE
         LEN(NUMATT) = NWORDS
         IF(ATTYPE.EQ.KZTEXT)LEN(NUMATT) = ATTCHA
         SINGLE(NUMATT) = 0
         ROWD(NUMATT) = ICOL
         COLD(NUMATT) = 0
         IF (ICOL.NE.0) COLD(NUMATT) = NWORDS/ICOL
         VAR(NUMATT) = NWORDS.EQ.0
         GOTO 100
C
C     FIXED-TEXT PRINT
C
  300    FP(NUMATT) = PGPPTR
         LEN(NUMATT) = IDL(3)
         ITEMW(NUMATT) = IDL(3)
         W = LXLENW(3)
         ATYPE(NUMATT) = 0
         VAR(NUMATT) = .FALSE.
C     TEXT DATA IS SAVED IN BUFFER
         IF (.NOT.PGSTOR(ASCREC(IDP(3)),W)) GOTO 8000
         GO TO 100

C
C     END OF CLAUSE
  500    IF (ITEMS.GT.1) THEN
            IF (.NOT.EQKEYW(2,BLKTYP)) THEN
               CALL MSG('E','EXPECTED ''END ' // BLKTYP // '''',' ')
               GOTO 9000
            ENDIF
         ENDIF
         PGCPRT = .TRUE.
         RETURN

C
C     ERRORS
C
 8000    CALL MSG('E','THE PROGRAM IS TOO LARGE',' ')

 9000    PGCPRT = .FALSE.
         RETURN
      END
