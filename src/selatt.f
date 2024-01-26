      FUNCTION SELATT(FTOK,NTOK)

         USE Text, only : ASCCHR, ASCTXT
         USE Utils, only : ZMOVE, HTOI, ITOH

         INCLUDE 'syspar.inc'
C
C     PARSE ATTRIBUTE SELECT ATTRIBUTE LIST
C
         LOGICAL SELATT

         INCLUDE 'ascpar.inc'
         INCLUDE 'flags.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'lxlcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplel.inc'
         INCLUDE 'files.inc'
         INCLUDE 'selcom.inc'
         INCLUDE 'srtcom.inc'

         LOGICAL EQKEYW,END,ALLATT,EQTOK
         LOGICAL EQ,NE
         INTEGER STATUS
         INCLUDE 'dclar1.inc'
         LOGICAL SUMALL
         CHARACTER*3 FUN
         INTEGER LKNAM(Z)
C
C     INITIALIZE
C
         CALL FILCH(TITLE,1,ZPRINL,ABLANK)
         CALL FILCH(MINUS,1,ZPRINL,ABLANK)
         CALL FILCH(LINE,1,ZPRINL,ABLANK)
         NUMATT = 0
C
         END = .FALSE.
         SFUNCT = .FALSE.
         SLNKFL = 0
         SGRPBY = 0
C
         IF(EQTOK(FTOK,ASSTAR)) THEN
            ALLATT = .TRUE.
         ELSE
            ALLATT = .FALSE.
         ENDIF
         IT = FTOK
         LAST = FTOK + NTOK - 1
         SUMALL = .FALSE.
         IF(ALLATT .AND.(NTOK.NE.1)) THEN
            CALL MSG('E',
     +               '''*'' CANNOT BE COMBINED WITH OTHER COLUMNS.',' ')
            GO TO 9000
         ENDIF
C
         IP = 0
         PFUNCT = 0

         IF (ALLATT)  CALL LOCATT(BLANK,NAME)
C
C     LOOP ON ATTRIBUTES
C
   10    CONTINUE
C
C     GET NEXT ATTRIBUTE
C
         IF(ALLATT) GO TO 50
C
C     GET FROM LIST
C
         IF(IT.GT.LAST) GO TO 1000
         CALL LXSREC(IT,ANAME,ZC)
C
C     CHECK FOR FUNCTION
C
         IF (PFUNCT.EQ.0 .AND. EQTOK(IT+1,ASLPAR)) THEN
            IF (KWS(IT).EQ.'NUM') PFUNCT = 1
            IF (KWS(IT).EQ.'SUM') PFUNCT = 2
            IF (KWS(IT).EQ.'AVE') PFUNCT = 3
            IF (KWS(IT).EQ.'MIN') PFUNCT = 4
            IF (KWS(IT).EQ.'MAX') PFUNCT = 5
            IF (PFUNCT.NE.0) THEN
               SFUNCT = .TRUE.
               FUN = KWS(IT)
               IT = IT + 2
               GOTO 10
            ENDIF
         ENDIF
C
C     CHECK FOR END OF FUNCTION
C
         IF (PFUNCT.NE.0 .AND. EQTOK(IT,ASRPAR)) THEN
            PFUNCT = 0
            IT = IT + 1
            GOTO 10
         ENDIF
C
C     CHECK FOR LINKED ATTRIBUTE
C
         IF (EQTOK(IT+1,ASCOLN)) THEN
C       IS LINKED - GET TARGET RELATION
            IF (PFUNCT.NE.0) THEN
               CALL MSG('E','LINKED FUNCTION IS NOT MEANINGFUL',' ')
               GOTO 9000
            ENDIF
            DO 20 I = 1, ZMSLK
               PLNKFL = I
               IF (I.GT.SLNKFL) THEN
                  SLNKFL = I
                  CALL ZMOVE(SLNKNM(1,I),ANAME)
                  GOTO 22
               ENDIF
               IF (EQ(SLNKNM(1,I),ANAME)) GOTO 22
   20       CONTINUE
            CALL MSG('E','TOO MANY INDEPENDENT LINKS.',' ')
            GOTO 9000

   22       CALL ZMOVE(LKNAM,ANAME)
            IT = IT + 2
            CALL LXSREC(IT,ANAME,ZC)
            IF (LOCLNK(LKNAM).NE.0) THEN
               CALL MSG('E','THERE IS NO LINK NAMED ''','+')
               CALL AMSG(LKNAM,-ZC,'+')
               CALL MSG(' ','''.',' ')
               GOTO 9000
            ENDIF
            IF (NE(R1NAME,NAME)) THEN
               CALL MSG('E','LINK ''','+')
               CALL AMSG(LNAME,-ZC,'+')
               CALL MSG(' ',''' DOES NOT LINK FROM TABLE ''','+')
               CALL AMSG(NAME,-ZC,'+')
               CALL MSG(' ','''.',' ')
               GOTO 9000
            ENDIF
            CALL ZMOVE(RNAME,R2NAME)
         ELSE
C       IS NORMAL ATTRIBUTE
            PLNKFL = 0
            CALL ZMOVE(RNAME,NAME)
         ENDIF

         IT = IT + 1

         CALL LOCATT(ANAME,RNAME)
         CALL ATTGET(STATUS)
         IF(STATUS.NE.0) THEN
            CALL WARN(3,ANAME,RNAME)
            GO TO 9000
         ENDIF
         NUMATT = NUMATT + 1
         IF(NUMATT.GT.ZMSEL) GO TO 8040

C     CHECK FOR A SINGLE NORMAL ATTRIBUTE IN FUNCTION QUERY
         IF (PFUNCT.EQ.0 .AND. PLNKFL.EQ.0) THEN
            IF (SFUNCT .AND. SGRPBY.NE.0) THEN
               CALL MSG('E','YOU MAY SPECIFY ONLY ONE INDEPENDENT ' //
     1           'COLUMN.',' ')
               GOTO 9000
            ENDIF
            SGRPBY = NUMATT
            SORTYP(1) = .TRUE.
            VARPOS(1) = ATTCOL
            L=1
            CALL TYPER(ATTYPE,SVM,TYP)
            IF(TYP.EQ.KZREAL) L=2
            IF(TYP.EQ.KZDOUB) L=3
            IF(TYP.EQ.KZTEXT) L=4
            VARTYP(1) = L
            VARLEN(1) = ATTWDS
         ENDIF

         CALL TYPER(ATTYPE,SVM,TYP)
         SUMFLG(NUMATT) = .FALSE.
C
C     SEE IF MAT(I,J) OR VEC(I)
C
         SINGLE(NUMATT) = 0
         IF(EQTOK(IT,ASLPAR)) THEN
            NUM = 0
            IF(SVM.EQ.KZVEC) NUM = 1
            IF(SVM.EQ.KZMAT) NUM = 2
            IF(.NOT.((EQTOK(IT+2,ASRPAR) .AND. NUM.EQ.1) .OR.
     1               (EQTOK(IT+3,ASRPAR) .AND. NUM.EQ.2))) GOTO 8000
            I1 = IDI(IT+1)
            I2 = 1
            IF(NUM.EQ.2) I2 = IDI(IT+2)
            IF(I1.LE.0 .OR. I2.LE.0) GO TO 8060
            CALL ITOH(N1,N2,ATTLEN)
            IF(N2.NE.0) THEN
               IF(TYP.EQ.KZDOUB) N2 = N2/2
               IF(NUM.EQ.2) THEN
                  IF(N1.NE.0) N2 = N2/N1
                  IF(I1.GT.N1) GO TO 8020
                  IF(I2.GT.N2) GO TO 8020
               ELSE
                  IF(I1.GT.N2) GO TO 8020
               ENDIF
            ENDIF
            IT = IT + 2 + NUM
            SINGLE(NUMATT) = I1
            IF(NUM.EQ.2)CALL HTOI(I1,I2,SINGLE(NUMATT))
         ENDIF
C
C     LOOK FOR ATTRIBUTE FORMAT, TITLE, AND SUM FLAGS
C
         PITEMW = 0
         PGRAPH = 0
         PFORMT = 0
         PTITLE = 0
   40    IF(IT.GT.LAST) GO TO 100
         IF (EQTOK(IT,EQSIGN)) THEN
            IF (KWS(IT+1).EQ.'S') THEN
               SUMFLG(NUMATT) = .TRUE.
            ELSE
               GOTO 8080
            ENDIF
            IT = IT + 2
            GOTO 40
         ENDIF
         IF (EQTOK(IT,ATSIGN)) THEN
            CALL LXFMT(IT+1,ATTYPE,PFORMT,PITEMW)
            IF (PFORMT.EQ.0) GOTO 8080
            IT = IT + 2
            GOTO 40
         ENDIF
         IF (EQTOK(IT,PCSIGN)) THEN
            CALL LXSREC(IT+1,LINE,ZPRINL)
            PTITLE = IDL(IT+1)
            IT = IT + 2
            GOTO 40
         ENDIF
         GOTO 100
C
C     ALL ATTRIBUTES
C
   50    CALL ATTGET(STATUS)
         IF(STATUS.NE.0) GO TO 1000
         NUMATT = NUMATT + 1
         IF(NUMATT.GT.ZMSEL) GO TO 8040
         CALL TYPER(ATTYPE,SVM,TYP)
         PITEMW = 0
         PGRAPH = 0
         PFORMT = 0
         PFUNCT = 0
         LNKPTR = 0
         PLNKFL = 0
         PTITLE = 0
         SINGLE(NUMATT) = 0
         IF(SUMALL .AND. (ATTYPE.EQ.KZINT .OR. ATTYPE.EQ.KZREAL .OR.
     1       ATTYPE.EQ.KZDOUB)) THEN
            SUMFLG(NUMATT) = .TRUE.
         ELSE
            SUMFLG(NUMATT) = .FALSE.
         ENDIF
C
C
C     GOT ATTRIBUTE IN TUPLEA
C
  100    NC = 0
C
         IF (PTITLE.EQ.0) THEN
            PTITLE = LSTRNG(ATTNAM,1,ZC,BLANK,1,1)
            IF (PTITLE.EQ.0) PTITLE = ZC
            JP = 1
            IF(VAR(NUMATT) .AND. IP+PTITLE+10.LE.UPRINL) THEN
               IF(SVM.EQ.KZVEC) CALL ASCTXT(LINE,10,'DIM')
               IF(SVM.EQ.KZMAT) CALL ASCTXT(LINE,10,'ROW  COL')
               IF(SVM.NE.KZSCA) JP = 10
            ENDIF
            IF (PFUNCT.NE.0) THEN
               CALL ASCTXT(LINE,4,FUN // '(')
               CALL STRMOV(ATTNAM,1,PTITLE,LINE,5)
               CALL PUTT(LINE,PTITLE+5,ASRPAR)
               PTITLE = PTITLE + 5
            ELSE
               CALL STRMOV(ATTNAM,1,PTITLE,LINE,JP)
               PTITLE = PTITLE + JP - 1
            ENDIF
         ENDIF

         IP = IP + 2
         ICOL = ATTCHA
         NWORDS = ATTWDS
         IF(TYP.EQ.KZDOUB) NWORDS = NWORDS/2
         COL1(NUMATT) = IP
         ATYPE(NUMATT) = ATTYPE
         LEN(NUMATT) = NWORDS
         IF(ATTYPE.EQ.KZTEXT)LEN(NUMATT) = ICOL
         ROWD(NUMATT) = ICOL
         COLD(NUMATT) = 0
         IF(ICOL.NE.0) COLD(NUMATT) = NWORDS/ICOL
         VAR(NUMATT) = NWORDS.EQ.0
         FP(NUMATT) = ATTCOL
C
C     PUT IN SPECIAL FORMATS FOR FUNCTIONS
         IF (PFORMT.EQ.0) THEN
            IF (PFUNCT.EQ.1) PFORMT = KRMINF
            IF (PFUNCT.EQ.3 .AND. TYP.EQ.KZINT) PFORMT = KRMRNF
         ENDIF
         IF (PFORMT.EQ.0) PFORMT = ATTFOR
         IF (PFORMT.EQ.0) THEN
            IF (TYP.EQ.KZTEXT) PFORMT = MIN(LEN(NUMATT),20)
            IF (TYP.EQ.KZINT ) PFORMT = KRMINF
            IF (TYP.EQ.KZREAL) PFORMT = KRMRNF
            IF (TYP.EQ.KZDOUB) PFORMT = KRMRNF
            IF (TYP.EQ.KZDATE) PFORMT = KRMDTF
            IF (TYP.EQ.KZTIME) PFORMT = KRMTMF
         ENDIF
         IF (PGRAPH.EQ.0) THEN
            IF (TYP.EQ.KZTEXT) PGRAPH = PFORMT
            IF (TYP.NE.KZTEXT) PGRAPH = PFORMT/10000
         ENDIF
         IF(VAR(NUMATT) .AND. PGRAPH.EQ.0) THEN
C       VARIABLE DEFAULTS
            IF(TYP.EQ.KZTEXT) PGRAPH = 40
            IF(TYP.NE.KZTEXT) PGRAPH = 4
         ENDIF
C
         IF(ATTYPE.EQ.KZTEXT) THEN
            ITEMW(NUMATT) = 1
            NC = LEN(NUMATT)
            IF(PGRAPH.NE.0) NC = PGRAPH
            NC = MAX(NC,PTITLE)
         ELSE IF(ATTYPE.EQ.KZDATE .OR. ATTYPE.EQ.KZTIME) THEN
            ITEMW(NUMATT) = 1
            CALL DTFSPL(NC,X1,X2,X3,X4,X5,X6,PFORMT)
            NC = MAX(NC,PTITLE)
         ELSE
            ITEMW(NUMATT) = MOD(IABS(PFORMT),100)
            NC = LEN(NUMATT) * (2 + ITEMW(NUMATT)) - 2
            IF(PGRAPH.NE.0)NC = PGRAPH*(2+ITEMW(NUMATT))-2
            NC = MAX(NC,PTITLE)
         ENDIF
         IF(SINGLE(NUMATT).NE.0) NC = MAX(ITEMW(NUMATT),PTITLE) + 2
         IF(NC.LE.0) NC = 40
         IF(IP+NC.GT.UPRINL) THEN
            NUMATT = NUMATT - 1
            GOTO 900
         ENDIF
C
C
         FORMT(NUMATT) = PGRAPH*10000 + MOD(PFORMT,10000)
         FUNCT(NUMATT) = PFUNCT
         LNKFL(NUMATT) = PLNKFL
         IF (PFUNCT.NE.0) SFUNCT = .TRUE.
C
C     INSERT TITLE
C
         CALL STRMOV(LINE,1,PTITLE,TITLE,IP)
         NUMCOL(NUMATT) = NC
C
C     MAKE DASHES
C
         CALL FILCH(MINUS,IP,NC,ASCCHR('-'))
         IP = IP + NC
         GO TO 10
C
C     LINE TOO LONG
C
  900    CALL MSG('W','LINE TRUNCATED.',' ')
         CALL IMSG(IP,6,' ')
         CALL IMSG(NC,6,' ')

C
C     OK EXIT
C
 1000    SELATT = .TRUE.
         IF (SGRPBY.NE.0 .AND. SFUNCT) NSOVAR = 1
         RETURN
C
C     --- ERROR EXITS ---
C
C     WRONG TYPE FOR FOLLOWING PARENS
C
 8000    CALL MSG('E','COLUMN ''','+')
         CALL AMSG(ATTNAM,-ZC,'+')
         CALL MSG(' ',''' IS A SCALER.',' ')
         GO TO 9000
C
C     SINGLE TOO BIG
C
 8020    CALL MSG('E','INDEX IS OUT OF RANGE FOR COLUMN ''','+')
         CALL AMSG(ATTNAM,-ZC,'+')
         CALL MSG(' ','''.',' ')
         GO TO 9000
C
C  TOO MAY ATTRIBUTES SPECIFIED
C
 8040    CALL MSG('E', 'YOU HAVE REQUESTED TOO MANY COLUMNS.',' ')
         GO TO 9000
C
C     ROW/COL MUST BE POSITIVE INTEGER
C
 8060    CALL MSG('E', 'ROW/COL MUST BE POSITIVE INTEGERS.',' ')
         GO TO 9000
C
C     ISSUE CORRECT MODIFIER SYNTAX
C
 8080    CALL MSG('E',
     +            'COLUMN MODIFIERS ARE: =S, @FORMAT, AND %TITLE',' ')
         GO TO 9000
C
C
 9000    CONTINUE
 9999    NUMATT = 0
         SELATT = .FALSE.
         RETURN
      END
