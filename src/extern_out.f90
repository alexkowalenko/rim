SUBMODULE (Extern) Extern_out
   implicit none

contains

   MODULE SUBROUTINE SETIN(FILE)
      !!
      !! SET THE INPUT FILE TO FILE
      !!     IF FILE IS THE TERMINAL THEN SWITCH TO UNIT NINT
      !!     ELSE SWITCH TO UNIT NINTA AND OPEN THE FILE
      !!
      USE RM_Parameters
      USE RM_Globals, only : CONNI, BATCH, NINT
      USE TextFiles, only : TIOOPN, TIOCLO

      CHARACTER(len=*), intent(in) :: FILE

      INCLUDE 'prom.inc'

      INTEGER :: STAT

      !
      !  SEE IF THE CURRENT INPUT FILE NEEDS TO BE CLOSED.
      !
      IF(NINT.NE.ZNINT) THEN
         CALL TIOCLO(FILE,NINT,'INPUT')
         NINT = ZNINT
      ENDIF
      !
      !  DETERMINE WHICH UNIT TO USE (TERMINAL IS ALWAYS OPEN)
      !
      CONNI = .TRUE.
      IF(FILE.NE.ZTRMIN) THEN
         CALL TIOOPN(FILE,ZNINTA,'INPUT',STAT)
         IF (STAT.EQ.0) THEN
            NINT = ZNINTA
            CONNI = .FALSE.
         ELSE
            ! NOTE.. ON ERROR MSG, MSG CALLS SETIN
            !        SO THIS CANNOT BE ERROR MESSAGE
            CALL MSG(' ','FILE NOT FOUND',' ')
         ENDIF
      ENDIF
      !
      IF(BATCH) CONNI = .FALSE.
      PRMPT = CONNI
      RETURN
   END SUBROUTINE SETIN


   MODULE SUBROUTINE SETOUT(UN,UNF,FILE,STAT)
      !!
      !! SET THE OUTPUT FILE
      !!     IF FILE IS THE TERMINAL THEN SET UN TO NOUT
      !!     ELSE SET UN TO UNF AND OPEN THE FILE
      !! IF COULD NOT OPEN FILE THEN SET STAT NE 0
      USE RM_Parameters
      USE RM_Globals, only : RMSTAT, CONNO, BATCH
      USE TextFiles, only : TIOOPN, TIOCLO
      USE Utils, only : ITOH

      INTEGER, intent(inout) :: UN
      INTEGER, intent(in) :: UNF
      CHARACTER(len=*), intent(in) :: FILE
      INTEGER, intent(out) :: STAT

      STAT = 0
      !
      !  SEE IF THE CURRENT OUTPUT FILE NEEDS TO BE CLOSED.
      !
      IF(UN.NE.ZNOUT) THEN
         CALL TIOCLO(FILE,UN,'OUTPUT')
         UN = ZNOUT
      ENDIF
      !
      !  DETERMINE WHICH UNIT TO USE
      !
      CONNO = .TRUE.
      IF(FILE.NE.ZTRMOU) THEN
         CALL TIOOPN(FILE,UNF,'OUTPUT',STAT)
         IF (STAT.EQ.0) THEN
            UN = UNF
            CONNO = .FALSE.
         ELSE
            CALL MSG('E','COULD NOT OPEN ''','+')
            CALL MSG(' ',FILE,'+')
            CALL MSG(' ',''' FOR OUTPUT.',' ')
            RMSTAT = 200
         ENDIF
      ENDIF
      !
      IF(BATCH) CONNO = .FALSE.
      RETURN
   END SUBROUTINE SETOUT


   MODULE SUBROUTINE PROMPT(PTXT)
      !!
      !! **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
      !!
      !! ISSUE TERMINAL PROMPT
      !!
      USE RM_Parameters
      USE RM_Globals, only: NINT, NOUT
      USE RM_Text, only : ABLANK, LOCASE, CHRASC

      INTEGER, intent(in) :: PTXT(*)
      INTEGER :: a, i, l

      character*(zc) p

      IF(nint.EQ.znint) then
         l = 0
         do i = 1, zc
            call gett(ptxt,i,a)
            if (a.eq.ablank) goto 11
            if (i.ne.1) a = locase(a)
            p(i:i) = chrasc(a)
            l = i
         end do
11       if (l.ne.0) write(nout,101) p(1:l)
101      FORMAT(a,$)
      endif
      RETURN
   END SUBROUTINE PROMPT


   MODULE SUBROUTINE PRMSET(MODE,PR)
      !!
      !! SET THE PROMPT CHARACTERS
      !!
      !! INPUT  - MODE... 'INIT' - SET INITIAL VALUES
      !!                  'SET'  - SET NEW VALUES
      !!                  'RESET'- RESTORE INITIAL VALUES
      !!          PR..... NEW VALUE FOR PROMPT
      !!
      USE RM_Parameters
      USE RM_Text, only : ASCCHR, LOCASE
      USE Utils, only : ZMOVE

      CHARACTER(len=*), intent(in) :: MODE,PR
      !
      INCLUDE 'prom.inc'
      CHARACTER(len=1) :: CH
      INTEGER :: AS, I
      !
      IF (MODE.EQ.'INIT' .OR. MODE.EQ.'SET') THEN
         DO I = 1, ZC
            IF (I.LE.LEN(PR)) THEN
               CH = PR(I:I)
            ELSE
               CH = ' '
            ENDIF
            AS = ASCCHR(CH)
            IF (I.GT.1) AS = LOCASE(AS)
            CALL PUTT(PROM,I,AS)
         END DO
      ENDIF
      !
      IF (MODE.EQ.'INIT') CALL ZMOVE(INIPRM,PROM)
      IF (MODE.EQ.'RESET') CALL ZMOVE(PROM,INIPRM)
      RETURN
   END SUBROUTINE PRMSET


   SUBROUTINE LXEDIT
      USE RM_Parameters
      USE RM_Globals, only : NINT
      USE Cards, only : CRDREC, CRDEND, CRDRLB, CRDRLL, CRDIDX
      USE RM_Text, only : UPCASE, ABLANK
      USE TextFiles, only : TIOIN
      USE Lexer, only : ASRCLL, ASLXEI, ASLXEE, ASLXEX, ASLXES

      !
      ! RECALL AND EDIT AN INPUT LINE OR COMMAND
      !
      INCLUDE 'prom.inc'
      !
      INTEGER :: EDTREC(ZCARDW)
      INTEGER :: CPYREC(ZCARDW)
      LOGICAL :: INSMOD
      INTEGER :: CP, CPCNT, CPYEND, CPYP, CRDP, ECH, EDTEND, EOF, I
      !
      ! RECALL AN INPUT RECORD
      !
      CP = CRDIDX
      CPCNT = 0
100   IF (CP.LT.1) CP = ZCARDN
      CPCNT = CPCNT + 1
      DO I = 1, ZCARDW
         CPYREC(I) = CRDRLB(I,CP)
      END DO
      CPYEND = CRDRLL(CP)
      IF (CPYEND.EQ.0 .OR. CPCNT.GT.ZCARDN) THEN
         CRDEND = 0
         GOTO 900
      ENDIF
      !
      ! ECHO RECALLED LINE AND GET EDITS
      !
150   CPYP = 1
      CRDP = 1
      CALL AMSG(CPYREC,CPYEND,' ')
      CALL TIOIN(NINT,EDTREC,EDTEND,EOF)
      IF (EOF.NE.0) GOTO 300
      IF(EDTEND.EQ.0) GO TO 300
      !
      ! IS IT ANOTHER RECALL COMMAND
      !
      IF (EDTEND.EQ.1) THEN
         CALL GETT(EDTREC,1,ECH)
         IF (UPCASE(ECH).EQ.ASRCLL) THEN
            CP = CP - 1
            GOTO 100
         ENDIF
      ENDIF
      !
      ! APPLY EDITS TO CPYREC
      !
      INSMOD = .FALSE.
      DO I = 1, EDTEND
         CALL GETT(EDTREC,I,ECH)
         !
         ! CHECK FOR '<' (BEGIN INSERT)
210      IF (ECH.EQ.ASLXEI .AND. .NOT. INSMOD) THEN
            INSMOD = .TRUE.
            GOTO 200
         ENDIF
         !
         ! CHECK FOR '>' (END INSERT)
         IF (ECH.EQ.ASLXEE .AND. INSMOD) THEN
            INSMOD = .FALSE.
            GOTO 200
         ENDIF
         !
         ! CHECK FOR '!' (END LINE)
         IF (ECH.EQ.ASLXEX .AND. .NOT. INSMOD) GOTO 320
         !
         ! CHECK FOR '#' (SKIP CHARACTER)
         IF (ECH.EQ.ASLXES .AND. .NOT. INSMOD) THEN
            CPYP = CPYP + 1
            GOTO 200
         ENDIF
         !
         ! CHECK FOR ' ' (KEEP CHARACTER)
         IF (ECH.EQ.ABLANK .AND. .NOT. INSMOD .AND. &
            CPYP.LE.CPYEND) CALL GETT(CPYREC,CPYP,ECH)
         !
         ! MOVE CHARACTER
190      CALL PUTT(CRDREC,CRDP,ECH)
         IF (.NOT. INSMOD) CPYP = CPYP + 1
         CRDP = CRDP + 1
         IF (CRDP.GT.ZCARDL) GOTO 320
200      CONTINUE
      END DO
      !
      ! COPY REST OF OLD LINE
      !
300   IF (CPYP.LE.CPYEND) THEN
         DO I = CPYP, CPYEND
            CALL GETT(CPYREC,I,ECH)
            CALL PUTT(CRDREC,CRDP,ECH)
            CRDP = CRDP + 1
            IF (CRDP.GT.ZCARDL) GOTO 320
         END DO
      ENDIF
320   CRDEND = CRDP -1
      INSMOD = .FALSE.
      IF (EOF.NE.0 .OR. EDTEND.EQ.0) GOTO 900
      !
      ! MORE EDITS
      !
      DO I = 1, ZCARDW
         CPYREC(I) = CRDREC(I)
      END DO
      CPYEND = CRDEND
      GOTO 150
      !
      ! DONE WITH EDITS
      !
900   RETURN
   END SUBROUTINE LXEDIT


   MODULE SUBROUTINE NXTCRD(EOF)
      !!
      !!  ROUTINE TO READ A RECORD TO /CARDS/
      !!!
      USE RM_Parameters
      USE Cards, only: CRDREC, CRDPTR, CRDEND, CRDRLB, CRDRLL, CRDIDX
      USE RM_Globals, only : TRACE, CONNI, NOUTT, NINT
      USE RM_Text, only : UPCASE, BLANK
      USE TextFiles, only : TIOIN
      USE Lexer, only : ASRCLL

      INTEGER, intent(out) :: EOF
      !!     EOF-----END-OF-FILE FLAG (0=NO, 1=YES)

      INCLUDE 'prom.inc'

      INTEGER :: CH, I

      !
      !
      !  READ A LINE FROM THE INPUT FILE
      !
100   CONTINUE
      IF(PRMPT) CALL PROMPT(PROM)
      CALL TIOIN(NINT,CRDREC,CRDEND,EOF)
101   IF (EOF.NE.0) GOTO 900
      !
      !  IF THERE IS NO DATA ON THE CARD READ ANOTHER ONE.
      !
      IF(CRDEND.EQ.0) GO TO 100
      CRDPTR = 0
      EOF = 0
      !
      ! LOOK FOR RECALL COMMAND
      !
150   IF (CONNI) THEN
         IF (CRDEND.LE.2) THEN
            CALL GETT(CRDREC,1,CH)
            IF (UPCASE(CH).EQ.ASRCLL) CALL LXEDIT
            IF (CRDEND.EQ.0) GOTO 100
         ENDIF
         !
         ! SAVE INPUT IN RECALL BUFFER
         !
         CRDIDX = CRDIDX + 1
         IF (CRDIDX.GT.ZCARDN) CRDIDX = 1
         DO I = 1, ZCARDW
            CRDRLB(I,CRDIDX) = CRDREC(I)
         END DO
         CRDRLL(CRDIDX) = CRDEND
      ENDIF
      !
      ! POSSIBLY TRACE INPUT
      !
      IF (TRACE.GT.0) THEN
         CALL MSG('T','INPUT RECORD',' ')
         DO I = 1, CRDEND
            MSGPTR = MSGPTR + 1
            CALL GETT(CRDREC,I,CH)
            CALL PUTT(MSGREC,MSGPTR,CH)
         END DO
         MSUNIT = NOUTT
         CALL AMSG(BLANK,0,' ')
      ENDIF
      RETURN
      !
      ! AT EOF RESET INPUT TO TERMINAL
      !
900   CONTINUE
      !--C  IF (NINT.NE.ZNINT) CALL MSG(' ','END OF INPUT FILE',' ')
      !---- CALL SETIN(ZTRMIN)
      RETURN
   END SUBROUTINE NXTCRD


   MODULE SUBROUTINE LOADIT(MAT,ATT)
      !!
      !!  THIS ROUTINE IS THE FORTRAN ROUTINE FOR LOADING DATA VALUES IN THE
      !!  RIM DATA BASE.
      !!
      USE RM_Parameters
      USE RM_Globals, only: HXFLAG
      USE RM_Buffer, only: ADDDAT
      USE Lexer, only: ITEMS, EQKEYW
      USE Message, only : WARN
      USE Parser, only: LODREC
      USE Utils, only : ZEROIT

      INCLUDE 'start.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      !
      INTEGER, intent(out) :: MAT(1)
      INTEGER, intent(in) :: ATT(ZTUPAL,1)
      !  PARAMETERS:
      !     MAT-----SCRATCH ARRAY FOR BUILDING TUPLES
      !     ATT-----ARRAY OF TUPLEA VALUES
      !
      INTEGER :: COLUMN
      INTEGER :: ENDCOL
      INTEGER :: I, IERR, IRR, J, NCOLT, NUMKEY
      !
      !  READ A CARD.
      !
100   CONTINUE
      !
      ! POSSIBLE SYSTEM INTERRUPTION
      !
      IF (HXFLAG.NE.0) THEN
         CALL WARN(6)
         GOTO 900
      ENDIF
      !
      CALL LODREC

      IF(EQKEYW(1,'LOAD')) GO TO 900
      IF(EQKEYW(1,'END')) GO TO 900
      !
      !  ASSUME THIS IS A DATA CARD.
      !
      !  ZERO OUT THE TUPLE.
      !
160   CALL ZEROIT(MAT,MAXCOL)
      !
      !  CHECK EACH ATTRIBUTE AND MOVE IT TO THE TUPLE FROM INPUT.
      !
      NUMKEY = 0
      J = 1
      ENDCOL = NCOL + 1
      IRR = 0
      DO I=1,NATT
         CALL BLKMOV(ATTNAM,ATT(1,I),ZTUPAL)
         COLUMN = ATTCOL
         IF(J.GT.ITEMS) THEN
            ! TOO FEW ITEMS
            CALL MSG('W','RECORD WAS IGNORED - TOO FEW ITEMS.',' ')
            GO TO 100
         ENDIF

         IF(ATTKEY.NE.0) NUMKEY = NUMKEY + 1
         !
         ! CALL PARVAL TO CRACK VALUE STRING
         !
         IF(ATTWDS.NE.0) THEN
            !
            !    FIXED ATTRIBUTE
            !
            CALL PARVAL(J,MAT(COLUMN),ATTYPE,ATTWDS,ATTCHA,0,IERR)
            IF(IERR.NE.0) IRR = 1
            GO TO 500
         ELSE
            !
            !    VARIABLE ATTRIBUTE
            !
            MAT(COLUMN) = ENDCOL
            NCOLT = ENDCOL + 1
            CALL PARVAL(J,MAT(ENDCOL+2),ATTYPE,ATTWDS,ATTCHA, &
               NCOLT,IERR)
            IF(IERR.NE.0) IRR = 1
            MAT(ENDCOL) = ATTWDS
            MAT(ENDCOL+1) = ATTCHA
            ENDCOL = ENDCOL + ATTWDS + 2
         ENDIF
500      CONTINUE
      END DO
      IF(IRR.NE.0) GO TO 100
      ENDCOL = ENDCOL - 1
      IF(J.LE.ITEMS) THEN
         ! TOO MANY ITEMS
         CALL MSG('W','RECORD WAS IGNORED - TOO MANY ITEMS.',' ')
         GO TO 100
      ENDIF
      NTUPLE = NTUPLE + 1
      CALL ADDDAT(1,REND,MAT,ENDCOL)
      IF(RSTART.EQ.0) RSTART = REND
      CALL RELPUT
      !
      !  PROCESS ANY KEY ATTRIBUTES.
      !
      IF(NUMKEY.EQ.0) GO TO 100
      CALL PRCKEY('ADD',MAT,ATT)
      GO TO 100
      !
      !  ALL DONE.
      !
900   CONTINUE
      RETURN
   END SUBROUTINE LOADIT


   MODULE SUBROUTINE LOADFM(MAT,ATT,FOR,NFOR)
      !!
      !! LOAD DATA VALUES FROM A FILE USING FORMAT.
      !!
      USE RM_Parameters
      USE RM_Globals, only : NINT
      USE RM_Buffer, only: ADDDAT
      USE Cards, only : CRDREC, CRDEND
      USE Formater, only : TYPER
      USE Lexer, only: KXTEXT, KXINT, KXREAL, IDT, ASCREC, IDP, IDL
      USE Lexer, only: KWS, ITEMS, IDI, IDR
      USE TextFiles, only : TIOIN
      USE RM_Text, only : ABLANK, ATOI, ATOR, ASCAN, STRMOV
      USE Utils, only : ZEROIT
      !  PARAMETERS:
      !     MAT-----SCRATCH ARRAY FOR BUILDING TUPLES
      !     ATT-----ARRAY OF TUPLEA VALUES
      !     FOR-----FORMAT ARRAY
      !        1) ATTCOL
      !        2) LINE NUMBER
      !        3) STARTING COLUMN NUMBER
      !        4) FIELD LENGTH PER ITEM (FROM FORMAT SPEC)
      !        5) FORMAT
      !        6) ITEM POSITION (LOADFM CALCULATES THIS)
      !     NFOR----NUMBER OF ITEMS IN FOR
      !
      INCLUDE 'buffer.inc'
      INCLUDE 'start.inc'
      INCLUDE 'rimptr.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'tupler.inc'
      INCLUDE 'dclar1.inc'
      !
      !  DECLARATION STATEMENTS
      !
      INTEGER, intent(out) :: MAT(MAXCOL)
      INTEGER, intent(in) :: ATT(ZTUPAL,1)
      INTEGER, intent(in out) :: FOR(6,1)
      INTEGER, intent(in) :: NFOR

      INTEGER :: CCOL, CLEN, CLN, COLUMN, ENDCOL, EOF, I, I2, IERR, II, IP, IROWS, IRR, J, N, N2, NCOLT, NI
      INTEGER :: NITMS, NUMKEY, NWORDS, SVM, TYP

      LOGICAL :: LOADMV
      LOGICAL :: EQ, NE
      !
      ! LOOP ON FORMAT
      !
10    CLN=0
      IP=0
      ITEMS=0
      !
      DO N = 1, NFOR

         IRR = 0
         IF (FOR(2,N).GT.CLN) THEN
            ! SKIP TO CORRECT LINE
            DO II=CLN,FOR(2,N)-1
               CALL TIOIN(NINT,CRDREC,CRDEND,EOF)
               IF (EOF.NE.0) GO TO 700
               CLN=CLN+1
            END DO
         ENDIF
         IF (FOR(1,N).EQ.0) GOTO 200
         !
         ! IT'S AN ATTRIBUTE.  FIND IT IN ATT
         !
         DO N2=1,NATT
            IF(ATT(ZA4-1,N2).EQ.FOR(1,N)) THEN
               CALL BLKMOV(ATTNAM,ATT(1,N2),ZTUPAL)
               GO TO 400
            ENDIF
         END DO
         CALL MSG('E','LOADFM ERROR: ATTRIBUTE NOT FOUND',' ')
         GOTO 9999

         !
         !  VALUES ARE READ IN ORDER THEY APPEAR IN LAYOUT.
         !  STORE ITEM POINTER IN FOR
         !
400      FOR(6,N) = ITEMS + 1
         CCOL=FOR(3,N)
         CLEN=FOR(4,N)
         CALL TYPER(ATTYPE,SVM,TYP)
         NITMS = ATTWDS
         IF (ATTWDS.EQ.0) NITMS = FOR(5,N)/10000

         IF(TYP.EQ.KZINT) THEN
            !
            !   INTEGER ATTRIBUTE
            !
            DO NI=1,NITMS
               IF (ATTWDS.EQ.0) THEN
                  ITEMS = ITEMS + 1
                  KWS(ITEMS) = '('
               ENDIF
               ITEMS=ITEMS+1
               IP=IP+1
               IDI(ITEMS)=0
               IDP(ITEMS)=IP
               IDT(ITEMS)=KXINT
               IDL(ITEMS)=1

               ! CHECK FOR MISSING VALUES
               IF (.NOT.LOADMV(CCOL,CLEN)) THEN
                  IF (.NOT. ATOI(CRDREC,CCOL,CLEN,IDI(ITEMS))) THEN
                     CALL MSG('E','EXPECTED INTEGER AT ','+')
                     CALL IMSG(CCOL,6,'+')
                     CALL IMSG(CLEN,6,' ')
                     IRR = 1
                  ENDIF
               ENDIF
               CCOL=CCOL+CLEN
            END DO
            IF (ATTWDS.EQ.0) THEN
               ITEMS = ITEMS + 1
               KWS(ITEMS) = ')'
            ENDIF
            !
         ELSE IF(TYP.EQ.KZREAL .OR. TYP.EQ.KZDOUB) THEN
            !
            !   REAL ATTRIBUTE
            !
            DO NI=1,NITMS
               IF (ATTWDS.EQ.0) THEN
                  ITEMS = ITEMS + 1
                  KWS(ITEMS) = '('
               ENDIF
               ITEMS=ITEMS+1
               IP=IP+1
               IDI(ITEMS)=0
               IDP(ITEMS)=IP
               IDT(ITEMS)=KXREAL
               IDL(ITEMS)=1

               ! CHECK FOR MISSING VALUES
               IF (.NOT.LOADMV(CCOL,CLEN)) THEN
                  IF (.NOT. ATOR(CRDREC,CCOL,CLEN,IDR(ITEMS))) THEN
                     CALL MSG('E','EXPECTED REAL AT ',' ')
                     CALL IMSG(CCOL,6,'+')
                     CALL IMSG(CLEN,6,' ')
                     IRR = 1
                  ENDIF
               ENDIF
               CCOL=CCOL+CLEN
            END DO
            IF (ATTWDS.EQ.0) THEN
               ITEMS = ITEMS + 1
               KWS(ITEMS) = ')'
            ENDIF
         ELSE
            !
            !   TEXT, DATE, OR TIME ATTRIBUTE
            !
            IP=IP+1
            ITEMS=ITEMS+1
            IDI(ITEMS)=0
            IDP(ITEMS)=IP
            IDT(ITEMS)=KXTEXT

            ! CHECK FOR MISSING VALUES
            IF (.NOT.LOADMV(CCOL,CLEN)) THEN
               ! STRIP TRAILING BLANKS ON VAR LEN TEXT
               IF (ATTWDS.EQ.0) THEN
                  CLEN = ASCAN(CRDREC,CCOL,0-CLEN,ABLANK,.FALSE.)
                  IF (CLEN.GT.0) CLEN = CLEN - CCOL + 1
                  IF (CLEN.EQ.0) CLEN = 1
               ENDIF
               IDL(ITEMS)=CLEN
               CALL STRMOV(CRDREC,CCOL,CLEN,ASCREC(IP),1)
               NWORDS=(CLEN-1)/ZCW + 1
               IP=IP+NWORDS-1
            ENDIF
         ENDIF
200      CONTINUE
      END DO
      !
      !
      !  WE HAVE REACHED EITHER END OF LAYOUT OR END OF DATA FILE
      !  IF THE FIRST LINE READ HAD EOF THIS MUST BE AN EMPTY FILE
      !
700   CONTINUE
      IROWS = IROWS + CLN
      IF (IROWS.EQ.0) THEN
         CALL MSG('E','THE DATA FILE IS EMPTY.',' ')
         GOTO 9999
      ENDIF
      IF(IRR.EQ.1) GO TO 1600
      IF(ITEMS.EQ.0) GO TO 1600
      !
      !  NOW LOAD THE TUPLE WITH EITHER AN ENTERED VALUE OR WITH NULL
      !
      !
      !  ZERO OUT THE TUPLE.
      !
      CALL ZEROIT(MAT,MAXCOL)
      NUMKEY = 0
      ENDCOL = NCOL + 1
      IRR=0
      DO I=1,NATT
         CALL BLKMOV(ATTNAM,ATT(1,I),ZTUPAL)
         COLUMN = ATTCOL
         IF (ATTKEY.NE.0) NUMKEY = NUMKEY+1
         DO I2=1,NFOR
            IF (ATTCOL.EQ.FOR(1,I2)) THEN
               J=FOR(6,I2)
               ATTFOR = FOR(5,I2)
               GO TO 750
            ENDIF
         END DO
         ! ATTRIBUTE HAS NO DATA - PUT NULL INTO MAT ARRAY
         IERR = -1
         J=1
         !
         !  CALL PARVAL TO PUT THE VALUE INTO MAT ARRAY
         !
750      IF (ATTWDS.NE.0) THEN
            CALL PARVAL(J,MAT(COLUMN),ATTYPE,ATTWDS,ATTCHA,0,IERR)
            IF(IERR.NE.0) IRR=1
         ELSE
            MAT(COLUMN) = ENDCOL
            NCOLT = ENDCOL + 1
            CALL PARVAL(J,MAT(ENDCOL+2),ATTYPE,ATTWDS,ATTCHA,NCOLT, &
               IERR)
            IF(IERR.NE.0) IRR=1
            MAT(ENDCOL) = ATTWDS
            MAT(ENDCOL+1) = ATTCHA
            ENDCOL = ENDCOL + ATTWDS + 2
         ENDIF
         !
         IF(IRR.EQ.1) GO TO 1600
      END DO
      ENDCOL = ENDCOL - 1
      NTUPLE = NTUPLE + 1
      CALL ADDDAT(1,REND,MAT,ENDCOL)
      IF(RSTART.EQ.0) RSTART = REND
      !
      !  PROCESS ANY KEY ATTRIBUTES.
      !
      IF (NUMKEY.EQ.0) GO TO 1600
      CALL PRCKEY('ADD',MAT,ATT)
      !
      ! IF IT'S NOT END OF DATA FILE DO ANOTHER RECORD
      !
1600  IF (EOF.EQ.0) GO TO 10
      CALL RELPUT

      ! AT END CLOSE THE FILE
9999  CALL SETIN(ZTRMIN)
      RETURN
   END SUBROUTINE LOADFM


   MODULE SUBROUTINE IMSG(NUM,NUMC,MCONT)
      !!
      !!  ROUTINE TO FORMAT AND PRINT AN INTEGER
      !!
      !!  Parameters
      !!
      !!     NUM-----INTEGER TO PRINT
      !!     NUMC----NUMBER OF CHARS (NEG = DELETE BLANKS)
      !!     MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
      !!

      USE RM_Parameters, only : ZCW, ZPRINW
      USE RM_Text, only : ITOA
      USE Utils, only : NDIGIT

      INTEGER, intent(in) :: NUM, NUMC
      CHARACTER(len=1), intent(in) :: MCONT
      !
      INTEGER, PARAMETER :: MAXL=24,MAXW=24/ZCW
      INTEGER :: NSTR(MAXW)
      INTEGER :: L, ERR
      !
      !
      L = NUMC
      IF (L.LT.0) L = MIN(NDIGIT(NUM),0-L)
      L = MIN(L,MAXL)
      CALL ITOA(NSTR,1,L,NUM,ERR)
      CALL AMSG(NSTR,L,MCONT)
      RETURN
   END SUBROUTINE IMSG


   MODULE SUBROUTINE DMSG(JDAT,DFMT,MCONT,TYP)
      !!
      !!  ROUTINE TO FORMAT AND PRINT A JULIAN DATE
      !!
      !!  Parameters
      !!
      !!     JDAT---JULIAN DATE OR TIME
      !!     DFMT----DATE/TIME FORMAT INTEGER
      !!     MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
      !!     TYP-----TYPE (KZDATE / KZTIME)
      !!
      USE RM_Parameters
      USE DateTime, only : ASCDAT

      INTEGER, intent(in) :: JDAT, DFMT, TYP
      CHARACTER(len=1) :: MCONT
      !
      INCLUDE 'rmatts.inc'
      !
      INTEGER, PARAMETER :: DATW=12/ZCW
      INTEGER :: ADAT(DATW), L
      !
      CALL ASCDAT(ADAT,1,L,JDAT,DFMT,TYP)
      CALL AMSG(ADAT,L,MCONT)
      RETURN
   END SUBROUTINE DMSG


   MODULE SUBROUTINE AMSG(MTEXT,NUMC,MCONT)
      !!
      !!  ROUTINE TO FORMAT AND PRINT ASCII-RM_Text
      !!
      !!  Parameters
      !!
      !!     MTEXT---RM_Text OF MESSAGE  (ASCII-RM_Text)
      !!
      !!     NUMC----NUMBER OF CHARS (NEG = DELETE TRAILING BLANKS)
      !!
      !!     MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
      !!
      USE RM_Parameters, only: ZPRINW, ZNOUT, ZPRINL
      USE RM_Globals, only : UTERML
      USE RM_Text, only : ABLANK, STRMOV
      USE TextFiles, only : TIOOUT

      INTEGER, intent(in) :: MTEXT(*)
      INTEGER, intent(in) :: NUMC
      CHARACTER(len=1), intent(in) :: MCONT
      !
      LOGICAL :: DTB
      INTEGER :: L, A, I, IERR, LNB
      !
      !
      IF (NUMC.EQ.0) GOTO 800
      L = NUMC
      DTB = .FALSE.
      IF (L.LT.0) THEN
         L = 0 - L
         DTB = .TRUE.
      ENDIF
      IF (L+MSGPTR.GT.ZPRINL) L = ZPRINL - MSGPTR
      !
      LNB = 0
      DO I = 1, L
         CALL GETT(MTEXT,I,A)
         CALL PUTT(MSGREC,MSGPTR+I,A)
         IF (A.NE.ABLANK) LNB = I
      END DO
      IF (DTB) L = LNB
      MSGPTR = MSGPTR + L
      !
      ! PRINT IF NO CONTINUATION
      ! (LINE MAY BE LONGER THAN TERMINAL LINE LENGTH)
      !
800   IF (MCONT.NE.' ') RETURN
810   L = MSGPTR
      IF (MSUNIT.EQ.ZNOUT .AND. L.GT.UTERML) L = UTERML
      CALL TIOOUT(MSUNIT,MSGREC,L,IERR)
      MSGPTR = MSGPTR - L
      IF (MSGPTR.EQ.0) RETURN
      CALL STRMOV(MSGREC,L+1,MSGPTR,MSGREC,1)
      GOTO 810
   END SUBROUTINE AMSG


   MODULE SUBROUTINE MSG(MTYPE,MTEXT,MCONT)
      !!
      !!  ROUTINE TO FORMAT AND PRINT MESSAGES
      !!
      !!  Parameters
      !!
      !! MTYPE---TYPE OF MESSAGE
      !!
      !!    1ST CHAR ' ' - INFORMATION RM_Text
      !!                 'W' - WARNING RM_Text
      !!             'E' - ERROR RM_Text
      !!             'R' - REPORT RM_Text  (UNIT IS NOUTR)
      !!             'L' - LOG ENTRY    (UNIT IS NOUTL)
      !!             'T' - TRACE ENTRY  (UNIT IS NOUTT)
      !!
      !!    2ND CHAR 'U' - LEAVE UPPERCASE
      !!             'F' - LOWERCASE ALL BUT FIRST CHARACTER
      !!             'L' - LOWERCASE
      !!             OTHER - SAME AS 'L'
      !!
      !! MTEXT---RM_Text OF MESSAGE
      !!
      !! MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
      !!
      !! MTYPE IS IGNORED IF THE MESSAGE BUFFER IS NOT EMPTY
      !!
      USE RM_Parameters
      USE RM_Globals, only : PGFLAG, CONNI, NOUT, NOUTR, NOUTT, NOUTL
      USE RM_Globals, only : ECHO, INLINE
      USE DateTime, only: RMTIME, RMDATE
      USE RM_Text, only : BLANK

      CHARACTER(len=*), intent(in) :: MTYPE
      CHARACTER(len=*), intent(in) :: MTEXT
      CHARACTER(len=1), intent(in) :: MCONT
      !
      INCLUDE 'rmatts.inc'
      !
      CHARACTER(len=1) :: CTYPE, MTYPE1
      INTEGER :: L, TDAY, TTIM
      !
      ! SETUP CASE CONVERSION CODE (FOR MESSAGE PRETTY PRINTING)
      !
      CTYPE = ' '
      IF (LEN(MTYPE).GE.2) CTYPE = MTYPE(2:2)
      IF (CTYPE.EQ.' ') THEN
         IF (MSGPTR.EQ.0) THEN
            CTYPE = 'F'
         ELSE
            CTYPE = 'L'
         ENDIF
      ENDIF
      !
      ! CHECK MTYPE IF NO MESSAGE PENDING
      !
      IF (MSGPTR.EQ.0) THEN
         MTYPE1 = MTYPE(1:1)
         IF ( (MTYPE1.EQ.'E' .OR. MTYPE1.EQ.'W') .AND. &
            (.NOT. (CONNI .OR. ECHO .OR. PGFLAG))) THEN
            ! IDENTIFY THE INPUT LINE
            CALL MSGCMV('LINE:','F')
            CALL IMSG(INLINE,6,' ')
         ENDIF
         IF (MTYPE1.EQ.'E') THEN
            CALL MSGCMV('YOUR REQUEST CANNOT BE COMPLETED.','F')
            MSUNIT = NOUT
            CALL AMSG(BLANK,0,' ')
            !--------   STOP INPUT FROM FILE ON ERROR
            !--------   CALL SETIN(ZTRMIN)
         ENDIF
         IF (MTYPE1.EQ.'W') CALL MSGCMV('* ','F')
         MSUNIT = NOUT
         IF (MTYPE1.EQ.'R') MSUNIT = NOUTR
         IF (MTYPE1.EQ.'L') MSUNIT = NOUTL
         IF (MTYPE1.EQ.'T') THEN
            MSUNIT = NOUTT
            CALL MSGCMV('* ','U')
            TDAY = RMDATE()
            TTIM = RMTIME()
            CALL DMSG(TDAY,0,'+',KZDATE)
            CALL MSGCMV(' ','U')
            CALL DMSG(TTIM,0,'+',KZTIME)
            CALL MSGCMV('* ','U')
         ENDIF
      ENDIF
      !
      L = LEN(MTEXT)
      IF (L+MSGPTR.GT.ZPRINL) L = ZPRINL - MSGPTR
      IF (L.GT.0) CALL MSGCMV(MTEXT(1:L),CTYPE)
      !
      ! PRINT IF NO CONTINUATION
      !
900   IF (MCONT.NE.'+') CALL AMSG(BLANK,0,MCONT)
      RETURN
   END SUBROUTINE MSG


   MODULE SUBROUTINE MSGCMV(MTEXT,CTYPE)
      !!
      !!  ROUTINE TO ADD CHARS TO THE OUTPUT LINE
      !!
      !!  Parameters
      !!
      !!     MTEXT---RM_Text OF MESSAGE
      !!
      !!     CTYPE---CASE CONVERSION CODE
      !!             'U' - LEAVE UPPERCASE
      !!             'F' - LOWERCASE ALL BUT FIRST CHARACTER
      !!             'L' - LOWERCASE
      !!             OTHER - SAME AS 'L'
      !!

      USE RM_Parameters, only : ZPRINW
      USE RM_Text, only : ASCCHR, LOCASE

      CHARACTER(len=*) :: MTEXT
      CHARACTER(len=*) :: CTYPE
      !
      !
      LOGICAL :: UCASE, FCHAR
      INTEGER :: I, L
      !
      UCASE = .FALSE.
      FCHAR = .FALSE.
      L = LEN(MTEXT)
      IF (CTYPE.EQ.'F') FCHAR = .TRUE.
      IF (CTYPE.EQ.'U') UCASE = .TRUE.
      !
      DO I = 1, L
         IF (FCHAR .OR. UCASE) THEN
            CALL PUTT(MSGREC,MSGPTR+I,ASCCHR(MTEXT(I:I)))
         ELSE
            CALL PUTT(MSGREC,MSGPTR+I,LOCASE(ASCCHR(MTEXT(I:I))))
         ENDIF
         IF (FCHAR .AND. (MTEXT(I:I).NE.' ')) FCHAR = .FALSE.
      END DO
      MSGPTR = MSGPTR + L
      RETURN
   END SUBROUTINE MSGCMV

END SUBMODULE Extern_out
