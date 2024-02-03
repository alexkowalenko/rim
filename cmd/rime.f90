PROGRAM MAIN

   USE, intrinsic :: iso_fortran_env

   USE Parameters
   USE Globals, only: KRMRNF, DBFNAM
   USE DateTime, only: RMTIME, RMDATE
   USE Lexer, only: KXINT, KXKEYW, TOKTYP, ASCREC, IDP, IDL, KWS, ITEMS, EQKEYW
   USE RandomFiles, only: RIOOPN, RIOCLO, RIOIN, RIOOUT
   USE System, only: SystemExit, SystemInitialise, SYSDBG, SYSDBN
   USE Text, only: STRMOV
   USE Utils, only: ZMOVE, HTOI, ITOH

   !
   !  ****************************************************************
   !
   !  THIS PROGRAM IS A FILE EDITOR FOR THE RIM DBMS
   !
   ! BY JIM FOX, UNIV OF WASHINGTON (ACS)
   !

   IMPLICIT NONE

   INCLUDE 'ascpar.inc'
   INCLUDE 'tokens.inc'
   INCLUDE 'rmatts.inc'
   INCLUDE 'rimcom.inc'
   INCLUDE 'files.inc'
   INCLUDE 'prom.inc'
   !
   CHARACTER (ZFNAML) :: RIMDB1,RIMDB2,RIMDB3,RIMDBX
   !
   COMMON /GRIMEX/ CURF, CURR, CUREL(Z), IOS, FILBUF(4096), UNIT(3), RLEN(3)
   INTEGER :: CURF, CURR, CUREL, IOS, FILBUF, UNIT, RLEN
   !
   INTEGER :: RBUF(5), DW(2)
   INTEGER :: F1HDR(ZF1HDR), F2HDR(ZF2HDR), F3HDR(ZF3HDR)
   EQUIVALENCE (F1,UNIT(1)),(F2,UNIT(2)),(F3,UNIT(3))
   INTEGER :: F1, F2, F3
   CHARACTER(len=40) :: ERRFIL
   CHARACTER(len=6) :: F1TXT(10),F2TXT(8),F3TXT(11)
   CHARACTER(len=6) :: RELTXT(10),ATTTXT(8),LNKTXT(6)

   INTEGER :: D, D1, D2, FMT, FMTLEN, H1, H2, NEWR
   INTEGER :: NW, NXTAR, NXTLR, NXTRR, NXTUP, R, RW, STAT, STTUP, SW, W
   INTEGER :: TDAY, TTIM
   INTEGER :: NUMOPN = 0

   !
   ! LABELS FOR DISPLAYS

   DATA F1TXT /'KDBHDR','VERS..','DBNAME','OWNER ', &
      'DBDATE','DBTIME','LF1REC','NRROW ','NAROW ','NLROW '/
   DATA F2TXT /'KDBHDR','VERS..','DBNAME','OWNER ', &
      'DBDATE','DBTIME','LF2REC','LF2WRD'/
   DATA F3TXT /'KDBHDR','VERS..','DBNAME','OWNER ', &
      'DBDATE','DBTIME','LF3REC','MOTREC','MOTADD', &
      'FRC...','FMC...'/
   DATA RELTXT /'NEXT..','NAME  ','RDATE ','NCOL  ','NATT  ', &
      'NTUPLE','RSTART','REND  ','RPW   ','MPW   '/
   DATA ATTTXT /'NEXT..','ATTNAM','RELNAM','ATTCOL', &
      'ATTLEN','ATTYPE','ATTKEY','ATTFOR'/
   DATA LNKTXT /'NEXT..','NAME  ','A1NAM ','R1NAM ','A2NAM ', &
      'R2NAM '/
   !
   !
   !  INITIALIZE AND GO TO THE COMMAND DISPATCHER
   !
   ! DATA UNIT /30,31,32/
   UNIT(1) = 30
   UNIT(2) = 31
   UNIT(3) = 32
   ! DATA RLEN /ZF1,ZF2,ZF3/
   RLEN(1) = ZF1
   RLEN(2) = ZF2
   RLEN(3) = ZF3

   BATCH = .FALSE.
   ECHO = .FALSE.

   CALL RMCONS
   CALL RMINIT
   CALL SystemInitialise
   ! DON'T USE THE INIT FILE
   IF (NINT.NE.ZNINT) CALL SETIN(ZTRMIN)
   !
   !  GET THE DATE AND TIME
   !
   TDAY = RMDATE()
   TTIM = RMTIME()
   !
   !  SET THE PROMPT TO EDIT
   !
   CALL PRMSET('INIT','RIM-EDIT>>')
   !
   !  PRINT THE GRIMEDIT EXECUTION HEADER
   !
   CALL MSG(' ','RIM FILE EDITOR   ','+')
   CALL DMSG(TDAY,0,'+',KZDATE)
   CALL MSG(' ','  ','+')
   CALL DMSG(TTIM,0,' ',KZTIME)

   !
   !  SET THE TRAPS FOR RECOVERING ERRORS.
   !
   ! CALL SYSTRP
   !

   ! -----------------------------------------------------
   ! GET A COMMAND AND DO IT
   !
   X = 0
   !
100 CONTINUE
   CALL LODREC
   !
   ! GET THE COMMAND
   !
   IF (.NOT.TOKTYP(1,KXKEYW)) GOTO 800
   !
   !----
   !
   IF (EQKEYW(1,'OPEN')) THEN
      CALL  SYSDBG(2,STAT)
      CALL  SYSDBN(DBFNAM,RIMDB1,RIMDB2,RIMDB3,RIMDBX)

      CALL RIOCLO(F1)
      ERRFIL = 'OPENING ' // RIMDB1
      CALL RIOOPN(RIMDB1,F1,ZF1,IOS)
      IF (IOS.NE.X) GOTO 700
      ERRFIL = 'READING ' // RIMDB1
      CALL RIOIN(F1,1,F1HDR,ZF1HDR,IOS)
      IF (IOS.NE.X) GOTO 700
      CALL BLKDSP('FILE 1 HEADER RECORD',F1HDR,'ZIZZDDIIII',F1TXT,10)
      CALL RIOCLO(F2)
      ERRFIL = 'OPENING ' // RIMDB2
      CALL RIOOPN(RIMDB2,F2,ZF2,IOS)
      IF (IOS.NE.X) GOTO 700
      ERRFIL = 'READING ' // RIMDB2
      CALL RIOIN(F2,1,F2HDR,ZF2HDR,IOS)
      IF (IOS.NE.X) GOTO 700


      CALL RIOCLO(F3)
      ERRFIL = 'OPENING ' // RIMDB3
      CALL RIOOPN(RIMDB3,F3,ZF3,IOS)
      IF (IOS.NE.X) GOTO 700
      ERRFIL = 'READING ' // RIMDB3
      CALL RIOIN(F3,1,F3HDR,ZF3HDR,IOS)
      IF (IOS.NE.X) GOTO 700

      CURF = 1
      CURR = 0
      NXTRR = ZRELRI
      NXTAR = ZATTRI
      NXTLR = ZLNKRI
      NXTUP = 0
      STTUP = 0
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'HEADER'))  THEN
      IF (.NOT.TOKTYP(2,KXINT)) GOTO 800
      IF (IDI(2).EQ.1) &
         CALL BLKDSP('FILE 1 HEADER RECORD',F1HDR,'ZIZZDTIIII',F1TXT,10)
      IF (IDI(2).EQ.2) &
         CALL BLKDSP('FILE 2 HEADER RECORD',F2HDR,'ZIZZDTHH',F2TXT,8)
      IF (IDI(2).EQ.3) &
         CALL BLKDSP('FILE 3 HEADER RECORD',F3HDR,'ZIZZDTIIIII',F3TXT,11)
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'FILE'))  THEN
      IF (.NOT.TOKTYP(2,KXINT)) GOTO 800
      CURF = IDI(2)
      CURR = 0
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'PAGE')) THEN
      IF (.NOT.TOKTYP(2,KXINT)) GOTO 800
      CURR = IDI(2)
      ERRFIL = 'READING FILE'
      CALL RIOIN(UNIT(CURF),CURR,FILBUF,RLEN(CURF),IOS)
      IF(IOS.NE.0) GO TO 800
      CALL MSG(' ','CURRENT PAGE IS: ','+')
      CALL IMSG(CURF,1,'+')
      CALL IMSG(CURR,7,' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'RELATION')) THEN
      IF (CURF.NE.1) GOTO 800
      IF (ITEMS.EQ.1) THEN
         RW = NXTRR
      ELSE
         IF (KWS(2).EQ.'.') THEN
            RW = ZRELRI
         ELSE IF (TOKTYP(2,KXINT)) THEN
            RW = IDI(2)
         ELSE
            CALL FINDR(2,RW)
         ENDIF
      ENDIF
      IF (RW.LE.0) GOTO 800
      CALL GETRA(RW,R,W,ZRELR,ZRELL)
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(W,   5,'+')
      CALL MSG(' ',':',' ')
      CALL BLKDSP('RELATION TABLE',FILBUF(W),'IZIIIIHHZZ',RELTXT,10)
      NXTRR = ABS(FILBUF(W))
      CALL ZMOVE(CUREL,FILBUF(W+1))
      STTUP = ABS(FILBUF(W+9))
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'ATTRIBUTES')) THEN
      IF (CURF.NE.1) GOTO 800
      IF (ITEMS.EQ.1) THEN
         RW = NXTAR
      ELSE
         IF (KWS(2).EQ.'.') THEN
            RW = ZATTRI
         ELSE IF (TOKTYP(2,KXINT)) THEN
            RW = IDI(2)
         ELSE
            CALL FINDA(2,RW)
         ENDIF
      ENDIF
      IF (RW.LE.0) GOTO 800
      CALL GETRA(RW,R,W,ZATTR,ZATTL)
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(W,   5,'+')
      CALL MSG(' ',':',' ')
      CALL BLKDSP('ATTRIBUTE TABLE',FILBUF(W),'IZZIHIII',ATTTXT,8)
      NXTAR = ABS(FILBUF(W))
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'LINKS')) THEN
      IF (CURF.NE.1) GOTO 800
      IF (ITEMS.EQ.1) THEN
         RW = NXTLR
      ELSE
         IF (KWS(2).EQ.'.') THEN
            RW = ZLNKRI
         ELSE IF (TOKTYP(2,KXINT)) THEN
            RW = IDI(2)
         ELSE
            CALL FINDL(2,RW)
         ENDIF
      ENDIF
      IF (RW.LE.0) GOTO 800
      CALL GETRA(RW,R,W,ZLNKR,ZLNKL)
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(W,   5,'+')
      CALL MSG(' ',':',' ')
      CALL BLKDSP('LINK TABLE',FILBUF(W),'IZZZZZ',LNKTXT,6)
      NXTLR = ABS(FILBUF(W))
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'ITOH')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      W = IDI(2)
      IF (W.LE.4096) W = FILBUF(W)
      CALL ITOH(H1,H2,W)
      CALL IMSG(H1,8,'+')
      CALL IMSG(H2,8,' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'HTOI')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      H1 = IDI(2)
      H2 = IDI(3)
      CALL HTOI(H1,H2,W)
      CALL IMSG(W,12,' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'TEXT')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      W = IDI(2)
      IF (W.LE.4096) W = FILBUF(W)
      CALL AMSG(W,4,' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'DATE')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      W = IDI(2)
      IF (W.LE.4096) W = FILBUF(W)
      CALL DMSG(W,0,' ',KZDATE)
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'TIME')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      W = IDI(2)
      IF (W.LE.4096) W = FILBUF(W)
      CALL DMSG(W,0,' ',KZTIME)
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'REAL')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      W = IDI(2)
      IF (W.LE.4096) W = FILBUF(W)
      FMT = KRMRNF
      IF (ITEMS.EQ.3) CALL LXFMT(3,KZREAL,FMT,FMTLEN)
      CALL SELPUT(W,KZREAL,FMT,1,RBUF)
      CALL AMSG(RBUF,MOD(FMT,100),' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'DOUBLE')) THEN
      CALL IMSG(CURF,2,'+')
      CALL IMSG(CURR,5,'+')
      CALL IMSG(IDI(2),   5,'+')
      CALL MSG(' ',':','+')
      W = IDI(2)
      DW(1) = IDI(2)
      DW(2) = IDI(3)
      IF (W.LE.4096) THEN
         DW(1) = FILBUF(W)
         DW(2) = FILBUF(W+1)
      ENDIF
      FMT = KRMRNF
      IF (ITEMS.EQ.3) CALL LXFMT(3,KZDOUB,FMT,FMTLEN)
      CALL SELPUT(DW,KZDOUB,FMT,1,RBUF)
      CALL AMSG(RBUF,MOD(FMT,100),' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'STORE')) THEN
      W = IDI(2)
      IF (TOKTYP(3,KXINT)) THEN
         FILBUF(W) = IDI(3)
      ELSE
         CALL STRMOV(ASCREC(IDP(3)),1,IDL(3),FILBUF(W),1)
      ENDIF
      ERRFIL = 'WRITING FILE'
      CALL RIOOUT(UNIT(CURF),CURR,FILBUF,RLEN(CURF),IOS)
      IF(IOS.NE.0) GO TO 800
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'PARAMETERS')) THEN
      CALL MSG(' ','ZF1,ZF2,ZF3: ','+')
      CALL IMSG(ZF1,6,'+')
      CALL IMSG(ZF2,6,'+')
      CALL IMSG(ZF3,6,' ')
      CALL MSG(' ','ZRELRI = ','+')
      CALL IMSG(ZRELRI,5,' ')
      CALL MSG(' ','ZATTRI = ','+')
      CALL IMSG(ZATTRI,5,' ')
      CALL MSG(' ','ZLNKRI = ','+')
      CALL IMSG(ZLNKRI,5,' ')
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'TUPLE')) THEN
      IF (CURF.NE.2) GOTO 800
      SW = 0
      NEWR = CURR
      IF (ITEMS.EQ.1) THEN
         IF (NXTUP.NE.0) CALL ITOH(SW,NEWR,NXTUP)
      ELSE
         IF (KWS(2).EQ.'.') THEN
            CALL ITOH(SW,NEWR,STTUP)
         ELSE IF (IDI(2).GT.0) THEN
            SW = IDI(2)
         ENDIF
      ENDIF
      IF (SW.LE.0) GOTO 800
      IF (ITEMS.EQ.3 .AND. IDI(3).GT.0) NEWR = IDI(3)
      IF (NEWR.NE.CURR) THEN
         CURR = NEWR
         ERRFIL = 'READING FILE'
         CALL RIOIN(UNIT(CURF),CURR,FILBUF,RLEN(CURF),IOS)
         IF(IOS.NE.0) GO TO 800
         CALL MSG(' ','CURRENT PAGE IS: ','+')
         CALL IMSG(CURF,1,'+')
         CALL IMSG(CURR,7,' ')
      ENDIF

      NXTUP = ABS(FILBUF(SW))
      NW = FILBUF(SW+1)
      DO W = SW, SW+NW+1
         CALL IMSG(CURF,2,'+')
         CALL IMSG(CURR,5,'+')
         CALL IMSG(W,   5,'+')
         CALL MSG(' ',':','+')
         D = FILBUF(W)
         CALL IMSG(D,15,'+')
         CALL MSG(' ',' ','+')
         CALL AMSG(D,ZCW,'+')
         IF (W.EQ.SW) THEN
            D = IABS(D)
            CALL ITOH(D1,D2,D)
            CALL MSG(' ',' ','+')
            CALL IMSG(D1,7,'+')
            CALL IMSG(D2,7,'+')
         ENDIF
         CALL MSG(' ',' ',' ')
      END DO
      GOTO 100
   ENDIF


   IF (TOKTYP(1,KXINT)) THEN
      SW = IDI(1)
      IF (ITEMS.GT.1) THEN
         NW = IDI(2)
      ELSE
         NW = 1
      ENDIF
      DO W = SW, SW+NW-1
         CALL IMSG(CURF,2,'+')
         CALL IMSG(CURR,5,'+')
         CALL IMSG(W,   5,'+')
         CALL MSG(' ',':','+')
         D = FILBUF(W)
         CALL IMSG(D,15,'+')
         CALL MSG(' ',' ','+')
         CALL AMSG(D,ZCW,'+')
         D = IABS(D)
         IF (D.GT.ZHTOI) THEN
            CALL ITOH(D1,D2,D)
            CALL MSG(' ',' ','+')
            CALL IMSG(D1,7,'+')
            CALL IMSG(D2,7,'+')
         ENDIF
         CALL MSG(' ',' ',' ')
      END DO
      GOTO 100
   ENDIF

   IF (EQKEYW(1,'SET')) CALL RMSET(*100)
   IF (EQKEYW(1,'SHOW')) CALL RMSHOW(*100)

   !
   ! CMS COMMAND
   !
   IF (EQKEYW(1,'SYSTEM')) CALL RMZIP(*100)
   !
   !---- EXIT
   !
   IF (EQKEYW(1,'EXIT'))    GOTO 900
   IF (EQKEYW(1,'QUIT'))    GOTO 900
   IF (EQKEYW(1,'END'))     GOTO 900
   !
   ! UNRECOGNISED COMMAND -
   !
   CALL MSG('E','???',' ')
   GOTO 100
   !
700 CALL MSG('E','IOS = ','+')
   CALL IMSG(IOS,5,'+')
   CALL MSG(' ',' ' // ERRFIL ,' ')
   GOTO 100
   !
800 CALL MSG('E',' ',' ')
   GOTO 100
   !
   !
   ! EXIT
   !
900 CALL RIOCLO(F1)
   CALL RIOCLO(F2)
   CALL RIOCLO(F3)
   !
   IF(BATCH) GO TO 999
   IF(.NOT.CONNI) GO TO 999
   IF(.NOT.CONNO) CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
999 CONTINUE
   !
   !  POSSIBLE SYSTEM DEPENDANT EXIT ROUTINE
   !
   CALL SystemExit
   !
   CALL EXIT(0)

CONTAINS

   SUBROUTINE FINDR(ITEM,RW)
      INCLUDE 'syspar.inc'
      !
      ! RIME SUBROUTINE - LOOKUP RELATION BY NAME
      !
      INCLUDE 'dclar1.inc'
      LOGICAL :: EQ
      !
      COMMON /GRIMEX/ CURF, CURR, CUREL(Z), IOS, &
         FILBUF(4096), UNIT(3), RLEN(3)
      !
      !
      RW = ZRELRI
      CALL LXSREC(ITEM,RNAME,ZC)
100   CALL GETRA(RW,R,W,ZRELR,ZRELL)
      IF (IOS.NE.0) GOTO 900
      IF (EQ(FILBUF(W+1),RNAME)) RETURN
      RW = ABS(FILBUF(W))
      IF (RW.NE.0) GOTO 100
900   RW = 0
      RETURN
   END SUBROUTINE FINDR


   SUBROUTINE FINDA(ITEM,RW)
      INCLUDE 'syspar.inc'
      !
      ! RIME SUBROUTINE - LOOKUP ATTRIBUTE BY NAME
      !
      INCLUDE 'tokens.inc'
      INCLUDE 'dclar1.inc'
      LOGICAL :: EQ,NE
      !
      COMMON /GRIMEX/ CURF, CURR, CUREL(Z), IOS, &
         FILBUF(4096), UNIT(3), RLEN(3)
      !
      RW = ZATTRI
      CALL LXSREC(ITEM,ANAME,ZC)
100   CALL GETRA(RW,R,W,ZATTR,ZATTL)
      IF (IOS.NE.0) GOTO 900
      IF (NE(FILBUF(W+1+Z),CUREL)) GOTO 110
      IF (KWS(ITEM).EQ.'*') RETURN
      IF (EQ(FILBUF(W+1),ANAME)) RETURN
110   RW = ABS(FILBUF(W))
      IF (RW.NE.0) GOTO 100
900   RW = 0
      RETURN
   END SUBROUTINE FINDA


   SUBROUTINE FINDL(ITEM,RW)
      INCLUDE 'syspar.inc'
      !
      ! RIME SUBROUTINE - LOOKUP LINK BY NAME
      !
      INCLUDE 'dclar1.inc'
      LOGICAL :: EQ
      !
      COMMON /GRIMEX/ CURF, CURR, CUREL(Z), IOS, &
         FILBUF(4096), UNIT(3), RLEN(3)
      !
      !
      RW = ZLNKRI
      CALL LXSREC(ITEM,RNAME,ZC)
100   CALL GETRA(RW,R,W,ZLNKR,ZLNKL)
      IF (IOS.NE.0) GOTO 900
      IF (EQ(FILBUF(W+1),RNAME)) RETURN
      RW = ABS(FILBUF(W))
      IF (RW.NE.0) GOTO 100
900   RW = 0
      RETURN
   END SUBROUTINE FINDL


   SUBROUTINE GETRA(RW,R,W,RPB,RL)
      INCLUDE 'syspar.inc'
      !
      ! RIME SUBROUTINE - GET A RELATION/ATTRIBUTE RECORD
      !
      COMMON /GRIMEX/ CURF, CURR, CUREL(Z), IOS, &
         FILBUF(4096), UNIT(3), RLEN(3)
      !
      !
      IF (RW.LE.0) GOTO 800
      R = (RW / RPB) + 1
      W = (RW - (RW/RPB)*RPB - 1) * RL + 2
      IF (CURR.NE.R) THEN
         CURR = R
         CALL RIOIN(UNIT(CURF),CURR,FILBUF,RLEN(CURF),IOS)
         IF(IOS.NE.0) GO TO 800
      ENDIF
      RETURN
800   RW = 0
      CALL MSG('E','READING FILE 1, REC ','+')
      CALL IMSG(CURR,I5,'+')
      CALL MSG(' ','  STATUS: ','+')
      CALL IMSG(IOS,I5,' ')
      RETURN
   END SUBROUTINE GETRA

END PROGRAM MAIN
