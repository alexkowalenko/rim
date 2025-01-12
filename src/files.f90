MODULE Files
   !! Low level file opening and closing
   USE RM_Parameters, only : ZICBL

   implicit none
   private

   INTEGER, public :: FILE1,LENBF1,LF1REC,CAREC,CRREC,CLREC
   !  VARIABLE DEFINITIONS:
   !     FILE1---UNIT FOR FILE1 - THE DIRECTORY FILE
   !     LENBF1--LENGTH OF BLOCKS ON FILE1
   !     LF1REC--LAST FILE1 RECORD USED
   !     CAREC---CURRENT ATTRIBUTE RECORD
   !     CRREC---CURRENT RELATION RECORD
   !     CLREC---CURRENT LINK RECORD

   INTEGER, public :: FILE2, LENBF2,LF2REC,LF2WRD
   INTEGER, public ::  CURBLK(3), MODFLG(3)
   !
   !  VARIABLE DEFINITIONS:
   !         FILE2---UNIT FOR FILE2 - THE DATA FILE
   !         LENBF2--LENGTH OF BLOCKS ON FILE2
   !         LF2REC--NEXT AVAILABLE RECORD ON FILE2
   !         LF2WRD--NEXT AVAILABLE WORD IN LF2REC
   !         CURBLK--CURRENT RECORDS IN CORE
   !         MODFLG--NON-ZERO FLAG INDICATES RECORD IN CORE HAS MODS

   INTEGER, public ::  FILE3,LENBF3,LF3REC,MOTREC,MOTADD,LAST,NUMIC,MAXIC,ICORE(3,ZICBL)
   !
   !  VARIABLE DEFINITIONS:
   !         FILE3---UNIT FOR FILE3 - THE B-TREE FILE
   !         LENBF3--LENGTH OF BLOCKS ON FILE3
   !         LF3REC--NEXT AVAILABLE RECORD ON FILE3
   !         MOTREC--LAST RECORD USED FOR MULTIPLE OCCURRENCE TABLE
   !         MOTADD--LAST WORD USED IN THAT RECORD
   !         LF3RCH--START OF FREE RECORD CHAIN
   !         LF3MCH--START OF FREE MOT CHAIN
   !         LAST----LAST RECORD REQUESTED
   !         NUMIC---CURRENT NUMBER OF INCORE BLOCKS
   !         MAXIC---MAXIMUM NUMBER OF INCORE BLOCKS POSSIBLE
   !         ICORE---ARRAY OF INCORE BLOCK DATA
   !             ROW 1 - NUMBER OF USES
   !             ROW 2 - WRITE FLAG (1=YES, 0=NO)
   !             ROW 3 - CORRESPONDING RECORD NUMBER

   ! FILE1 private common
   INTEGER :: CPTTD,KUSAGE(20)

   ! FILE3 private common
   INTEGER :: LF3RCH, LF3MCH

   public Initialise
   public F1OPN
   public F2OPN
   public F3OPN
   public REUSE
   public RMCLOS


CONTAINS

   SUBROUTINE Initialise
      USE RM_Parameters, only : ZNFIL1, ZF1, ZNFIL2, ZF2, ZNFIL3, ZF3
      !  /F1COM/
      FILE1 = ZNFIL1
      LENBF1 = ZF1
      LF1REC = 0
      CAREC = 0
      CRREC = 0
      CLREC = 0

      !  /F2COM/
      FILE2 = ZNFIL2
      LENBF2 = ZF2
      CURBLK = [0, 0, 0]
      MODFLG = [0, 0, 0]

      !  /F3COM/
      FILE3 = ZNFIL3
      LENBF3 = ZF3
      MAXIC = ZICBL
   END SUBROUTINE Initialise

   SUBROUTINE F1OPN(RIMDB1)
      !!
      !!  PURPOSE:   OPEN THE RIM DIRECTORY FILE - FILE 1
      !!
      !!  PARAMETERS:
      !!     RIMDB1--NAME OF THE FILE TO USE FOR FILE1
      !!
      USE RM_Parameters
      USE RM_Globals, only : DBNAME, OWNER, DBDATE, DBTIME, DFLAG, DMFLAG, DBFNAM, KDBHDR, RMSTAT
      USE RM_Attributes_Data, only: NAROW
      USE Extern, only: MSG
      USE RM_Links_Data, only: NLROW
      USE RandomFiles, only : RIOOPN, RIOIN, RIOOUT
      USE RM_Relations_Data, only: RELBUF, NRROW
      USE RM_Text, only : ASCTXT
      USE Utils, only : ZEROIT, ZMOVE

      CHARACTER*(ZFNAML), intent(in) :: RIMDB1

      LOGICAL :: NE

      INTEGER :: IOS, I, II

      !
      !  OPEN THE DIRECTORY FILE.
      !
      CALL RIOOPN(RIMDB1,FILE1,LENBF1,IOS)
      !
      !---  CALL MSG(' ','F1OPN: ' // RIMDB1,'+')
      !---  CALL IMSG(FILE1,3,'+')
      !---  CALL IMSG(IOS,5,' ')
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      !
      !  READ IN THE FIRST RECORD FROM THIS FILE.
      !
      CALL RIOIN(FILE1,1,RELBUF,LENBF1,IOS)
      IF(IOS.NE.0) GO TO 500
      CRREC = 0
      !
      ! CHECK THAT THIS IS A DATABASE FILE
      !
      IF(NE(RELBUF(ZFXHID),KDBHDR)) THEN
         RMSTAT = 10
         GO TO 1000
      ENDIF
      !
      ! CHECK FOR A TOO-OLD VERSION OF THE DB
      !
      IF (RELBUF(ZFXHVS).LT.1000) THEN
         CALL MSG('WF', &
            'YOUR DATABASE WAS CREATED BY AN OLDER VERSION OF RIM', &
            ' ')
         CALL MSG(' ', &
            'WHICH BUILT KEY POINTERS IMPROPERLY---YOU SHOULD',' ')
         CALL MSG(' ', &
            'UNLOAD AND RELOAD THE DATABASE TO ENSURE VALID KEYS.', &
            ' ')
      ENDIF

      !
      !  MOVE THE CONTROL DATA TO WHERE IT IS NEEDED.
      !
      CALL ZMOVE(DBNAME,RELBUF(ZFXHDB))
      CALL ZMOVE(OWNER,RELBUF(ZFXHOW))
      DBDATE = RELBUF(ZFXHDT)
      DBTIME = RELBUF(ZFXHTM)
      LF1REC = RELBUF(ZF1HLR)
      NRROW = RELBUF(ZF1HNR)
      NAROW = RELBUF(ZF1HNA)
      NLROW = RELBUF(ZF1HNL)
      !CC   CPTTD=FLOAT(RELBUF(ZF1H9))/100
      !CC   CPSTRT=SECOND()
      CALL BLKMOV(KUSAGE,RELBUF(ZF1HDR+2),20)
      ! IF(KUSAGE(1).EQ.0) CALL ZMOVE(KUSAGE(1),DBDATE)
      ! CALL GETUN(KUSAGE(2))
      ! CALL GETID(KUSAGE(3))
      ! KUSAGE(4)=KUSAGE(4)+1
      !
      !  FILE 2 DELETE STATS.
      !
      !C    ISTAT = RELBUF(51)
      !C    CPLAST= FLOAT(RELBUF(52))/100.
      !C    IDELP = RELBUF(53)
      !
      !  SUCCESSFUL OPEN.
      !
      DFLAG = .TRUE.
      DMFLAG = .TRUE.
      RMSTAT = 0
      GO TO 9999
      !
      !  EMPTY FILE 1 - WRITE THE FIRST RECORD ON IT.
      !
500   CONTINUE
      CALL ZEROIT(RELBUF,LENBF1)
      DO I = 1,4
         CALL RIOOUT(FILE1,0,RELBUF,LENBF1,IOS)
      END DO
      !
      CALL ASCTXT(DBNAME,ZC,DBFNAM)
      LF1REC = 4
      CAREC = 0
      CRREC = 0
      CLREC = 0
      NRROW = ZRELRI
      NAROW = ZATTRI
      NLROW = ZLNKRI
      CPTTD=0
      ! CPSTRT=SECOND()
      RMSTAT = 15
      !CC   CALL RMDATE(KUSAGE(1))
      !CCC  CALL GETUN(KUSAGE(2))
      !CCC  CALL GETID(KUSAGE(3))
      KUSAGE(4)=0
      DO II=5,20
         KUSAGE(II)=0
      END DO
      GO TO 1000
      !
      !  UNABLE TO OPEN FILE 1.
      !
1000  CONTINUE
      DFLAG = .FALSE.
9999  RETURN
   END SUBROUTINE F1OPN


   SUBROUTINE F1CLO
      !!
      !!  PURPOSE:   CLOSE THE RIM DIRECTORY FILE - FILE 1
      !!
      USE RM_Parameters
      USE RM_Globals, only : DBNAME, OWNER, DBDATE, DBTIME, KDBVER, KDBHDR, RMSTAT
      USE RM_Attributes_Data, only: ATTBUF, ATTMOD, NAROW
      USE RM_Links_Data, only: LNKBUF, NLROW, LNKMOD
      USE RandomFiles, only : RIOCLO, RIOOUT
      USE RM_Relations_Data, only: RELBUF, RELMOD, NRROW
      USE Utils, only : ZEROIT, ZMOVE

      INTEGER :: IOS

      !
      !  WRITE OUT THE RELATION BUFFER IF IT WAS MODIFIED.
      !
      IF(RELMOD.EQ.0) GO TO 100
      CALL RIOOUT(FILE1,CRREC,RELBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
100   CONTINUE
      CRREC = 0
      RELMOD = 0
      !
      !  WRITE OUT THE ATTRIBUTE BUFFER IF IT WAS MODIFIED.
      !
      IF(ATTMOD.EQ.0) GO TO 200
      CALL RIOOUT(FILE1,CAREC,ATTBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
200   CONTINUE
      CAREC = 0
      ATTMOD = 0
      !
      !  WRITE OUT THE LINK BUFFER IF IT WAS MODIFIED.
      !
      IF(LNKMOD.EQ.0) GO TO 300
      CALL RIOOUT(FILE1,CLREC,LNKBUF,LENBF1,IOS)
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
300   CONTINUE
      CLREC = 0
      LNKMOD = 0
      !
      !  ZERO OUT RELBUF AND MOVE CONTROL VARIABLES THERE.
      !
      CALL ZEROIT(RELBUF,LENBF1)
      CALL ZMOVE(RELBUF(ZFXHDB),DBNAME)
      CALL ZMOVE(RELBUF(ZFXHID),KDBHDR)
      CALL ZMOVE(RELBUF(ZFXHOW),OWNER )
      RELBUF(ZFXHVS) = KDBVER
      RELBUF(ZFXHDT) = DBDATE
      RELBUF(ZFXHTM) = DBTIME
      RELBUF(ZF1HLR) = LF1REC
      RELBUF(ZF1HNR) = NRROW
      RELBUF(ZF1HNA) = NAROW
      RELBUF(ZF1HNL) = NLROW
      !CCCDCRELBUF(ZF1HDR+1)=IFIX((CPTTD+SECOND()-CPSTRT)*100)
      CALL BLKMOV(RELBUF(ZF1HDR+2),KUSAGE,20)
      !C    RELBUF(51) = ISTAT
      !C    RELBUF(52) = INT(CPLAST*100.)
      !C    RELBUF(53) = IDELP
      !
      !  WRITE OUT THE CONTROL BLOCK.
      !
      CALL RIOOUT(FILE1,1,RELBUF,LENBF1,IOS)
      !CC   CALL BLKDSP('F1CLOSE',RELBUF, 'ZIZZIIIII')
      IF(IOS.NE.0) RMSTAT = 2100 + IOS
      !
      ! SYSTEM DEPENDENT CLOSE ROUTINE
      !
      CALL RIOCLO(FILE1)
      RETURN
   END SUBROUTINE F1CLO


   SUBROUTINE F2OPN(RIMDB2)
      !!
      !!  PURPOSE:    OPEN A DATA RANDOM IO PAGING FILE - FILE 2
      !!
      !!  PARAMETERS:
      !!    RIMDB2-----NAME OF THE FILE TO USE FOR FILE 2
      !!

      USE RM_Parameters
      USE RM_Globals, only : DBNAME, OWNER, DBDATE, DBTIME, KDBHDR, RMSTAT
      USE RM_Blocks, only: BLKDEF, BLKLOC
      USE RM_BufferData, only: BUFFER
      USE RandomFiles, only : RIOOPN, RIOIN, RIOOUT
      USE Utils, only : ZEROIT, ZMOVE


      CHARACTER*(ZFNAML), intent(in) :: RIMDB2

      INTEGER :: IOS, KQ1, KQ0
      LOGICAL :: NE

      !
      !  OPEN UP THE PAGED DATA FILE.
      !
      CALL RIOOPN(RIMDB2,FILE2,LENBF2,IOS)
      !
      !---  CALL MSG(' ','F2OPN: ' // RIMDB2,'+')
      !---  CALL IMSG(FILE2,3,'+')
      !---  CALL IMSG(IOS,5,' ')
      IF(IOS.NE.0) RMSTAT = 2200 + IOS
      !
      !  SEE IF THE FILE EXISTS YET. IF SO, READ CONTROL DATA.
      !
      CALL BLKDEF(1,LENBF2,1)
      KQ1 = BLKLOC(1)
      KQ0 = KQ1 - 1
      CALL RIOIN(FILE2,1,BUFFER(KQ1),LENBF2,IOS)
      IF(IOS.NE.0) GO TO 100
      IF(NE(KDBHDR,BUFFER(KQ0 + ZFXHID))) GO TO 8000
      IF(NE(OWNER,BUFFER(KQ0 + ZFXHOW))) GO TO 8000
      !C    IF(DBDATE.NE.BUFFER(KQ0 + ZFXHDT)) GO TO 8000
      !C    IF(DBTIME.NE.BUFFER(KQ0 + ZFXHTM)) GO TO 8000
      GO TO 10
      !
      !  CONTROL VALUES DO NOT MATCH.
      !
8000  CONTINUE
      RMSTAT = 12
10    CONTINUE
      LF2REC = BUFFER(KQ0 + ZF2HLR)
      LF2WRD = BUFFER(KQ0 + ZF2HNW)
      GO TO 200
      !
      !  INITIALIZE THE CONTROL VARIABLES.
      !
100   CONTINUE
      LF2REC = 1
      LF2WRD = 20
      !
      !  WRITE OUT THE CONTROL BLOCK FOR THE FIRST TIME.
      !
      CALL ZEROIT(BUFFER(KQ1),LENBF2)
      CALL ZMOVE(BUFFER(KQ0 + ZFXHDB),DBNAME)
      CALL ZMOVE(BUFFER(KQ0 + ZFXHID),KDBHDR)
      CALL ZMOVE(BUFFER(KQ0 + ZFXHOW),OWNER )
      BUFFER(KQ0 + ZFXHDT) = DBDATE
      BUFFER(KQ0 + ZFXHTM) = DBTIME
      BUFFER(KQ0 + ZF2HLR) = LF2REC
      BUFFER(KQ0 + ZF2HNW) = LF2WRD
      CALL RIOOUT(FILE2,0,BUFFER(KQ1),LENBF2,IOS)
      IF(IOS.NE.0) RMSTAT = 2200 + IOS
200   CONTINUE
      !
      !  INITIALIZE THE CONTROL BLOCKS.
      !
      CURBLK(1) = 1
      CURBLK(2) = 0
      CURBLK(3) = 0
      CALL ZEROIT(MODFLG,3)
      RETURN
   END SUBROUTINE F2OPN


   SUBROUTINE F2CLO
      USE RM_Parameters
      USE RM_Globals, only : DBNAME, OWNER, DBDATE, DBTIME, KDBVER, KDBHDR, RMSTAT
      USE RM_Blocks, only: BLKCHG, BLKCLR, BLKLOC
      USE RM_BufferData, only: BUFFER
      USE RandomFiles, only : RIOCLO, RIOIN, RIOOUT
      USE Utils, only : ZMOVE

      !
      !  PURPOSE:    CLOSE THE DATA RANDOM IO FILE - FILE 2
      !

      INTEGER :: REC1, NUMB, IOS, KQ1, KQ0

      !
      !  SEQUENCE THROUGH THE BUFFERS LOOKING FOR WRITE FLAGS.
      !
      REC1 = 0
      DO NUMB=1,4
         IF(NUMB.EQ.4) GO TO 100
         IF(CURBLK(NUMB).EQ.1) GO TO 100
         IF(MODFLG(NUMB).EQ.0) GO TO 400
         !
         !  WRITE IT OUT.
         !
         KQ1 = BLKLOC(NUMB)
         CALL RIOOUT(FILE2,CURBLK(NUMB),BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
         MODFLG(NUMB) = 0
         CURBLK(NUMB) = 0
         CALL BLKCLR(NUMB)
         GO TO 400
100      CONTINUE
         IF(REC1.EQ.1) GO TO 400
         IF(NUMB.NE.4) GO TO 200
         !
         !  READ IN THE CONTROL BLOCK FIRST.
         !
         CALL BLKCHG(1,LENBF2,1)
         KQ1 = BLKLOC(1)
         CALL RIOIN(FILE2,1,BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
         GO TO 300
         !
         !  WRITE OUT THE CONTROL BLOCK.
         !
200      CONTINUE
         KQ1 = BLKLOC(NUMB)
300      CONTINUE
         KQ0 = KQ1 - 1
         CALL ZMOVE(BUFFER(KQ0 + ZFXHDB),DBNAME)
         CALL ZMOVE(BUFFER(KQ0 + ZFXHID),KDBHDR)
         CALL ZMOVE(BUFFER(KQ0 + ZFXHOW),OWNER )
         BUFFER(KQ0 + ZFXHVS) = KDBVER
         BUFFER(KQ0 + ZFXHDT) = DBDATE
         BUFFER(KQ0 + ZFXHTM) = DBTIME
         BUFFER(KQ0 + ZF2HLR) = LF2REC
         BUFFER(KQ0 + ZF2HNW) = LF2WRD
         CALL RIOOUT(FILE2,1,BUFFER(KQ1),LENBF2,IOS)
         IF(IOS.NE.0) RMSTAT = 2200 + IOS
         REC1 = 1
         IF(NUMB.EQ.4) GO TO 400
         MODFLG(NUMB) = 0
         CURBLK(NUMB) = 0
400      CONTINUE
      END DO
      !
      ! SYSTEM DEPENDENT CLOSE ROUTINE
      !
      CALL RIOCLO(FILE2)
      RETURN
   END SUBROUTINE F2CLO


   SUBROUTINE F3OPN(RIMDB3)
      !!
      !!  PURPOSE:    OPEN A B-TREE RANDOM IO PAGING FILE - FILE 3
      !!
      !!  PARAMETERS:
      !!      RIMDB3----NAME OF THE FILE TO USE FOR FILE 3
      !!
      USE RM_Parameters
      USE RM_BTree_Data, only: CORE, START
      USE RM_Globals, only : DBNAME, OWNER, DBDATE, DBTIME, KDBHDR, RMSTAT
      USE RandomFiles, only : RIOOPN, RIOIN, RIOOUT
      USE Utils, only : ZEROIT, ZMOVE

      CHARACTER*(ZFNAML), intent(in) :: RIMDB3

      INTEGER :: IOS
      LOGICAL :: NE

      !
      !  OPEN UP THE BTREE AND MOT FILE.
      !
      CALL RIOOPN(RIMDB3,FILE3,LENBF3,IOS)
      !
      !---  CALL MSG(' ','F3OPN: ' // RIMDB3,'+')
      !---  CALL IMSG(FILE3,3,'+')
      !---  CALL IMSG(IOS,5,' ')
      IF(IOS.NE.0) RMSTAT = 2300 + IOS
      !
      !  SEE IF THE FILE EXISTS YET. IF SO, READ CONTROL DATA.
      !
      CALL RIOIN(FILE3,1,CORE,LENBF3,IOS)
      IF(IOS.NE.0) GO TO 100
      IF(NE(KDBHDR,CORE(ZFXHID))) GO TO 8000
      IF(NE(OWNER,CORE(ZFXHOW))) GO TO 8000
      !C    IF(DBDATE.EQ.CORE(ZFXHDT))) GO TO 8000
      !C    IF(DBTIME.NE.CORE(ZFXGTM))) GO TO 8000
      GO TO 10
      !
      !  CONTROL VALUES DO NOT MATCH.
      !
8000  CONTINUE
      RMSTAT = 12
10    CONTINUE
      LF3REC = CORE(ZF3HLR)
      MOTREC = CORE(ZF3HMO)
      MOTADD = CORE(ZF3HNM)
      LF3RCH = CORE(ZF3HRC)
      LF3MCH = CORE(ZF3HMC)
      GO TO 200
      !
      !  INITIALIZE THE CONTROL VARIABLES.
      !
100   CONTINUE
      START = 0
      LF3REC = 2
      MOTREC = 0
      MOTADD = LENBF3 + 1
      LF3RCH = 0
      LF3MCH = 0
      !
      !  WRITE OUT THE CONTROL BLOCK FOR THE FIRST TIME.
      !
      CALL ZEROIT(CORE,LENBF3)
      CALL ZMOVE(CORE(ZFXHDB),DBNAME)
      CALL ZMOVE(CORE(ZFXHID),KDBHDR)
      CALL ZMOVE(CORE(ZFXHOW),OWNER )
      CORE(ZFXHDT) = DBDATE
      CORE(ZFXHTM) = DBTIME
      CORE(ZF3HLR) = LF3REC
      CORE(ZF3HMO) = MOTREC
      CORE(ZF3HNM) = MOTADD
      CORE(ZF3HRC) = LF3RCH
      CORE(ZF3HMC) = LF3MCH
      CALL RIOOUT(FILE3,0,CORE,LENBF3,IOS)
      IF(IOS.NE.0) RMSTAT = 2300 + IOS
200   CONTINUE
      !
      !  INITIALIZE THE TREE COMMON BLOCK.
      !
      NUMIC = 0
      LAST = 0
      CALL ZEROIT(ICORE(1,1),3*ZICBL)
      RETURN
   END SUBROUTINE F3OPN


   SUBROUTINE F3CLO
      USE RM_Parameters
      USE RM_Globals, only : DBNAME, OWNER, DBDATE, DBTIME, KDBVER, KDBHDR, RMSTAT
      USE RM_BTree_Data, only: CORE
      USE RandomFiles, only : RIOCLO, RIOOUT
      USE Utils, only : ZEROIT, ZMOVE

      !
      !  PURPOSE:    CLOSE THE B-TREE RANDOM IO FILE - FILE 3
      !

      INTEGER :: NUMB, ISTRT, IOS

      !
      !  SEQUENCE THROUGH THE INCORE BLOCKS LOOKING FOR WRITE FLAGS.
      !
      DO NUMB=1,NUMIC
         IF(ICORE(2,NUMB).EQ.0) GO TO 100
         !
         !  WRITE IT OUT.
         !
         ISTRT = (NUMB-1) * LENBF3 + 1
         CALL RIOOUT(FILE3,ICORE(3,NUMB),CORE(ISTRT),LENBF3,IOS)
         IF(IOS.NE.0) RMSTAT = 2300 + IOS
100      CONTINUE
      END DO
      !
      !  WRITE OUT THE CONTROL BLOCK.
      !
      CALL ZEROIT(CORE,LENBF3)
      CALL ZMOVE(CORE(ZFXHDB),DBNAME)
      CALL ZMOVE(CORE(ZFXHID),KDBHDR)
      CALL ZMOVE(CORE(ZFXHOW),OWNER )
      CORE(ZFXHVS) = KDBVER
      CORE(ZFXHDT) = DBDATE
      CORE(ZFXHTM) = DBTIME
      CORE(ZF3HLR) = LF3REC
      CORE(ZF3HMO) = MOTREC
      CORE(ZF3HNM) = MOTADD
      CORE(ZF3HRC) = LF3RCH
      CORE(ZF3HMC) = LF3MCH
      CALL RIOOUT(FILE3,1,CORE,LENBF3,IOS)
      IF(IOS.NE.0) RMSTAT = 2300 + IOS
      !
      ! SYSTEM DEPENDENT CLOSE ROUTINE
      !
      CALL RIOCLO(FILE3)
      RETURN
   END SUBROUTINE F3CLO


   SUBROUTINE REUSE
      !!
      !!  PURPOSE:    RESET THE USAGE FLAGS TO OFF IN THE ICORE FLAGS
      !!
      INTEGER :: NUMB

      DO NUMB=1,NUMIC
         ICORE(1,NUMB) = 0
      END DO
      RETURN
   END SUBROUTINE REUSE


   SUBROUTINE RMCLOS
      !!
      !!  PURPOSE:   CLOSE A RIM DATABASE.
      !!

      USE RM_Globals, only : DFLAG, IFMOD, DBDATE, DBTIME, RMSTAT
      USE DateTime, only: RMTIME, RMDATE

      !  DO NOT CLOSE THE DATABASE IF THERE WERE NO MODIFICATIONS
      !
      RMSTAT = 0
      IF(.NOT.DFLAG) GOTO 999
      DFLAG = .FALSE.
      IF(.NOT.IFMOD) GOTO 999
      !
      !  RESET THE DATABASE DATE AND TIME.
      !
      DBDATE = RMDATE()
      DBTIME = RMTIME()
      !
      !  CLOSE THE THREE DATABASE FILES.
      !
      CALL F1CLO
      CALL F2CLO
      CALL F3CLO
999   DFLAG = .FALSE.
      IFMOD = .FALSE.
      RETURN
   END SUBROUTINE RMCLOS

END MODULE Files
