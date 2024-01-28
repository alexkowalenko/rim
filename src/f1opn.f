      SUBROUTINE F1OPN(RIMDB1)

         USE RandomFiles, only : RIOOPN, RIOIN, RIOOUT
         USE Text, only : ASCTXT
         USE Utils, only : ZEROIT, ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   OPEN THE RIM DIRECTORY FILE - FILE 1
C
C  PARAMETERS:
C         RIMDB1--NAME OF THE FILE TO USE FOR FILE1
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'f1com.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'reltbl.inc'
         INCLUDE 'lnktbl.inc'
         INCLUDE 'flags.inc'
         INCLUDE 'cflags.inc'

         LOGICAL NE
         COMMON /USAGE/ CPTTD,CPSTRT,KUSAGE(20)
         INCLUDE 'dstats.inc'
         CHARACTER*(ZFNAML) RIMDB1

C
C  OPEN THE DIRECTORY FILE.
C
         CALL RIOOPN(RIMDB1,FILE1,LENBF1,IOS)
C
C---  CALL MSG(' ','F1OPN: ' // RIMDB1,'+')
C---  CALL IMSG(FILE1,3,'+')
C---  CALL IMSG(IOS,5,' ')
         IF(IOS.NE.0) RMSTAT = 2100 + IOS
C
C  READ IN THE FIRST RECORD FROM THIS FILE.
C
         CALL RIOIN(FILE1,1,RELBUF,LENBF1,IOS)
         IF(IOS.NE.0) GO TO 500
         CRREC = 0
C
C     CHECK THAT THIS IS A DATABASE FILE
C
         IF(NE(RELBUF(ZFXHID),KDBHDR)) THEN
            RMSTAT = 10
            GO TO 1000
         ENDIF
C
C     CHECK FOR A TOO-OLD VERSION OF THE DB
C
         IF (RELBUF(ZFXHVS).LT.1000) THEN
            CALL MSG('WF',
     1        'YOUR DATABASE WAS CREATED BY AN OLDER VERSION OF RIM',
     1        ' ')
            CALL MSG(' ',
     2        'WHICH BUILT KEY POINTERS IMPROPERLY---YOU SHOULD',' ')
            CALL MSG(' ',
     3        'UNLOAD AND RELOAD THE DATABASE TO ENSURE VALID KEYS.',
     3        ' ')
         ENDIF

C
C  MOVE THE CONTROL DATA TO WHERE IT IS NEEDED.
C
         CALL ZMOVE(DBNAME,RELBUF(ZFXHDB))
         CALL ZMOVE(OWNER,RELBUF(ZFXHOW))
         DBDATE = RELBUF(ZFXHDT)
         DBTIME = RELBUF(ZFXHTM)
         LF1REC = RELBUF(ZF1HLR)
         NRROW = RELBUF(ZF1HNR)
         NAROW = RELBUF(ZF1HNA)
         NLROW = RELBUF(ZF1HNL)
CCC   CPTTD=FLOAT(RELBUF(ZF1H9))/100
CCC   CPSTRT=SECOND()
         CALL BLKMOV(KUSAGE,RELBUF(ZF1HDR+2),20)
C     IF(KUSAGE(1).EQ.0) CALL ZMOVE(KUSAGE(1),DBDATE)
C     CALL GETUN(KUSAGE(2))
C     CALL GETID(KUSAGE(3))
C     KUSAGE(4)=KUSAGE(4)+1
C
C  FILE 2 DELETE STATS.
C
CC    ISTAT = RELBUF(51)
CC    CPLAST= FLOAT(RELBUF(52))/100.
CC    IDELP = RELBUF(53)
C
C  SUCCESSFUL OPEN.
C
         DFLAG = .TRUE.
         DMFLAG = .TRUE.
         RMSTAT = 0
         GO TO 9999
C
C  EMPTY FILE 1 - WRITE THE FIRST RECORD ON IT.
C
  500    CONTINUE
         CALL ZEROIT(RELBUF,LENBF1)
         DO 505 I = 1,4
  505    CALL RIOOUT(FILE1,0,RELBUF,LENBF1,IOS)
C
         CALL ASCTXT(DBNAME,ZC,DBFNAM)
         LF1REC = 4
         CAREC = 0
         CRREC = 0
         CLREC = 0
         NRROW = ZRELRI
         NAROW = ZATTRI
         NLROW = ZLNKRI
         CPTTD=0
C     CPSTRT=SECOND()
         RMSTAT = 15
CCC   CALL RMDATE(KUSAGE(1))
CCCC  CALL GETUN(KUSAGE(2))
CCCC  CALL GETID(KUSAGE(3))
         KUSAGE(4)=0
         DO 107 II=5,20
  107    KUSAGE(II)=0
         GO TO 1000
C
C  UNABLE TO OPEN FILE 1.
C
 1000    CONTINUE
         DFLAG = .FALSE.
 9999    RETURN
      END
