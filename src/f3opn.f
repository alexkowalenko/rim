      SUBROUTINE F3OPN(RIMDB3)

         USE Utils, only : ZEROIT, ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    OPEN A B-TREE RANDOM IO PAGING FILE - FILE 3
C
C  PARAMETERS:
C          RIMDB3----NAME OF THE FILE TO USE FOR FILE 3
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'f3com.inc'
         INCLUDE 'flags.inc'
         INCLUDE 'btbuf.inc'
         INCLUDE 'start.inc'
         INCLUDE 'rimcom.inc'

         LOGICAL NE
         CHARACTER*(ZFNAML) RIMDB3
C
C  OPEN UP THE BTREE AND MOT FILE.
C
         CALL RIOOPN(RIMDB3,FILE3,LENBF3,IOS)
C
C---  CALL MSG(' ','F3OPN: ' // RIMDB3,'+')
C---  CALL IMSG(FILE3,3,'+')
C---  CALL IMSG(IOS,5,' ')
         IF(IOS.NE.0) RMSTAT = 2300 + IOS
C
C  SEE IF THE FILE EXISTS YET. IF SO, READ CONTROL DATA.
C
         CALL RIOIN(FILE3,1,CORE,LENBF3,IOS)
         IF(IOS.NE.0) GO TO 100
         IF(NE(KDBHDR,CORE(ZFXHID))) GO TO 8000
         IF(NE(OWNER,CORE(ZFXHOW))) GO TO 8000
CC    IF(DBDATE.EQ.CORE(ZFXHDT))) GO TO 8000
CC    IF(DBTIME.NE.CORE(ZFXGTM))) GO TO 8000
         GO TO 10
C
C  CONTROL VALUES DO NOT MATCH.
C
 8000    CONTINUE
         RMSTAT = 12
   10    CONTINUE
         LF3REC = CORE(ZF3HLR)
         MOTREC = CORE(ZF3HMO)
         MOTADD = CORE(ZF3HNM)
         LF3RCH = CORE(ZF3HRC)
         LF3MCH = CORE(ZF3HMC)
         GO TO 200
C
C  INITIALIZE THE CONTROL VARIABLES.
C
  100    CONTINUE
         START = 0
         LF3REC = 2
         MOTREC = 0
         MOTADD = LENBF3 + 1
         LF3RCH = 0
         LF3MCH = 0
C
C  WRITE OUT THE CONTROL BLOCK FOR THE FIRST TIME.
C
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
  200    CONTINUE
C
C  INITIALIZE THE TREE COMMON BLOCK.
C
         NUMIC = 0
         LAST = 0
         CALL ZEROIT(ICORE(1,1),3*ZICBL)
         RETURN
      END
