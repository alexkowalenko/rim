      SUBROUTINE F3CLO

         USE RandomFiles, only : RIOCLO, RIOOUT
         USE Utils, only : ZEROIT, ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    CLOSE THE B-TREE RANDOM IO FILE - FILE 3
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'f3com.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'btbuf.inc'
         INCLUDE 'flags.inc'
C
C  SEQUENCE THROUGH THE INCORE BLOCKS LOOKING FOR WRITE FLAGS.
C
         DO 100 NUMB=1,NUMIC
            IF(ICORE(2,NUMB).EQ.0) GO TO 100
C
C  WRITE IT OUT.
C
            ISTRT = (NUMB-1) * LENBF3 + 1
            CALL RIOOUT(FILE3,ICORE(3,NUMB),CORE(ISTRT),LENBF3,IOS)
            IF(IOS.NE.0) RMSTAT = 2300 + IOS
  100    CONTINUE
C
C  WRITE OUT THE CONTROL BLOCK.
C
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
C
C     SYSTEM DEPENDENT CLOSE ROUTINE
C
         CALL RIOCLO(FILE3)
         RETURN
      END
