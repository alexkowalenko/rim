      SUBROUTINE F2CLO

         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    CLOSE THE DATA RANDOM IO FILE - FILE 2
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'f2com.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'flags.inc'
C
C  SEQUENCE THROUGH THE BUFFERS LOOKING FOR WRITE FLAGS.
C
         REC1 = 0
         DO 400 NUMB=1,4
            IF(NUMB.EQ.4) GO TO 100
            IF(CURBLK(NUMB).EQ.1) GO TO 100
            IF(MODFLG(NUMB).EQ.0) GO TO 400
C
C  WRITE IT OUT.
C
            KQ1 = BLKLOC(NUMB)
            CALL RIOOUT(FILE2,CURBLK(NUMB),BUFFER(KQ1),LENBF2,IOS)
            IF(IOS.NE.0) RMSTAT = 2200 + IOS
            MODFLG(NUMB) = 0
            CURBLK(NUMB) = 0
            CALL BLKCLR(NUMB)
            GO TO 400
  100       CONTINUE
            IF(REC1.EQ.1) GO TO 400
            IF(NUMB.NE.4) GO TO 200
C
C  READ IN THE CONTROL BLOCK FIRST.
C
            CALL BLKCHG(1,LENBF2,1)
            KQ1 = BLKLOC(1)
            CALL RIOIN(FILE2,1,BUFFER(KQ1),LENBF2,IOS)
            IF(IOS.NE.0) RMSTAT = 2200 + IOS
            GO TO 300
C
C  WRITE OUT THE CONTROL BLOCK.
C
  200       CONTINUE
            KQ1 = BLKLOC(NUMB)
  300       CONTINUE
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
  400    CONTINUE
C
C     SYSTEM DEPENDENT CLOSE ROUTINE
C
         CALL RIOCLO(FILE2)
         RETURN
      END
