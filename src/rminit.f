      SUBROUTINE RMINIT

         USE Utils, only : ZEROIT, ZMOVE

         INCLUDE 'syspar.inc'
C
C     RUN-TIME INITIALIZATION (CALLED WHEN DATABASE IS OPENED)
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'f1com.inc'
         INCLUDE 'f2com.inc'
         INCLUDE 'f3com.inc'
         INCLUDE 'incore.inc'
         INCLUDE 'reltbl.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'lnktbl.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'ptrcom.inc'
         INCLUDE 'pgmcom.inc'
C
C  /RELTBL/
         CALL ZMOVE(CNAME,BLANK)
         LRROW = 0
         NRROW = ZRELRI
         RELMOD = 0
         RPBUF = ZRELR

C
C  /TUPLER/
         CALL ZMOVE(NAME,BLANK)

C  /ATTBLE/
         CALL ZMOVE(CANAME,BLANK)
         CALL ZMOVE(CRNAME,BLANK)
         CRSTRT = 0
         CROW = 0
         LROW = 0
         NAROW = ZATTRI
         ATTMOD = 0
         APBUF = ZATTR

C  /LNKTBL/
         LLROW = 0
         NLROW = 0
         LPBUF = ZLNKR

C  /INCORE/
         CALL ZEROIT(BLOCKS(1,1),60)
         NEXT = 1
         LIMIT = ZBUF
         NUMBL = 0

C  /F1COM/
         FILE1 = ZNFIL1
         LENBF1 = ZF1
         LF1REC = 0
         CAREC = 0
         CRREC = 0
         CLREC = 0

C  /F2COM/
         FILE2 = ZNFIL2
         LENBF2 = ZF2
         DO 200 I=1,3
            CURBLK(I) = 0
            MODFLG(I) = 0
  200    CONTINUE

C  /F3COM/
         FILE3 = ZNFIL3
         LENBF3 = ZF3
         MAXIC = ZICBL

C  /RIMPTR/
         IVAL = 0
         CID = 0
         NID = 0
         NS = 0
         MID = 0
         INDCUR = ZIMISS
         INDMAX = 0

C  /SRTCOM/
         NSORTW = 25
         FIXLT = .TRUE.
         NSORT = 0
         NREAD = 0

C     /PTRCOM/
         NEXPOS = 0
         NEXPOT = 0

C     /PGMCOM/
         PGPBLK = 0
         PGVBLK = 0


         RETURN
      END
