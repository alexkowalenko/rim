      SUBROUTINE PGEXEC
      INCLUDE 'syspar.d'
C
C     EXECUTE A RIM PROGRAM
C
      INCLUDE 'ascpar.d'
      INCLUDE 'flags.d'
      INCLUDE 'cflags.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'buffer.d'
      INCLUDE 'tupler.d'
      INCLUDE 'tuplea.d'
      INCLUDE 'tuplel.d'
      INCLUDE 'files.d'
      INCLUDE 'rimcom.d'
      INCLUDE 'rimptr.d'
      INCLUDE 'selcom.d'
      INCLUDE 'srtcom.d'
      INCLUDE 'whcom.d'
      INCLUDE 'msgcom.d'
C
      INCLUDE 'pgmcom.d'
      INCLUDE 'expcom.d'
 
      INCLUDE 'dclar1.d'
      LOGICAL ALDONE, ATDONE
      LOGICAL PGEEXP, WHEVAL
      CHARACTER*(ZFNAML) FN
 
C     BUFFERS *** BLOCKS 5 - 7 MUST STAY FIXED
C
C     BLOCK 1 CONTAINS FILE 2 DATA         (LEN LENBF2)
C     BLOCK 2 CONTAINS FILE 2 DATA         (LEN LENBF2)
C     BLOCK 3 CONTAINS FILE 2 DATA         (LEN LENBF2)
C     BLOCK 4 WILL CONTAIN GTSORT DATA     (GTSBLK) (LEN MAXCOL)
C     BLOCK 5 CONTAINS THE VARIABLE LIST   (PGVBLK)
C     BLOCK 6 CONTAINS THE PROGRAM         (PGPBLK)
C     BLOCK 7 WILL CONTAIN IF/WHILE DATA   (WHSAVE)
C
C
      PGFLAG = .TRUE.
      SLLVL = 0
      IP = PGPBLK
 
C     ALLOCATE THE BLOCK FOR IF/WHILE CLAUSES
      CALL BLKDEF(7,ZWHCOM,1)
      WHSAVE = BLKLOC(7)
 
      HDRPTR = 0
      FTRPTR = 0
      BUFFER(RLNPTR) = 1
      BUFFER(RPGPTR) = 1
      SLVMHD = .FALSE.
C
100   CONTINUE
C
C     POSSIBLE SYSTEM INTERRUPTION
C
      IF (HXFLAG.NE.0) THEN
         CALL WARN(6,0,0)
         GOTO 9000
      ENDIF
C
      OP = MOD(BUFFER(IP),1000)
      PGLINE = BUFFER(IP)/1000
      IF (TRACE.GE.8) THEN
         CALL MSG('T','PGEXEC:','+')
         CALL IMSG(OP,5,'+')
         CALL IMSG(PGLINE,5,' ')
      ENDIF
 
      IF (OP.EQ.XISEL)    GOTO 1000
      IF (OP.EQ.XISELX)   GOTO 1050
      IF (OP.EQ.XISELE)   GOTO 3000
      IF (OP.EQ.XIPRI)    GOTO 1200
      IF (OP.EQ.XICMP)    GOTO 1300
      IF (OP.EQ.XIIF)     GOTO 1400
      IF (OP.EQ.XIIFX)    GOTO 3000
      IF (OP.EQ.XIWH)     GOTO 1500
      IF (OP.EQ.XIWHE)    GOTO 3000
      IF (OP.EQ.XIHDR)    GOTO 1600
      IF (OP.EQ.XIFTR)    GOTO 1650
      IF (OP.EQ.XINEWP)   GOTO 1680
      IF (OP.EQ.XISET)    GOTO 1700
      IF (OP.EQ.XIPROC)   GOTO 3000
      IF (OP.EQ.XIPROE)   GOTO 3000
      IF (OP.EQ.XIPROX)   GOTO 1900
      IF (OP.EQ.XIEXIT)   GOTO 5000
      CALL MSG('E','UNRECOGNISED INSTRUCTION',' ')
      GOTO 9000
 
 
C
C     SELECT INSTRUCTION
C
1000  IF (SLLVL.GT.0) THEN
C        SAVE PRIOR SELECT INFO
         CALL RMSAV(SLLVL)
         IF (NSOVAR.GT.0) THEN
C           SAVE GTSORT RECORD
            SLPTR = SLLVL + 10
            CALL BLKEXT(SLPTR,SSL,XXX)
            CALL BLKCHG(SLPTR,SSL+MAXCOL,1)
            SSP = BLKLOC(SLPTR)
            GTS = BLKLOC(8)
            CALL BLKMOV(BUFFER(SSP+SSL),BUFFER(GTS),MAXCOL)
         ENDIF
      ENDIF
      SLLVL = SLLVL + 1
      MRINDX = SLLVL
      INDCUR = SLLVL
      CALL ZMOVE(RNAME,BUFFER(IP+2))
      IF (LOCREL(RNAME).NE.0) GOTO 8000
      P = IP + 2 + Z
      CALL PGBRES('WH ',P)
      NS = BUFFER(P)
      KSTRT = BUFFER(P+1)
      P = P + 2
      CALL PGBRES('SRT',P)
1001  IF (NSOVAR.GT.0) THEN
         NKSORT = 1
         RMSTAT = 0
         CALL BLKUP
         IF (RMSTAT.NE.0) THEN
            CALL MSG('E','MAIN MEMORY FULL. CANNOT CONTINUE REPORT',' ')
            CALL IMSG(rmstat,6,' ')
            GOTO 9000
         ENDIF
         CALL SORT(NKSORT)
C        ABORT ON SORT ERROR
         IF (RMSTAT.NE.0) GOTO 9000
         CALL BLKDWN
         LENGTH = NCOL
         CALL GTSORT(MP,1,-1,LENGTH)
      ENDIF
      GOTO 3000
 
C
C     SELECT CONTINUE
C
1050  CONTINUE
      IF (NSOVAR.GT.0) THEN
         CALL GTSORT(TP,1,1,LENGTH)
      ELSE
         CALL RMLOOK(TP,1,1,LENGTH)
      ENDIF
 
      IF(RMSTAT.NE.0) THEN
         SLLVL = SLLVL - 1
         IF (SLLVL.GE.1) THEN
C           RECOVER PRIOR SELECTION INFO
            CALL RMRES(SLLVL)
            MRINDX = SLLVL
            IF (NSOVAR.GT.0) THEN
C              RESTORE GTSORT RECORD
               SLPTR = SLLVL + 10
               SSP = BLKLOC(SLPTR)
               CALL BLKEXT(SLPTR,SSL,XXX)
               CALL BLKCHG(8,MAXCOL,1)
               TP = BLKLOC(8)
               CALL BLKMOV(BUFFER(TP),BUFFER(SSP+SSL-MAXCOL),MAXCOL)
            ELSE
               NID = CID
               CALL GETDAT(1,NID,TP,LENGTH)
            ENDIF
         ENDIF
         IP = BUFFER(IP+1)
         GOTO 100
      ENDIF
      IP = IP + 2
      GOTO 100
 
 
C
C     PRINT
C
1200  CONTINUE
      P = BUFFER(IP+2)
      CALL PGBRES('SEL',P)
 
      DO 1210 II=1,NUMATT
1210  CURPOS(II) = 1
 
      L = 1
1220  ALDONE = .TRUE.
 
      IF (ULPP.GT.0 .AND. BUFFER(RLNPTR).EQ.1) THEN
         FFFLAG = 1
         CALL PGEPRT(HDRPTR)
      ENDIF
 
      CALL FILCH(LINE,1,UPRINL,BLANK)
 
      DO 1240 I = 1, NUMATT
      IF (LIN1(I).GT.L) THEN
         ALDONE = .FALSE.
         GOTO 1240
      ENDIF
 
C     FIX VARIABLE LENGTH ATTRIBUTES
      IF(VAR(I)) THEN
        JP = TP + FP(I) - 1
        JP = BUFFER(JP) + TP - 1
        LEN(I) = BUFFER(JP)
        CALL TYPER(ATYPE(I),SVM,TYP)
        IF(TYP.EQ.KZTEXT) LEN(I) = BUFFER(JP+1)
        IF(TYP.EQ.KZDOUB) LEN(I) = LEN(I)/2
        ROWD(I) = BUFFER(JP+1)
        IF(SVM.EQ.KZMAT) COLD(I) = LEN(I)/ROWD(I)
      ENDIF
 
      IF (FP(I).GT.0) THEN
         JP = TP + FP(I) - 1
         IF(VAR(I)) JP = BUFFER(JP) + TP + 1
      ELSE
         JP = 0 - FP(I)
      ENDIF
 
1239  IF (ATYPE(I).EQ.0) THEN
C        OUTPUT FIXED TEXT
         IF (LIN1(I).EQ.L)
     1      CALL STRMOV(BUFFER(FP(I)),1,LEN(I),LINE,COL1(I))
      ELSE
         CALL SELOUT(BUFFER(JP),I,ATDONE)
         ALDONE = ALDONE.AND.ATDONE
      ENDIF
 
1240  CONTINUE
 
      MSUNIT = NOUTR
      CALL AMSG(LINE,-UPRINL,' ')
      CALL FILCH(LINE,1,UPRINL,BLANK)
      BUFFER(RLNPTR) = BUFFER(RLNPTR) + 1
 
      IF (ULPP.GT.0 .AND. BUFFER(RLNPTR).GE.ULPP) THEN
         CALL PGEPRT(FTRPTR)
         BUFFER(RPGPTR) = BUFFER(RPGPTR) + 1
         BUFFER(RLNPTR) = 1
      ENDIF
      IF (.NOT.ALDONE) THEN
         L = L + 1
         GOTO 1220
      ENDIF
      GOTO 3000
 
C
C     COMPUTE
C
1300  P = BUFFER(IP+2)
      VP = BUFFER(P)
      P = P + 1
      CALL PGBRES('EXP',P)
      IF (.NOT.PGEEXP(VP,TP)) GOTO 8000
      GOTO 3000
 
C
C     IF INSTRUCTION
C
1400  CALL BLKMOV(BUFFER(WHSAVE),WHCOM0,ZWHCOM)
      P = IP + 3
      CALL PGBRES('WH ',P)
      IF (WHEVAL(TP)) THEN
         IP = BUFFER(IP+2)
      ELSE
         IP = BUFFER(IP+1)
      ENDIF
      CALL BLKMOV(WHCOM0,BUFFER(WHSAVE),ZWHCOM)
      GOTO 100
 
C
C     WHILE INSTRUCTION
C
1500  CALL BLKMOV(BUFFER(WHSAVE),WHCOM0,ZWHCOM)
      P = IP + 3
      CALL PGBRES('WH ',P)
      IF (WHEVAL(TP)) THEN
         IP = BUFFER(IP+2)
      ELSE
         IP = BUFFER(IP+1)
      ENDIF
      CALL BLKMOV(WHCOM0,BUFFER(WHSAVE),ZWHCOM)
      GOTO 100
 
C
C     HEADER INSTRUCTION
C
1600  P = BUFFER(IP+2)
      HDRLEN = BUFFER(P)
      HDRPTR = P + 1
      GOTO 3000
 
C
C     FOOTER INSTRUCTION
C
1650  P = BUFFER(IP+2)
      FTRLEN = BUFFER(P)
      FTRPTR = P + 1
      GOTO 3000
 
C
C     NEWPAGE INSTRUCTION
C
1680  IF (ULPP.GT.0 .AND. BUFFER(RLNPTR).LT.ULPP) THEN
         DO 1685 I = BUFFER(RLNPTR), ULPP
1685     CALL MSG('R',' ',' ')
         CALL PGEPRT(FTRPTR)
         BUFFER(RPGPTR) = BUFFER(RPGPTR) + 1
         BUFFER(RLNPTR) = 1
      ENDIF
      IP = IP + 1
      GOTO 100
 
C
C     SET INSTRUCTION
C
1700  SETOP = BUFFER(IP+2)
      GOTO (1710,1720,1730,1740,1750) SETOP
      GOTO 3000
 
C     CASE
1710  IF (BUFFER(IP+3).EQ.0) THEN
         CASEIG = .TRUE.
      ELSE
         CASEIG = .FALSE.
      ENDIF
      GOTO 3000
 
C     INPUT FILE
1720  CALL STRASC(FN,BUFFER(IP+3),ZC)
      IF (FN.EQ.'TERMINAL') FN = ZTRMIN
      CALL SETIN(FN)
      GOTO 3000
 
C     OUTPUT FILE
1730  CALL STRASC(FN,BUFFER(IP+3),ZC)
      IF (FN.EQ.'TERMINAL') FN = ZTRMIN
      CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
      IF (STAT.EQ.0) GOTO 3000
      CALL MSG('E','COULD NOT OPEN ' // FN // 'FOR OUTPUT',' ')
      GOTO 8000
 
C     REPORT WIDTH
1740  UPRINL = BUFFER(IP+3)
      IF (UPRINL.LE.0 .OR. UPRINL.GT.ZPRINL) UPRINL = ZPRINL
      GOTO 3000
 
C     REPORT HEIGHT
1750  ULPP = BUFFER(IP+3)
      IF (ULPP.LT.0) ULPP = 0
      GOTO 3000
 
C
C     PROCEDURE EXECUTION
C
1900  P = BUFFER(IP+1)
      BUFFER(BUFFER(P-1)-1) = IP+2
      IP = P
      GOTO 100
 
C
C
C     NEXT OP
C
3000  IP = BUFFER(IP+1)
      GOTO 100
 
C
C     END REPORT (FILL OUT LAST PAGE)
C
5000  CONTINUE
      IF (ULPP.GT.0 .AND. BUFFER(RLNPTR).LT.ULPP) THEN
         DO 5010 I = BUFFER(RLNPTR), ULPP
5010     CALL MSG('R',' ',' ')
         CALL PGEPRT(FTRPTR)
         FFFLAG = 1
         CALL MSG('R',' ',' ')
      ENDIF
 
      CALL MSG(' ','REPORT COMPLETE',' ')
      GOTO 9900
C
C
C     ERRORS AND ETC. (SHOULD NOT BE ENCOUNTERED)
C     (MUST CLEANUP SYSTEM ON ABNORMAL EXIT)
C
8000  CALL MSG('E','EXECUTION ERROR',' ')
C     ERROR EXIT
9000  CALL MSG(' ','AT LINE ','+')
      CALL IMSG(PGLINE,6,' ')
C     MUST RESET LOTS OF STUFF
      CALL DBOPEN(DBFNAM,.FALSE.)
      RMSTAT = 200
      GOTO 9999
 
C     GOOD EXIT
9900  RMSTAT = 0
9999  PGVBLK = 0
      PGFLAG = .FALSE.
      RETURN
      END
