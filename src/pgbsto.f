      LOGICAL FUNCTION PGBSTO(TYP)

         USE RM_BufferData, only: BUFFER

         INCLUDE 'syspar.inc'
C
C     STORE A COMMON BLOCK
C
C     INPUT:   PGM-----CODE FOR BLOCK TO STORE
C                    SEL = SELCOM
C                    WH  = WHCOM
C                    SRT = SRTCOM
C                    EXP = EXPCOM
C
         CHARACTER*3 TYP

         INCLUDE 'selcom.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'ptrcom.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'pgmcom.inc'
         INCLUDE 'expcom.inc'
         INCLUDE 'rimptr.inc'
         LOGICAL PGSTOR
C
         PGBSTO = .FALSE.
         P = PGPPTR

         IF (TYP.EQ.'SEL') THEN
C     SELCOM
            IF (.NOT.PGSTOR(0,-(1+13*NUMATT))) GOTO 8000
            CALL BLKMOV(BUFFER(P),NUMATT, 1)
            CALL BLKMOV(BUFFER(P+1          ),LIN1, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*1 ),COL1, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*2 ),ITEMW, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*3 ),CURPOS, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*4 ),NUMCOL, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*5 ),FORMT, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*6 ),ATYPE, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*7 ),LEN,   NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*8 ),ROWD,  NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*9 ),COLD,  NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*10),FP,    NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*11),SINGLE, NUMATT)
            CALL BLKMOV(BUFFER(P+1+NUMATT*12),VAR,   NUMATT)
            PGPPTR = P + 1 + NUMATT*13
            GOTO 9000

         ELSE IF (TYP.EQ.'WH ') THEN
C     WHCOM
            IF (.NOT.PGSTOR(0,-(3+8*NBOO+NEXPOS+NEXPOT))) GOTO 8000
            CALL BLKMOV(BUFFER(P),NBOO,  1)
            CALL BLKMOV(BUFFER(P+1),NEXPOS, 1)
            CALL BLKMOV(BUFFER(P+2),NEXPOT, 1)
            CALL BLKMOV(BUFFER(P+3       ),BOO,   NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*1),KATTP, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*2),KATTL, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*3),KATTY, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*4),KOMTYP, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*5),KOMPOS, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*6),KOMLEN, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*7),KOMPOT, NBOO)
            CALL BLKMOV(BUFFER(P+3+NBOO*8),WHRVAL, NEXPOS)
            CALL BLKMOV(BUFFER(P+3+NBOO*8+NEXPOS),WHRLEN, NEXPOT)
            PGPPTR = P + 3 + NBOO*8 + NEXPOS + NEXPOT
            GOTO 9000

         ELSE IF (TYP.EQ.'SRT') THEN
C     SRTCOM (SAVE WHOLE THING
            IF (.NOT.PGSTOR(SRTCM0,ZSRTCM)) GOTO 8000
            GOTO 9000

         ELSE IF (TYP.EQ.'EXP') THEN
C     EXPCOM
            IF (.NOT.PGSTOR(0,-(1+7*NEOP))) GOTO 8000
            CALL BLKMOV(BUFFER(P),NEOP,  1)
            CALL BLKMOV(BUFFER(P+1       ),EOP,   NEOP)
            CALL BLKMOV(BUFFER(P+1+NEOP*1),EXPTP, NEOP)
            CALL BLKMOV(BUFFER(P+1+NEOP*2),EXPLEN, NEOP)
            CALL BLKMOV(BUFFER(P+1+NEOP*3),EXPVTP, NEOP)
            CALL BLKMOV(BUFFER(P+1+NEOP*4),EXPVAL, NEOP)
            CALL BLKMOV(BUFFER(P+1+NEOP*5),EXPVRL, NEOP*2)
            PGPPTR = P + 1 + NEOP*7
            GOTO 9000

         ELSE
            RETURN
         ENDIF

 8000    RETURN
 9000    PGBSTO = .TRUE.
         RETURN
      END
