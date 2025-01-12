      SUBROUTINE PGBRES(TYP,P)

         USE RM_BufferData, only: BUFFER

         INCLUDE 'syspar.inc'
C
C     RESTORE A COMMON BLOCK
C
C     INPUT:   TYP-----CODE FOR BLOCK TO RESTORE
C                    SEL = SELCOM
C                    WH  = WHCOM
C                    SRT = SRTCOM
C                    EXP = EXPCOM
C              P ------LOCATION OF SAVED DATA IN BUFFER
C
C     OUT:     P ------LOCATION OF NEXT BUFFER POSITION
C
         CHARACTER*3 TYP

         INCLUDE 'selcom.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'ptrcom.inc'
         INCLUDE 'srtcom.inc'
         INCLUDE 'expcom.inc'
         INCLUDE 'rimptr.inc'
C

         IF (TYP.EQ.'SEL') THEN
C     SELCOM
            CALL BLKMOV(NUMATT,BUFFER(P),1)
            CALL BLKMOV(LIN1  ,BUFFER(P+1          ),NUMATT)
            CALL BLKMOV(COL1  ,BUFFER(P+1+NUMATT*1 ),NUMATT)
            CALL BLKMOV(ITEMW ,BUFFER(P+1+NUMATT*2 ),NUMATT)
            CALL BLKMOV(CURPOS,BUFFER(P+1+NUMATT*3 ),NUMATT)
            CALL BLKMOV(NUMCOL,BUFFER(P+1+NUMATT*4 ),NUMATT)
            CALL BLKMOV(FORMT ,BUFFER(P+1+NUMATT*5 ),NUMATT)
            CALL BLKMOV(ATYPE ,BUFFER(P+1+NUMATT*6 ),NUMATT)
            CALL BLKMOV(LEN   ,BUFFER(P+1+NUMATT*7 ),NUMATT)
            CALL BLKMOV(ROWD  ,BUFFER(P+1+NUMATT*8 ),NUMATT)
            CALL BLKMOV(COLD  ,BUFFER(P+1+NUMATT*9 ),NUMATT)
            CALL BLKMOV(FP    ,BUFFER(P+1+NUMATT*10),NUMATT)
            CALL BLKMOV(SINGLE,BUFFER(P+1+NUMATT*11),NUMATT)
            CALL BLKMOV(VAR   ,BUFFER(P+1+NUMATT*12),NUMATT)
            P = P + 1 + NUMATT*13
            GOTO 9000

         ELSE IF (TYP.EQ.'WH ') THEN
C     WHCOM
            CALL BLKMOV(NBOO  ,BUFFER(P),1)
            CALL BLKMOV(NEXPOS,BUFFER(P+1),1)
            CALL BLKMOV(NEXPOT,BUFFER(P+2),1)
            CALL BLKMOV(BOO   ,BUFFER(P+3       ),NBOO)
            CALL BLKMOV(KATTP ,BUFFER(P+3+NBOO*1),NBOO)
            CALL BLKMOV(KATTL ,BUFFER(P+3+NBOO*2),NBOO)
            CALL BLKMOV(KATTY ,BUFFER(P+3+NBOO*3),NBOO)
            CALL BLKMOV(KOMTYP,BUFFER(P+3+NBOO*4),NBOO)
            CALL BLKMOV(KOMPOS,BUFFER(P+3+NBOO*5),NBOO)
            CALL BLKMOV(KOMLEN,BUFFER(P+3+NBOO*6),NBOO)
            CALL BLKMOV(KOMPOT,BUFFER(P+3+NBOO*7),NBOO)
            CALL BLKMOV(WHRVAL,BUFFER(P+3+NBOO*8),NEXPOS)
            CALL BLKMOV(WHRLEN,BUFFER(P+3+NBOO*8+NEXPOS),NEXPOT)
            P = P + 3 + NBOO*8 + NEXPOS + NEXPOT
            GOTO 9000

         ELSE IF (TYP.EQ.'SRT') THEN
C     SRTCOM (SAVE WHOLE THING
            CALL BLKMOV(SRTCM0,BUFFER(P),ZSRTCM)
            P = P + ZSRTCM
            GOTO 9000

         ELSE IF (TYP.EQ.'EXP') THEN
C     EXPCOM
            CALL BLKMOV(NEOP  ,BUFFER(P),1)
            CALL BLKMOV(EOP   ,BUFFER(P+1       ),NEOP)
            CALL BLKMOV(EXPTP ,BUFFER(P+1+NEOP*1),NEOP)
            CALL BLKMOV(EXPLEN,BUFFER(P+1+NEOP*2),NEOP)
            CALL BLKMOV(EXPVTP,BUFFER(P+1+NEOP*3),NEOP)
            CALL BLKMOV(EXPVAL,BUFFER(P+1+NEOP*4),NEOP)
            CALL BLKMOV(EXPVRL,BUFFER(P+1+NEOP*5),NEOP*2)
            P = P + 1 + NEOP*7
            GOTO 9000

         ELSE
            RETURN
         ENDIF
C
 9000    RETURN
      END
