      SUBROUTINE PJECT(*)

         USE Globals, only : DFLAG, DMFLAG
         USE DateTime, only : RMDATE
         Use Lexer, only: KXNAME, TOKTYP
         USE Message, only : WARN
         USE Text, only : BLANK
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     PERFORM PHYSICAL PROJECTIONS ON EXISTING RELATIONS.
C
C     PROJECT RNAME2 FROM RNAME1 USING ATTR1 ATTR2...ATTRN
C       ... WHERE CONDITION
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'rimcom.inc'
C
C
         INTEGER STATUS
         LOGICAL EQKEYW
         INTEGER ATNCOL
         LOGICAL PJALL
         INTEGER :: TEMP(Z)
         INCLUDE 'dclar1.inc'
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C     MAKE SURE THE DATABASE MAY BE MODIFIED
C
         IF(.NOT.DMFLAG) THEN
            CALL WARN(8)
            GO TO 999
         ENDIF
C
C     CHECK SYNTAX AND RELATION NAMES
C
         CALL BLKCLN
         IF(.NOT.EQKEYW(3,'FROM')) THEN
            CALL WARN(4,BLANK,BLANK)
            GOTO 999
         ENDIF
         UC = LFIND(5,ITEMS-4,'USING')
         CALL LXSREC(4,RNAME1,ZC)
         I = LOCREL(RNAME1)
         LENF = NCOL
         IF(I.NE.0) THEN
C       RNAME1 DOES NOT EXIST
            CALL WARN(1,RNAME1)
            GO TO 999
         ENDIF
C
C
         IF(.NOT.TOKTYP(2,KXNAME)) THEN
            !CALL WARN(7,ASCTXT(IDP(2)),0) ! ASCTXT is a subroutine ???
            TEMP(1) = KXNAME
            CALL WARN(7, TEMP)
            GO TO 999
         ENDIF
         CALL LXSREC(2,RNAME2,ZC)
         I = LOCREL(RNAME2)
         IF(I.EQ.0) THEN
C       DUPLICATE RELATION NAME ENCOUNTERED
            CALL WARN(5,RNAME2)
            GO TO 999
         ENDIF
C
C  CHECK USER READ SECURITY
C
         I = LOCREL(RNAME1)
         I = LOCPRM(RNAME1,1)
         IF(I.NE.0) THEN
            CALL WARN(9,RNAME1)
            GO TO 999
         ENDIF
         NS = 0
         NID = RSTART
C
C  SET UP THE WHERE CLAUSE
C
         K = LFIND(5,ITEMS-4,'WHERE')
         NBOO = 0
         LIMTU = ALL9S
         RMSTAT = 0
         KKX = K
         IF(K.NE.0) CALL SELWHR(K,ITEMS-K+1)
         IF(RMSTAT.NE.0) GO TO 999
C
C  CHECK THE ATTRIBUTES AND BUILD POINTER ARRAY - POS. 7
C
         CALL BLKDEF(7,LENF,1)
         KQ7 = BLKLOC(7) - 1
         NOCOLS = 0
         UE = ITEMS
         IF(K.NE.0) UE = K - 1
         IF (UC.EQ.0) THEN
C
C        ALL ATTRIBUTES
C
            PJALL = .TRUE.
            NOATTS = NATT
         ELSE
C
C        SELECTED ATTRIBUTES - CHECK THEM
C
            IERR = 0
            DO 140 I=UC+1,UE
               CALL LXSREC(I,ANAME,ZC)
               IF(LOCATT(ANAME,NAME).NE.0) THEN
                  CALL WARN(3,ANAME,NAME)
                  IERR = 1
               ENDIF
  140       IF(IERR.EQ.1) GO TO 999
            PJALL = .FALSE.
            NOATTS = UE - UC
         ENDIF
         CALL ATTNEW(RNAME2,NOATTS)
C
         DO 160 I=1,NOATTS
            IF(PJALL) THEN
               STATUS = LOCATT(BLANK,NAME)
               DO 148 J=1,I
                  CALL ATTGET(STATUS)
  148          IF(STATUS.NE.0) GO TO 160
            ELSE
               CALL LXSREC(I+UC,ANAME,ZC)
               IERR = LOCATT(ANAME,NAME)
               CALL ATTGET(STATUS)
            ENDIF
C
            ATNCOL = NOCOLS + 1
            IF(ATTWDS.GT.0) THEN
C
C        FIXED LENGTH
C
               KQ = KQ7 + ATTCOL
               DO 150 KK=1,ATTWDS
                  NOCOLS = NOCOLS + 1
                  BUFFER(KQ) = NOCOLS
  150          KQ = KQ + 1
            ELSE
C
C        VARIABLE LENGTH
C
               NOCOLS = NOCOLS + 1
               BUFFER(KQ7+ATTCOL) = -NOCOLS
            ENDIF
            CALL ZMOVE(RELNAM,RNAME2)
            ATTCOL = ATNCOL
            ATTKEY = 0
            CALL ATTADD
  160    CONTINUE
C
C  SET UP RELTBLE
C
         CALL ZMOVE(NAME,RNAME2)
         RDATE = RMDATE()
         NCOL = NOCOLS
         NATT = NOATTS
         NTUPLE = 0
         RSTART = 0
         REND = 0
         CALL RELADD
C
C     1 IS INPUT BUFFER, 2 IS OUTPUT BUFFER, 8 IS OUTPUT TUPLE
C
         LPAG = MAXCOL + 2
         CALL BLKDEF(8,LPAG,1)
         KQ8 = BLKLOC(8)
C
C     LOOP THRU THOSE TUPLES
C
         RMSTAT = 0
         I = LOCREL(RNAME1)
         KNEW = 0
         MSTART = 0
         MEND = 0
C
  170    CALL RMLOOK(IPOINT,1,1,LENGTH)
         IF (RMSTAT.EQ.0) THEN
            CALL PRJTUP(BUFFER(KQ7+1),LENF,NOCOLS,BUFFER(IPOINT),
     X                  BUFFER(KQ8),LENT)
            CALL ADDDAT(2,MEND,BUFFER(KQ8),LENT)
            IF(MSTART.EQ.0)MSTART = MEND
            KNEW = KNEW + 1
            GO TO 170
         ENDIF
C
         I = LOCREL(RNAME2)
         CALL RELGET(STATUS)
         NTUPLE = KNEW
         RSTART = MSTART
         REND = MEND
         CALL RELPUT
         CALL MSG(' ','PROJECT COMPLETED,','+')
         CALL IMSG(KNEW,6,'+')
         CALL MSG(' ',' ROWS GENERATED.',' ')
C
C
  999    CALL BLKCLR(7)
         CALL BLKCLR(8)
         RETURN 1
      END
