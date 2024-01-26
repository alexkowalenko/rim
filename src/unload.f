      SUBROUTINE UNLOAD(*)

      USE DateTime, only: RMTIME, RMDATE
      USE Utils, only : ZMOVE
      
      INCLUDE 'syspar.inc'
C
C     UNLOAD DATABASE SCHEMA, DATA, OR BOTH
C
      INCLUDE 'ascpar.inc'
      INCLUDE 'tokens.inc'
      INCLUDE 'rmatts.inc'
      INCLUDE 'files.inc'
      INCLUDE 'buffer.inc'
      INCLUDE 'flags.inc'
      INCLUDE 'rimcom.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'dclar1.inc'
      INTEGER IREL(Z,1)
      CHARACTER*3 MODE
      CHARACTER*(ZFNAML) FN
      EQUIVALENCE (BUFFER(1),IREL(1,1))
      LOGICAL ALL,PERM,LHASH, EQ, NE, EQKEYW
      DATA NWORDS /2500/
C
C
      TP = 0
      NN = 0
 
C     SET FOR NO FORMS CONTROL
      SAVLPP = ULPP
      ULPP = 0
C
C     CHECK FOR A DATABASE
C
      IF (.NOT.DFLAG) THEN
         CALL WARN(2,0,0)
         GOTO 999
      ENDIF
C
C  INITIALIZE
C
      CALL BLKCLN
      CALL BLKDEF(4,100,1)
      RMSTAT = 0
      LHASH = .FALSE.
      NOGO = 0
      ICNTR = 0
      IPERM = 0
      ALL = .TRUE.
      MODE = 'ALL'
      PTR = 2
C
C     SEE IF AN OUTPUT FILE HAS BEEN SPECIFIED
C
      I = LFIND(1,ITEMS,'TO')
      IF (I.NE.0) THEN
         IF (I.EQ.ITEMS) GOTO 900
         CALL STRASC(FN,ASCREC(IDP(I+1)),IDL(I+1))
         IF (KWS(I+1).EQ.'TERMINAL') FN = ZTRMOU
         CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
         IF (STAT.NE.0) GOTO 999
         TP = I
      ENDIF
C
      IF (TP.GT.0 .AND. TP.LT.ITEMS) ITEMS = TP - 1
 
C
C  CHECK TO SEE IF ALL DEFAULTS        (UNLOAD)
C
      IF (ITEMS .EQ. 1) GO TO 25
C
C  CHECK FOR TYPE OF UNLOAD      (UNLOAD ... ALL/DATA/SCHEMA )
C
      IF (ITEMS .LT. PTR) GO TO 25
      IF ( EQKEYW(PTR,'ALL') .OR. EQKEYW(PTR,'DEFINITIONS') .OR.
     X     EQKEYW(PTR,'DATA') .OR. EQKEYW(PTR,'PASSWORDS') ) THEN
         MODE = KWS(PTR)
         PTR = PTR + 1
      ENDIF
C
C  UNLOAD PASSWORDS REQUIRES OWNER PRIV
C
      IF (MODE.EQ.'PAS' .AND. NE(OWNER,USERID)) THEN
         CALL MSG('E',
     X  'YOU ARE NOT PERMITTED TO UNLOAD THE PASSWORDS',' ')
         RMSTAT = 9
         GOTO 999
      ENDIF
C
C  CHECK FOR HASH  (( THIS OPTION IS NOT USED ))
C
20    IF (KWS(PTR).EQ.'=') THEN
         IF (KWS(PTR+1).NE.'HASH') GO TO 900
         LHASH = .TRUE.
         PTR = PTR + 2
      ENDIF
C
25    IF (ITEMS .LT. PTR) THEN
C       THE COMMAND IS ALL SO SET ICNTR TO MAX
        ICNTR = ALL9S
        GO TO 400
      ENDIF
C
C  THE USER HAS SPECIFIED WHICH RELATIONS HE WANTS DUMPED
C
      J = PTR
      ALL = .FALSE.
210   CALL LXSREC(J,RNAME,ZC)
      IERR = 0
      IN = LOCREL (RNAME)
      IF (IN .NE. 0) THEN
         CALL WARN(1,RNAME,0)
         RMSTAT = 2
         IERR = 1
      ENDIF
C
C  CALL CHKREL TO CHECK PASSWORD PERMISSION ON THE UNLOAD
C
      CALL CHKREL (PERM,MODE,ISTAT,USERID)
      IF (.NOT.PERM) THEN
         CALL WARN(9,RNAME,0)
         RMSTAT = 9
         IERR = 1
         GO TO 350
      ENDIF
C
C  CHECK TO MAKE SURE THERE IS ONLY ONE OF THE RELATIONS LISTED
C
      IF (ICNTR .EQ. 0 ) GO TO 335
      DO 310 KK = 1,ICNTR
      IF (EQ(IREL(1,ICNTR),RNAME)) THEN
         CALL MSG('E','YOU HAVE ALREADY SPECIFIED TABLE ''','+')
         CALL AMSG(RNAME,-ZC,'+')
         CALL MSG(' ','''.',' ')
         GO TO 350
      ENDIF
  310 CONTINUE
C
C  EVERYTHING IS CORRECT -- SAVE CERTAIN DATA IN IREL(ICNTR)
C
335   ICNTR = ICNTR + 1
      CALL ZMOVE(IREL(1,ICNTR),NAME)
350   J = J + 1
      IF (IERR .EQ. 1) NOGO = 1
      IF ( J .LE. ITEMS) GO TO 210
C
C  DONE WITH PERMISSION AND CRACKING
C
  400 CONTINUE
      IF (NOGO .EQ. 1) GO TO 999
      CALL MSG('R','*(UNLOAD OF ','+')
      CALL AMSG(DBNAME,-ZC,'+')
      CALL MSG(' ',' AT ','+')
      I = RMDATE()
      CALL DMSG(I,0,'+',KZDATE)
      CALL MSG(' ','  ','+')
      I = RMTIME()
      CALL DMSG(I,0,'+',KZTIME)
      CALL MSG(' ',')',' ')
C
      CALL MSG('R','*(SET CON=+)',' ')
      CALL MSG('R','*(SET END=NULL)',' ')
C--   IF (LHASH) THEN
C--      CALL RMTIME (ITIM)
C--      NUM = MOD(ITIM,7)
C--      CALL MSG('R','RIM COMMUNICATION FILE ??',' ')
C--   ENDIF
C
C  IF DIRECTIVE ALL OR SCHEMA CALL UNDEF
C
      CALL BLKDEF(5,2500,1)
      KQ2 = BLKLOC(5)
      IF ((MODE.EQ.'DEF') .OR. (MODE.EQ.'ALL'))
     +  CALL UNDEF(ALL,ICNTR,NUM,MODE,LHASH,BUFFER(KQ2))
      IF (ICNTR .EQ. 0) GO TO 800
      CALL BLKCHG(5,250,6)
      KQ2 = BLKLOC(5)
C
C  IF DIRECTIVE ALL OR DATA CALL UNDATA
C
      IF ((MODE.EQ.'DAT') .OR. (MODE.EQ.'ALL'))
     +  CALL UNDATA(ALL,ICNTR,NUM,MODE,LHASH,BUFFER(KQ2))
      IF (ICNTR .EQ. 0) GO TO 800
C
C  IF DIRECTIVE PASSWORDS CALL UNPASS
C
      IF (MODE.EQ.'PAS')
     +  CALL UNPASS(ALL,ICNTR,NUM,MODE,LHASH,BUFFER(KQ2))
      IF (ICNTR .EQ. 0) GO TO 800
C
      CALL MSG('R','*(SET END=;)',' ')
      CALL MSG('R','EXIT',' ')
      GO TO 999
C
C  ERROR FOR UNLOADING ALL OF THE DATA
C
800   CALL WARN(8,0,0)
      RMSTAT = 9
      GO TO 999
C
C  INCORRECT SYNTAX ERROR MESSAGE
C
900   CALL WARN(4,0,0)
      RMSTAT = 4
C
C  CLEAN UP AND END
C
999   CALL BLKCLN
      ULPP = SAVLPP
      IF (TP.NE.0) THEN
         CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
         IF (RMSTAT.EQ.0) CALL MSG(' ',' UNLOAD COMPLETED',' ')
      ENDIF
C
      RETURN 1
      END
