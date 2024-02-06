      LOGICAL FUNCTION LOADMV(CCOL,CLEN)

         USE Globals, only : KMSSVL, KMSSVT, KNAPVL, KNAPVT
         USE Lexer, only: KXTEXT, KXINT, KXREAL, IDT, ITEMS, IDI
         USE Text, only : BLANK, ABLANK, ASCAN, STRMOV
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     CHECK FORMATTED LOADING DATA FOR MISSING VALUES
C
C  PARAMETERS:
C         CCOL----STARTING COLUMN
C         CLEN----LENGTH
C
         INCLUDE 'cards.inc'
C
C  DECLARATION STATEMENTS
C
         LOGICAL EQ
         INTEGER NULLBF(Z)

         LOADMV = .FALSE.
C
C     -- CHECK FOR NULL VALUE FLAGS
C
         LNB = ASCAN(CRDREC,CCOL,0-CLEN,ABLANK,.FALSE.)
         IF (LNB.GT.0) LNB = LNB - CCOL + 1
         IF (LNB.GT.ZC) GOTO 500
         CALL ZMOVE(NULLBF,BLANK)
         CALL STRMOV(CRDREC,CCOL,LNB,NULLBF,1)

         IF (LNB.EQ.0 .OR. LNB.EQ.KMSSVL) THEN
            IF (EQ(NULLBF,KMSSVT)) THEN
               IDI(ITEMS) = ZIMISS
               IDT(ITEMS) = KXTEXT + KXINT + KXREAL
               LOADMV = .TRUE.
               GOTO 500
            ENDIF
         ENDIF
         IF (LNB.EQ.0 .OR. LNB.EQ.KNAPVL) THEN
            IF (EQ(NULLBF,KNAPVT)) THEN
               IDI(ITEMS) = ZINAPP
               IDT(ITEMS) = KXTEXT + KXINT + KXREAL
               LOADMV = .TRUE.
            ENDIF
         ENDIF
C
  500    CONTINUE
         RETURN
      END
