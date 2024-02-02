      INTEGER FUNCTION NXTCHR(TXTONL)

         USE Text, only : ABLANK
         USE Lexer, only : ASSEMI, ASPLUS

C
C --- TEXT PARSING ROUTINE
C
C     PARSE THE INPUT LINE RETRIEVING THE NEXT CHARACTER
C
C     PARAMETERS
C              TXTONL--TRUE IF PARSING QUOTED TEXT STRING
C
C     CHARACTER IS RETURNED AS THE FUNCTION VALUE (ASCII-CHAR)
C
C     IF MACWPT<MACWPZ THEN THE CHARACTER IS TAKEN FROM MACWRK
C
         INCLUDE 'syspar.inc'


         LOGICAL TXTONL
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'cards.inc'
         INCLUDE 'files.inc'
         INCLUDE 'maccom.inc'
C
C     CHECK FOR ACTIVE MACRO TEXT
C
         IF (MACWPT.LT.MACWPZ) THEN
            CALL GETT(MACWRK(1),MACWPT,NXT)
            MACWPT = MACWPT + 1
            IF (NXT.EQ.0) GOTO 810
            GOTO 400
         ENDIF
C
C     CHECK STATUS OF CARD RECORD
C
  100    IF (READCD.EQ.0) THEN
            CALL NXTCRD(INEOF)
            IF (INEOF.NE.0) GOTO 800
            READCD = 1
         ENDIF
         IF (READCD.EQ.2) GOTO 800
C
         NXT = 0
  200    CONTINUE
         CRDPTR = CRDPTR + 1
         IF (CRDPTR.GT.CRDEND) THEN
            IF (ASPLUS.EQ.NULL) THEN
               READCD = 0
               NXT = ABLANK
               GOTO 600
            ELSE
               GOTO 800
            ENDIF
         ENDIF

         CALL GETT(CRDREC,CRDPTR,NXT)
C
C     PLUS AT END OF LINE IS CONTINUATION UNLESS NOT READING
C
  300    IF (NXT.EQ.ASPLUS .AND. CRDPTR.EQ.CRDEND) THEN
            IF (READCD.GE.0) THEN
               READCD = 0
               GOTO 100
            ENDIF
            IF (READCD.LT.0) GOTO 800
         ENDIF
C
C     SEMICOLON IS LOGICAL END-OF-CARD UNLESS QUOTED
C
  400    IF (.NOT.TXTONL .AND. NXT.EQ.ASSEMI) GOTO 810
C
C     RETURN THE CHARACTER
C
  600    NXTCHR = NXT
         LASTCH = NXT
         LXEOC = 0
         GOTO 900
C
C     END-OF-CARD REACHED
C
  800    IF (READCD.GT.0) READCD = 0
  810    NXTCHR = 0
         LASTCH = 0
         LXEOC = 1
C
  900    CONTINUE
         RETURN
      END
