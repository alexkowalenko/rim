      SUBROUTINE NXTCRD(EOF)

         USE Cards, only: CRDREC, CRDPTR, CRDEND, CRDRLB, CRDRLL, CRDIDX
         USE Globals, only : TRACE
         USE Text, only : UPCASE
         USE TextFiles, only : TIOIN
         USE Lexer, only : ASRCLL

         INCLUDE 'syspar.inc'
C
C  ROUTINE TO READ A RECORD TO /CARDS/
C
C  PARAMETERS
C
C         EOF-----END-OF-FILE FLAG (0=NO, 1=YES)
C
         INCLUDE 'files.inc'
         INCLUDE 'prom.inc'
         INCLUDE 'msgcom.inc'
C
C
C  READ A LINE FROM THE INPUT FILE
C
  100    CONTINUE
         IF(PRMPT) CALL PROMPT(PROM)
         CALL TIOIN(NINT,CRDREC,CRDEND,EOF)
  101    IF (EOF.NE.0) GOTO 900
C
C  IF THERE IS NO DATA ON THE CARD READ ANOTHER ONE.
C
         IF(CRDEND.EQ.0) GO TO 100
         CRDPTR = 0
         EOF = 0
C
C     LOOK FOR RECALL COMMAND
C
  150    IF (CONNI) THEN
            IF (CRDEND.LE.2) THEN
               CALL GETT(CRDREC,1,CH)
               IF (UPCASE(CH).EQ.ASRCLL) CALL LXEDIT
               IF (CRDEND.EQ.0) GOTO 100
            ENDIF
C
C     SAVE INPUT IN RECALL BUFFER
C
            CRDIDX = CRDIDX + 1
            IF (CRDIDX.GT.ZCARDN) CRDIDX = 1
            DO 200 I = 1, ZCARDW
  200       CRDRLB(I,CRDIDX) = CRDREC(I)
            CRDRLL(CRDIDX) = CRDEND
         ENDIF
C
C     POSSIBLY TRACE INPUT
C
         IF (TRACE.GT.0) THEN
            CALL MSG('T','INPUT RECORD',' ')
            DO 300 I = 1, CRDEND
               MSGPTR = MSGPTR + 1
               CALL GETT(CRDREC,I,CH)
               CALL PUTT(MSGREC,MSGPTR,CH)
  300       CONTINUE
            MSUNIT = NOUTT
            CALL AMSG(0,0,' ')
         ENDIF
         RETURN
C
C     AT EOF RESET INPUT TO TERMINAL
C
  900    CONTINUE
C--C  IF (NINT.NE.ZNINT) CALL MSG(' ','END OF INPUT FILE',' ')
C---- CALL SETIN(ZTRMIN)
         RETURN
      END
