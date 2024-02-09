      SUBROUTINE LXEDIT

         USE Cards, only : CRDREC, CRDEND, CRDRLB, CRDRLL, CRDIDX
         USE Text, only : UPCASE, ABLANK
         USE TextFiles, only : TIOIN
         USE Lexer, only : ASRCLL, ASLXEI, ASLXEE, ASLXEX, ASLXES

         INCLUDE 'syspar.inc'
C
C     RECALL AND EDIT AN INPUT LINE OR COMMAND
C
         INCLUDE 'files.inc'
         INCLUDE 'prom.inc'
         INCLUDE 'msgcom.inc'
C
         INTEGER EDTREC(ZCARDW)
         INTEGER CPYREC(ZCARDW)
         LOGICAL INSMOD
C
C     RECALL AN INPUT RECORD
C
         CP = CRDIDX
         CPCNT = 0
  100    IF (CP.LT.1) CP = ZCARDN
         CPCNT = CPCNT + 1
         DO 110 I = 1, ZCARDW
  110    CPYREC(I) = CRDRLB(I,CP)
         CPYEND = CRDRLL(CP)
         IF (CPYEND.EQ.0 .OR. CPCNT.GT.ZCARDN) THEN
            CRDEND = 0
            GOTO 900
         ENDIF
C
C     ECHO RECALLED LINE AND GET EDITS
C
  150    CPYP = 1
         CRDP = 1
         CALL AMSG(CPYREC,CPYEND,' ')
         CALL TIOIN(NINT,EDTREC,EDTEND,EOF)
         IF (EOF.NE.0) GOTO 300
         IF(EDTEND.EQ.0) GO TO 300
C
C     IS IT ANOTHER RECALL COMMAND
C
         IF (EDTEND.EQ.1) THEN
            CALL GETT(EDTREC,1,ECH)
            IF (UPCASE(ECH).EQ.ASRCLL) THEN
               CP = CP - 1
               GOTO 100
            ENDIF
         ENDIF
C
C     APPLY EDITS TO CPYREC
C
         INSMOD = .FALSE.
         DO 200 I = 1, EDTEND
            CALL GETT(EDTREC,I,ECH)
C
C     CHECK FOR '<' (BEGIN INSERT)
  210       IF (ECH.EQ.ASLXEI .AND. .NOT. INSMOD) THEN
               INSMOD = .TRUE.
               GOTO 200
            ENDIF
C
C     CHECK FOR '>' (END INSERT)
            IF (ECH.EQ.ASLXEE .AND. INSMOD) THEN
               INSMOD = .FALSE.
               GOTO 200
            ENDIF
C
C     CHECK FOR '!' (END LINE)
            IF (ECH.EQ.ASLXEX .AND. .NOT. INSMOD) GOTO 320
C
C     CHECK FOR '#' (SKIP CHARACTER)
            IF (ECH.EQ.ASLXES .AND. .NOT. INSMOD) THEN
               CPYP = CPYP + 1
               GOTO 200
            ENDIF
C
C     CHECK FOR ' ' (KEEP CHARACTER)
            IF (ECH.EQ.ABLANK .AND. .NOT. INSMOD .AND.
     1          CPYP.LE.CPYEND) CALL GETT(CPYREC,CPYP,ECH)
C
C     MOVE CHARACTER
  190       CALL PUTT(CRDREC,CRDP,ECH)
            IF (.NOT. INSMOD) CPYP = CPYP + 1
            CRDP = CRDP + 1
            IF (CRDP.GT.ZCARDL) GOTO 320
  200    CONTINUE
C
C     COPY REST OF OLD LINE
C
  300    IF (CPYP.LE.CPYEND) THEN
            DO 310 I = CPYP, CPYEND
               CALL GETT(CPYREC,I,ECH)
               CALL PUTT(CRDREC,CRDP,ECH)
               CRDP = CRDP + 1
               IF (CRDP.GT.ZCARDL) GOTO 320
  310       CONTINUE
         ENDIF
  320    CRDEND = CRDP -1
         INSMOD = .FALSE.
         IF (EOF.NE.0 .OR. EDTEND.EQ.0) GOTO 900
C
C     MORE EDITS
C
         DO 400 I = 1, ZCARDW
  400    CPYREC(I) = CRDREC(I)
         CPYEND = CRDEND
         GOTO 150
C
C     DONE WITH EDITS
C
  900    RETURN
      END
