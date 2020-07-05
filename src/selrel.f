      FUNCTION SELREL(FTOK,NTOK)
      INCLUDE 'syspar.d'
C
C  FUNCTION TO VALIDATE AND PROCESS A FROM OR USE CLAUSE
C
C  PARAMETERS
C         FTOK--- LOCATION OF 'FROM' TOKEN
C         NTOK--- NUMBER OF TOKENS IN THE FROM CLAUSE
C                 NTOK = 0 USES PRIOR VALUE
C
      LOGICAL SELREL
C
      INCLUDE 'ascpar.d'
      INCLUDE 'selcom.d'
      INCLUDE 'tokens.d'
      INCLUDE 'tupler.d'
      INCLUDE 'dclar1.d'
 
      LOGICAL NE, EQ
C
C     -----------------------------
C
      SELREL = .FALSE.
      CALL ZMOVE(RNAME,BLANK)
      IF (NTOK.EQ.0) THEN
         CALL ZMOVE(RNAME,NAME)
      ELSE IF (NTOK.EQ.2) THEN
         CALL LXSREC(FTOK+1,RNAME,ZC)
      ENDIF
      IF (EQ(RNAME,BLANK)) THEN
         CALL WARN(4,0,0)
         GOTO 900
      ENDIF
 
C
C  FIND THE RELATION NAME IN RELTBLE.
C
      I = LOCREL(RNAME)
      IF(I.NE.0) THEN
C
C        UNRECOGNIZED RELATION NAME.
C
         CALL WARN(1,RNAME,0)
         GO TO 900
      ENDIF
C
C  CHECK FOR READ PERMISSION.
C
      L = LOCPRM(NAME,1)
      IF(L.NE.0) THEN
         CALL WARN(9,NAME,0)
         GO TO 900
      ENDIF
C
C  SEE IF ANY TUPLES EXIST.
C
      IF(NTUPLE.LE.0) THEN
         CALL MSG('W','TABLE ''','+')
         CALL AMSG(NAME,-ZC,'+')
         CALL MSG(' ',''' CONTAINS NO DATA.',' ')
      ENDIF
C
C  RELATION OK
C
      SELREL = .TRUE.
C
900   RETURN
      END
