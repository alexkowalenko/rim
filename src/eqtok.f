      LOGICAL FUNCTION EQTOK(I,ASCHR)
      INCLUDE 'syspar.inc'
C
C     THIS FUNCTION COMPARES ASCHR WITH ITEM I OF THE
C     COMMAND TOKEN LIST
C
C     INPUT - I........ITEM NUMBER
C             ASCHR....ASCII-CHAR
C     OUTPUT- EQTOK ....TRUE. IFF
C                   A. ITEM I IS TEXT OF LENGTH 1
C                   B. ITEM I EQUALS ASCHR
C
      INCLUDE 'tokens.inc'
C
      EQTOK = .FALSE.
      IF (IDL(I).NE.1) RETURN
      CALL GETT(ASCREC(IDP(I)),1,A)
      IF (A.NE.ASCHR) RETURN
      EQTOK = .TRUE.
      RETURN
      END
