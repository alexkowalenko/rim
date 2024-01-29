      SUBROUTINE LXSREC(I,STRING,NUMC)

         USE Text, only : BLANK, ABLANK
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C
C  MOVE NUMC ASCII-CHARS FROM THE ITH ITEM.                   .
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INTEGER STRING(1)
C
C  BLANK FILL FIRST.
C
         IF (NUMC.EQ.ZC) THEN
            CALL ZMOVE(STRING,BLANK)
         ELSE
            DO 100 J=1,NUMC
  100       CALL PUTT(STRING,J,ABLANK)
         ENDIF
C
C  MOVE THE TEXT.
C
         IF(TOKTYP(I,KXTEXT)) THEN
            K = IDP(I)
            NUM = IDL(I)
            IF(NUM.LE.0) RETURN
            IF(NUMC.LT.NUM) NUM = NUMC
            CALL STRMOV(ASCREC(K),1,NUM,STRING,1)
         ENDIF
         RETURN
      END
