      FUNCTION LXLENW(I)

         USE Lexer, only: KXTEXT

         INCLUDE 'syspar.inc'
C
C  RETURN THE LENGTH IN WORDS FOR THE ITH ITEM.
C
         INCLUDE 'tokens.inc'
         LXLENW = 1
         IF (TOKTYP(I,KXTEXT)) LXLENW = (IDL(I) - 1) / ZCW + 1
         RETURN
      END
