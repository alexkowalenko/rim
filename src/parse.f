      FUNCTION PARSE(KEYLST,KEYLEN,KEYPTR)

         USE Lexer, only : ITEMS, LFIND
         INCLUDE 'syspar.inc'
C
C  SUBROUTINE TO PARSE A FREE-FIELD FORMAT INPUT RECORD.
C  LOOKING FOR KEYWORDS
C         PARSE RETURNS THE POSITION OF THE FIRST KEYWORD
C
C  RM_Parameters
C         KEYLST--LIST OF KEYWORDS
C         KEYLEN--LENGTH OF KEYLST
C         KEYPTR--ARRAY OF POINTERS TO KEYWORD PHRASES
C            (1,X) = INDEX OF KEYWORD IN /TOKENS/
C            (2,X) = LENGTH IN TOKENS OF PHRASE
C
         CHARACTER*(ZKEYWL) KEYLST(1)
         INTEGER KEYPTR(2,1)
C
         INCLUDE 'rmatts.inc'
C
C
C     FIND THE TOKENS
C
         DO 200 I = 1, KEYLEN
            KEYPTR(1,I) = 0
            KEYPTR(2,I) = 0
            IF (KEYLST(I).EQ.' ') GOTO 200
            KEYPTR(1,I) = LFIND(1,ITEMS,KEYLST(I))
  200    CONTINUE
C
C     LOCATE THE END OF CLAUSES
C
C
         PARSE = ITEMS + 1
         DO 250 I = 1, KEYLEN
            IF (KEYPTR(1,I).EQ.0) GOTO 250
            IF (KEYPTR(1,I).LT.PARSE) PARSE = KEYPTR(1,I)
            KE = ITEMS
            DO 240 J = 1, KEYLEN
               IF (KEYPTR(1,J).GT.KEYPTR(1,I) .AND. KEYPTR(1,J).LT.KE)
     X            KE = KEYPTR(1,J) - 1
  240       CONTINUE
            KEYPTR(2,I) = KE - KEYPTR(1,I) + 1
  250    CONTINUE
C
         RETURN
      END
