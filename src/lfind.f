      INTEGER FUNCTION LFIND(ITEM1,NUM,KEY)

         USE Lexer, only: ITEMS, EQKEYW

         INCLUDE 'syspar.inc'
C
C     THIS ROUTINE LOOKS FOR A KEYWORD IN THE COMMAND TOKEN
C     RECORD.  IT RETURNS 0 IF NOT FOUND AND THE ITEM
C     NUMBER IF FOUND.
C
C     ITEM1---FIRST TOKEN TO CHECK
C     NUM-----NUMBER OF TOKENS TO CHECK
C     KEY ----KEYWORD TO LOOK FOR
C
C
         INCLUDE 'tokens.inc'

         CHARACTER*(*) KEY
         NEND = ITEM1 + NUM - 1
         IF (NEND.GT.ITEMS) NEND = ITEMS
         DO 10 J=ITEM1,NEND
            IF(EQKEYW(J,KEY)) GO TO 20
   10    CONTINUE
         J = 0
   20    CONTINUE
         LFIND = J
         RETURN
      END
