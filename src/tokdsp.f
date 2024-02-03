      SUBROUTINE TOKDSP

         USE, intrinsic :: iso_fortran_env

         USE Parameters
         USE Lexer, only : IDT, ASCREC, IDP, IDL, KWS
         USE Maths, only : IEXP
         USE Text, only : FILCH, ABLANK, RTOA

         implicit none

C
C     DISPLAY /TOKENS/ COMMON
C
         INCLUDE 'tokens.inc'
         INTEGER :: RTXT(ZPRINW), I, RF, N, ERR
C
         CALL MSG(' ',' /TOKENS/ ',' ')
         DO 100 I = 1, ITEMS
            CALL MSG(' ','ITEM: ','+')
            CALL IMSG(I,3,'+')
            CALL MSG(' ',' TYPE=','+')
            CALL IMSG(IDT(I),4,'+')
            CALL MSG(' ',' LENGTH=','+')
            CALL IMSG(IDL(I),4,'+')
            CALL MSG(' ',' KWS=','+')
            CALL MSG(' U',' :' // KWS(I) // ':',' ')
            CALL MSG(' ',' INT=','+')
            CALL IMSG(IDI(I),10,' ')
            IF (IDR(I).EQ.0) THEN
               RF = 103
            ELSE
               N = IEXP(IDR(I))
               IF (IDR(I).LT.0) N = N + 1
               RF = (IDL(I)-N)*100  + IDL(I)+6
            ENDIF
            CALL MSG(' ',' REAL(','+')
            CALL IMSG(RF,4,'+')
            CALL MSG(' ',')=','+')
            CALL FILCH(RTXT,1,ZPRINL,ABLANK)
            CALL RTOA(RTXT,1, RF, IDR(I), ERR)
            CALL RTOA(RTXT,40,-RF, IDR(I), ERR)
            CALL AMSG(RTXT,-79,' ')
            CALL MSG(' ','[','+')
            CALL AMSG(ASCREC(IDP(I)),IDL(I),'+')
            CALL MSG(' ',']',' ')
  100    CONTINUE
         RETURN
      END
