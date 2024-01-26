      SUBROUTINE LXINIT

         USE Text, only : ASCCHR

         INCLUDE 'syspar.inc'
C
C     INITIALIZE THE LXLCOM VARIABLES
C
         INCLUDE 'lxlcom.inc'

         ASBLK  = ASCCHR(' ')
         ASBLAN = ASCCHR(' ')
         ASCOM  = ASCCHR(',')
         ASDOL  = 0
         ASPLUS = ASCCHR('+')
         ASQUO  = SQUOTE
         ASLPAR = ASCCHR('(')
         ASRPAR = ASCCHR(')')
         ASSEMI = ASCCHR(';')
         RETURN
      END
