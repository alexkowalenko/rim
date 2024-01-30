      SUBROUTINE STRASC(STR,ASC,NC)
         USE Text, only : CHRASC
         INCLUDE 'syspar.inc'
C
C     RETURN THE STRING EQUIVALENT OF ASC (ASCII-TEXT, LENGTH NC)

c     UNIX version does not uppercase the result.  This routine
c     is used to make filenames and system commands.  These are
c     uniformly uppercase on many machines, but are mixed case
c     on UNIX.

C
         CHARACTER*(*) STR
C
         STR = ' '
         DO 100 I = 1, MIN(NC,LEN(STR))
            CALL GETT(ASC,I,CH)
  100    STR(I:I) = CHRASC(CH)
         RETURN
      END
