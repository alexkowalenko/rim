      SUBROUTINE FILCH(ASC,ST,NC,CH)

         INCLUDE 'syspar.inc'
         INTEGER, intent(out) :: ASC(*)
C
C     FILL ASC (ASCII-TEXT) WITH CH (ASCII-CHAR)
C     ST IS START IN ASC, NC IS NUMBER OF CHARS
C
         DO 100 I = ST,ST+NC-1
  100    CALL PUTT(ASC,I,CH)
         RETURN
      END
