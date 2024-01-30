      SUBROUTINE STRMOV(FTXT,FPOS,NUMC,TTXT,TPOS)
         implicit none
         INTEGER :: FTXT(*), FPOS
         INTEGER :: TTXT(*), TPOS, NUMC

         INTEGER :: I, A
C
C
C  MOVE NUMC ASCII-CHARS FROM FTXT(FPOS) -> TTXT(TPOS)               .
C
         IF (FPOS < 0 .OR. NUMC <= 0 .OR. TPOS < 0) THEN
            write(*,*) "Return"
            RETURN
         END IF
         DO 100 I = 1, NUMC
            CALL GETT(FTXT,FPOS+I-1,A)
            CALL PUTT(TTXT,TPOS+I-1,A)
  100    CONTINUE
         RETURN
      END
