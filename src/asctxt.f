      SUBROUTINE ASCTXTX(ATXT,LTXT,CSTR)

         USE Text, only : ASCCHR

         INCLUDE 'syspar.inc'
C
C     RETURN THE ASCII-TEXT EQUIVALENT OF CSTR
C
C        ATXT -- PLACE TO PUT ASCII-TEXT
C        LXTX -- LENGTH OF ATXT
C        CSTR -- CHARACTER STRING
C
         INTEGER ATXT(1)
         CHARACTER*(*) CSTR
C
         INCLUDE 'ascpar.inc'
C
         L = LEN(CSTR)
         DO 100 I = 1, LTXT
            IF (I.LE.L) THEN
               CALL PUTT(ATXT,I,ASCCHR(CSTR(I:I)))
            ELSE
               CALL PUTT(ATXT,I,ABLANK)
            ENDIF
  100    CONTINUE
         RETURN
      END
