      SUBROUTINE MSGCMV(MTEXT,CTYPE)

         USE Text, only : ASCCHR, LOCASE

         INCLUDE 'syspar.inc'
C
C  ROUTINE TO ADD CHARS TO THE OUTPUT LINE
C
C  PARAMETERS
C
C         MTEXT---TEXT OF MESSAGE
C
C         CTYPE---CASE CONVERSION CODE
C                 'U' - LEAVE UPPERCASE
C                 'F' - LOWERCASE ALL BUT FIRST CHARACTER
C                 'L' - LOWERCASE
C                 OTHER - SAME AS 'L'
C
         CHARACTER*(*) MTEXT
         CHARACTER*(*) CTYPE
C
         INCLUDE 'files.inc'
         INCLUDE 'msgcom.inc'
C
         LOGICAL UCASE, FCHAR
C
         UCASE = .FALSE.
         FCHAR = .FALSE.
         L = LEN(MTEXT)
         IF (CTYPE.EQ.'F') FCHAR = .TRUE.
         IF (CTYPE.EQ.'U') UCASE = .TRUE.
C
         DO 100 I = 1, L
            IF (FCHAR .OR. UCASE) THEN
               CALL PUTT(MSGREC,MSGPTR+I,ASCCHR(MTEXT(I:I)))
            ELSE
               CALL PUTT(MSGREC,MSGPTR+I,LOCASE(ASCCHR(MTEXT(I:I))))
            ENDIF
            IF (FCHAR .AND. (MTEXT(I:I).NE.' ')) FCHAR = .FALSE.
  100    CONTINUE
         MSGPTR = MSGPTR + L
         RETURN
      END
