      SUBROUTINE AMSG(MTEXT,NUMC,MCONT)

         USE Text, only : ABLANK
         USE TextFiles, only : TIOOUT

         INCLUDE 'syspar.inc'
C
C  ROUTINE TO FORMAT AND PRINT ASCII-TEXT
C
C  PARAMETERS
C
C         MTEXT---TEXT OF MESSAGE  (ASCII-TEXT)
C
C         NUMC----NUMBER OF CHARS (NEG = DELETE TRAILING BLANKS)
C
C         MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
C
C-----------------------------------------

         INTEGER MTEXT
         INTEGER NUMC
         CHARACTER*1 MCONT
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'flags.inc'
         INCLUDE 'files.inc'
         INCLUDE 'msgcom.inc'
         LOGICAL DTB
C
C
         IF (NUMC.EQ.0) GOTO 800
         L = NUMC
         DTB = .FALSE.
         IF (L.LT.0) THEN
            L = 0 - L
            DTB = .TRUE.
         ENDIF
         IF (L+MSGPTR.GT.ZPRINL) L = ZPRINL - MSGPTR
C
         LNB = 0
         DO 100 I = 1, L
            CALL GETT(MTEXT,I,A)
            CALL PUTT(MSGREC,MSGPTR+I,A)
            IF (A.NE.ABLANK) LNB = I
  100    CONTINUE
         IF (DTB) L = LNB
         MSGPTR = MSGPTR + L
C
C     PRINT IF NO CONTINUATION
C     (LINE MAY BE LONGER THAN TERMINAL LINE LENGTH)
C
  800    IF (MCONT.NE.' ') RETURN
  810    L = MSGPTR
         IF (MSUNIT.EQ.ZNOUT .AND. L.GT.UTERML) L = UTERML
         CALL TIOOUT(MSUNIT,MSGREC,L,IERR)
         MSGPTR = MSGPTR - L
         IF (MSGPTR.EQ.0) RETURN
         CALL STRMOV(MSGREC,L+1,MSGPTR,MSGREC,1)
         GOTO 810
      END
