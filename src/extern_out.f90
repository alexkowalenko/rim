SUBMODULE (Extern) Extern_out
   implicit none

contains

   SUBROUTINE SETIN(FILE)
      !!
      !! SET THE INPUT FILE TO FILE
      !!     IF FILE IS THE TERMINAL THEN SWITCH TO UNIT NINT
      !!     ELSE SWITCH TO UNIT NINTA AND OPEN THE FILE
      !!
      USE Parameters
      USE TextFiles, only : TIOOPN, TIOCLO

      CHARACTER(len=*), intent(in) :: FILE

      INCLUDE 'files.inc'
      INCLUDE 'prom.inc'

      INTEGER :: STAT

      !
      !  SEE IF THE CURRENT INPUT FILE NEEDS TO BE CLOSED.
      !
      IF(NINT.NE.ZNINT) THEN
         CALL TIOCLO(FILE,NINT,'INPUT')
         NINT = ZNINT
      ENDIF
      !
      !  DETERMINE WHICH UNIT TO USE (TERMINAL IS ALWAYS OPEN)
      !
      CONNI = .TRUE.
      IF(FILE.NE.ZTRMIN) THEN
         CALL TIOOPN(FILE,ZNINTA,'INPUT',STAT)
         IF (STAT.EQ.0) THEN
            NINT = ZNINTA
            CONNI = .FALSE.
         ELSE
            ! NOTE.. ON ERROR MSG, MSG CALLS SETIN
            !        SO THIS CANNOT BE ERROR MESSAGE
            CALL MSG(' ','FILE NOT FOUND',' ')
         ENDIF
      ENDIF
      !
      IF(BATCH) CONNI = .FALSE.
      PRMPT = CONNI
      RETURN
   END SUBROUTINE SETIN


   SUBROUTINE SETOUT(UN,UNF,FILE,STAT)
      !!
      !! SET THE OUTPUT FILE
      !!     IF FILE IS THE TERMINAL THEN SET UN TO NOUT
      !!     ELSE SET UN TO UNF AND OPEN THE FILE
      !! IF COULD NOT OPEN FILE THEN SET STAT NE 0
      USE Parameters
      USE Globals, only : RMSTAT
      USE TextFiles, only : TIOOPN, TIOCLO
      USE Utils, only : ITOH

      INTEGER, intent(inout) :: UN
      INTEGER, intent(in) :: UNF
      CHARACTER(len=*), intent(in) :: FILE
      INTEGER, intent(out) :: STAT


      INCLUDE 'files.inc'
      !
      STAT = 0
      !
      !  SEE IF THE CURRENT OUTPUT FILE NEEDS TO BE CLOSED.
      !
      IF(UN.NE.ZNOUT) THEN
         CALL TIOCLO(FILE,UN,'OUTPUT')
         UN = ZNOUT
      ENDIF
      !
      !  DETERMINE WHICH UNIT TO USE
      !
      CONNO = .TRUE.
      IF(FILE.NE.ZTRMOU) THEN
         CALL TIOOPN(FILE,UNF,'OUTPUT',STAT)
         IF (STAT.EQ.0) THEN
            UN = UNF
            CONNO = .FALSE.
         ELSE
            CALL MSG('E','COULD NOT OPEN ''','+')
            CALL MSG(' ',FILE,'+')
            CALL MSG(' ',''' FOR OUTPUT.',' ')
            RMSTAT = 200
         ENDIF
      ENDIF
      !
      IF(BATCH) CONNO = .FALSE.
      RETURN
   END SUBROUTINE SETOUT


   SUBROUTINE PROMPT(PTXT)
      !!
      !! **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
      !!
      !! ISSUE TERMINAL PROMPT
      !!
      USE Parameters
      USE Text, only : ABLANK, LOCASE, CHRASC

      INTEGER, intent(in) :: PTXT(*)
      INTEGER :: a, i, l

      INCLUDE 'msgcom.inc'
      INCLUDE 'files.inc'

      character*(zc) p

      IF(nint.EQ.znint) then
         l = 0
         do i = 1, zc
            call gett(ptxt,i,a)
            if (a.eq.ablank) goto 11
            if (i.ne.1) a = locase(a)
            p(i:i) = chrasc(a)
            l = i
         end do
11       if (l.ne.0) write(nout,101) p(1:l)
101      FORMAT(a,$)
      endif
      RETURN
   END SUBROUTINE PROMPT


   SUBROUTINE PRMSET(MODE,PR)
      !!
      !! SET THE PROMPT CHARACTERS
      !!
      !! INPUT  - MODE... 'INIT' - SET INITIAL VALUES
      !!                  'SET'  - SET NEW VALUES
      !!                  'RESET'- RESTORE INITIAL VALUES
      !!          PR..... NEW VALUE FOR PROMPT
      !!
      USE Parameters
      USE Text, only : ASCCHR, LOCASE
      USE Utils, only : ZMOVE

      CHARACTER(len=*), intent(in) :: MODE,PR
      !
      INCLUDE 'prom.inc'
      CHARACTER(len=1) :: CH
      INTEGER :: AS, I
      !
      IF (MODE.EQ.'INIT' .OR. MODE.EQ.'SET') THEN
         DO I = 1, ZC
            IF (I.LE.LEN(PR)) THEN
               CH = PR(I:I)
            ELSE
               CH = ' '
            ENDIF
            AS = ASCCHR(CH)
            IF (I.GT.1) AS = LOCASE(AS)
            CALL PUTT(PROM,I,AS)
         END DO
      ENDIF
      !
      IF (MODE.EQ.'INIT') CALL ZMOVE(INIPRM,PROM)
      IF (MODE.EQ.'RESET') CALL ZMOVE(PROM,INIPRM)
      RETURN
   END SUBROUTINE PRMSET

END SUBMODULE Extern_out
