      SUBROUTINE RTOA(STRING,SC,FMT,RNUM,IERR)

         USE, intrinsic :: iso_fortran_env

         USE Text, only : FILCH

         INCLUDE 'syspar.inc'
C
C     CONVERT A REAL (RNUM) TO ASCII-TEXT (STRING)
C     IF THE REAL WILL NOT FIT, STRING IS
C     BLANKED OUT AND IERR IS RETURNED NON-ZERO.
C
C     STRING....REPOSITORY FOR TEXT OF RNUM
C     SC .......STARTING CHARACTER POS
C     FMT ......FORMAT (LENGTH + 100*DECIMAL PLACES + 10000*REPEAT)
C     RNUM......VALUE TO CONVERT.
C     IERR......0 IF VAL FITS, 1 OTHERWISE
C
         INTEGER, intent(out) :: STRING(*)
         REAL(real64) :: RNUM
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'lxlcom.inc'
         LOGICAL FFMT
C
         REAL(real64) :: R,RR,REAL,POINT,RREM
C

  100    IERR = 0
         S = SC - 1
         F = MOD(FMT,10000)
         FFMT = .TRUE.
         IF (FMT.LT.0) THEN
            F = 0 - F
            FFMT = .FALSE.
         ENDIF
         L = MOD(F,100)
         CALL FILCH(STRING,SC,L,ABLANK)
         D = F/100
C
         IF (FFMT) THEN
C
C     F FORMAT
C
            CALL ROUN(RNUM,D,REAL)
            R = REAL
            IL = L - D - 1
   91       CALL RITOA(STRING,SC,IL,R,POINT,IERR)
            IF(IERR.NE.0) GO TO 900
            CALL PUTT(STRING,SC+IL,DECIM)
            IF(D.GT.0) THEN
               POINT = POINT * (10.0D0**D)
               CALL FILCH(STRING,SC+IL+1,D,U0)
   92          CALL RITOA(STRING,SC+IL+1,D,POINT,RREM,IERR)
               IF(IERR.NE.0) GO TO 900
            ENDIF
         ELSE
C
C        E - FORMAT
C
C        INCREASE D FOR SINGLE DIGIT LEFT OF DECIMAL
            D = D + 1
            IL = L - D - 5
            IF(IL.LT.0) GO TO 900
            IF(RNUM.LT.0.) THEN
               IF (IL.LT.1) GOTO 900
               CALL PUTT(STRING,S+IL,MNSIGN)
            ENDIF
C        REMEMBER START OF DIGITS
            ILDP = IL
            IL = IL + 1
C
C        FIND THE INTEGER AND THE EXPONENT
C
            IE = IEXP(RNUM)
            RR = ABS(RNUM)/(10.0D0**IE)
            CALL ROUN(RR,D,RR)
            IE = IE - 1
            ESGN = PLSIGN
            IF (IE.LT.0) THEN
               IE = 0 - IE
               ESGN = MNSIGN
            ENDIF
C
            NUME = 2
            IF(IE.GE.100) NUME = 3
            R = RR*(10.0D0**D)
            CALL FILCH(STRING,SC+IL,D,U0)
            CALL RITOA(STRING,SC+IL,D,R,RREM,IERR)
            IF(IERR.NE.0) GO TO 900
            IL = IL + D
            IF (NUME.LT.3) THEN
               CALL PUTT(STRING,SC+IL,UECH)
               IL = IL + 1
            ENDIF
            CALL PUTT(STRING,SC+IL,ESGN)
            IL = IL + 1
            CALL ITOA(STRING,SC+IL,NUME,IE,IERR)
            IF(IERR.NE.0) GO TO 900
            IF (IE.LT.10) CALL PUTT(STRING,SC+IL,U0)
C
C        REVERSE DECIMAL AND FIRST DIGIT FOR X.XXX FORMAT
C
            CALL GETT(STRING,SC+ILDP+1,FD)
            CALL PUTT(STRING,SC+ILDP,FD)
            CALL PUTT(STRING,SC+ILDP+1,DECIM)
         ENDIF
C
         GO TO 9999
C
C     ERROR
C
  900    CALL FILCH(STRING,SC,L,ASSTAR)
         IERR = 1
 9999    CONTINUE
         RETURN
      END
