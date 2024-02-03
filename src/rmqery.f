      SUBROUTINE RMQERY(*,QCOM)

         USE Globals, only : DFLAG, MRINDX

         INCLUDE 'syspar.inc'
C
C     PROG INTERFACE DRIVER FOR QUERY OF THE RIM DATA BASE.
C     ( THIS IS SIMILAR TO QUERY, BUT DOES NOT LOAD THE
C       SELATT ROUTINES AND COMMONS )
C
C     ALSO NO SORTING (9/28/89)
C
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'selcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'rimcom.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'whcom.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'files.inc'
         INCLUDE 'srtcom.inc'
C
         LOGICAL SELREL, SELWHR, SELSRT
C
         CHARACTER*(*) QCOM
C
C     PARSING DATA FOR QUERY COMMANDS
C
         PARAMETER (QKEYL=3)
         CHARACTER*(ZKEYWL) QKEYS(QKEYL)
         INTEGER QPTRS(2,QKEYL)
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2,0,0)
            GOTO 999
         ENDIF
C
C
         QKEYS(1) = 'FROM'
         QKEYS(2) = 'WHERE'
         QKEYS(3) = 'SORTED'
C
C  PARSE THE COMMAND
C
         SC = PARSE(QKEYS,QKEYL,QPTRS)
         J = QPTRS(1,1)
         JW = QPTRS(1,2)
         JS = QPTRS(1,3)
         NS = 0
C
C
C     GET RELATION INFO
C
         IF (.NOT.SELREL(QPTRS(1,1),QPTRS(2,1))) GOTO 900
C
         CALL RELGET(I)
         IF (I.NE.0) THEN
            RMSTAT = 20
            GOTO 999
         ENDIF
C
C  NO ATTRIBUTE INFO
C
C  EVALUATE THE WHERE CLAUSE.
C
         NBOO = 0
         LIMTU = ALL9S
         IF(JW.EQ.0) GO TO 500
         IF (.NOT.SELWHR(JW,QPTRS(2,2))) GOTO 900
         IF(RMSTAT.NE.0) GO TO 900
C
C  SEE IF ANY TUPLES SATISFY THE WHERE CLAUSE.
C
         CALL RMLOOK(IDUMMY,1,1,LENGTH)
         IF(RMSTAT.NE.0) GOTO 900
C     BACKSPACE TO BEFORE FIRST FOUND ROW
         NID = CID
         IVAL = IVAL - 1
         LIMVAL = 0
         IF(NS.EQ.3) NS = 2
C
C  SEE IF SORTING IS NEEDED OR ASKED FOR.
C
  500    IF(JS.NE.0) THEN
C
C        CHECK PI LIMITS FOR SORTED RETRIEVALS
C
            IF (MRINDX.LT.1 .OR. MRINDX.GT.3) THEN
               RMSTAT = 70
               GOTO 900
            ENDIF
C
C        SORT THE DATA
C
            IF (.NOT.SELSRT(JS,QPTRS(2,3))) GOTO 900
            IF(IERR.EQ.1) GO TO 900
            NKSORT = 1
            CALL SORT(NKSORT)
            NS = 1
C
C        OPEN THE SORT FILE TO SETUP FOR RMGET
C
            LENGTH = NCOL
            CALL GTSORT(IP,1,-1,LENGTH)
C
C        USER'S RMGET WILL GET THE RECORDS
C
         ENDIF
C
C     INIT SOME OTHER VARIABLES
C
         CALL RMPII
C
C     ADIOS
C
  900    CONTINUE
  999    RETURN  1
      END
