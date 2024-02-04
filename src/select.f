      SUBROUTINE SELECT(*)

         USE Globals, only : DFLAG
         USE Lexer, only : ASCREC, IDP, IDL, KWS
         USE Message, only: WARN
         USE Text, only : STRASC

         INCLUDE 'syspar.inc'
C
C     PROCESS SELECT COMMAND
C
         INCLUDE 'ascpar.inc'
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
         LOGICAL EQKEYW
         LOGICAL SELREL, SELATT, SELWHR, SELSRT
         CHARACTER*(ZFNAML) FN
C
C     PARSING DATA FOR QUERY COMMANDS
C
         PARAMETER (QKEYL=4)
         CHARACTER*(ZKEYWL) QKEYS(QKEYL)
         INTEGER QPTRS(2,QKEYL)
C
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C
         QKEYS(1) = 'FROM'
         QKEYS(2) = 'WHERE'
         QKEYS(3) = 'SORT'
         QKEYS(4) = 'TO'
C
C
C  PARSE THE COMMAND
C
         SC = PARSE(QKEYS,QKEYL,QPTRS)
CCC   CALL BLKDSP('QUERY PARSE',QPTRS(1,1),'IIIIIIII')
         IF (SC.LT.3) THEN
            CALL WARN(4)
            GOTO 999
         ENDIF
         J = QPTRS(1,1)
         JW = QPTRS(1,2)
         JS = QPTRS(1,3)
         JT = QPTRS(1,4)
         NS = 0
         NSOVAR = 0
C
C     GET RELATION INFO
C
         IF (.NOT.SELREL(QPTRS(1,1),QPTRS(2,1))) GOTO 900
C
C     GET ATTRIBUTE INFO
C
         IF (.NOT.SELATT(2,SC-2)) GOTO 900
C
C     EVALUATE THE WHERE CLAUSE.
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
         IF(RMSTAT.NE.0)THEN
            CALL MSG('W','NO ROWS SATISFY YOUR SELECTION CRITERIA.',' ')
            GO TO 900
         ENDIF
C     BACKSPACE TO BEFORE FIRST FOUND ROW
         NID = CID
         IVAL = IVAL - 1
         LIMVAL = 0
         IF(NS.EQ.3) NS = 2
C
C     CHECK SORT CLAUSE
C
  500    IF (JS.NE.0) THEN
C        SORT IS REQUESTED
            IF (SFUNCT) THEN
               CALL MSG('E',
     1           'FUNCTION SORT IS ON THE INDEPENDENT COLUMN.',' ')
               CALL MSG(' ','YOU MAY NOT SPECIFY A SORT CLAUSE.',' ')
               GOTO 900
            ENDIF
            IF (.NOT.SELSRT(JS,QPTRS(2,3))) GOTO 900
            NKSORT = 1
            CALL SORT(NKSORT)
            NS = 1
         ELSE
C        SORT NOT REQUESTED - MAY DO ONE ANYWAY
            IF (SFUNCT) THEN
               OFFSET = 0
               NKSORT = 1
               CALL SORT(NKSORT)
               NS = 1
            ENDIF
         ENDIF
C
C     SEE IF AN OUTPUT FILE HAS BEEN SPECIFIED
C
         IF (JT.NE.0) THEN
            CALL STRASC(FN,ASCREC(IDP(JT+1)),IDL(JT+1))
            IF (KWS(JT+1).EQ.'TERMINAL') FN = ZTRMOU
            CALL SETOUT(NOUTR,ZNOUTR,FN,STAT)
            IF (STAT.NE.0) GOTO 999
         ENDIF
C
C     CALL THE REPORT ROUTINE
C
         CALL SELRPT
         GOTO 900
C
C     ADIOS
C
  900    NUMATT = 0
         IF (JT.NE.0) THEN
            CALL SETOUT(NOUTR,ZNOUTR,ZTRMOU,STAT)
            CALL MSG(' ','SELECT COMPLETED.',' ')
         ENDIF
  999    RETURN  1
      END
