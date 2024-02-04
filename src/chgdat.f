      SUBROUTINE CHGDAT(*)

         USE Globals, only : DFLAG
         USE Message, only : WARN
         USE DateTime, only : RMDATE

         INCLUDE 'syspar.inc'
C
C     PROCESS CHANGE DATA COMMAND
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
         INCLUDE 'buffer.inc'
C
         LOGICAL SELREL, SELATT, SELWHR
         INCLUDE 'dclar1.inc'
C
C
C     PARSING DATA FOR CHANGE COMMAND
C
         PARAMETER (QKEYL=3)
         CHARACTER*(ZKEYWL) QKEYS(QKEYL)
         INTEGER QPTRS(2,QKEYL)
C
C     ---------------------------------------------
C
C     CHECK FOR A DATABASE
C
         IF (.NOT.DFLAG) THEN
            CALL WARN(2)
            GOTO 999
         ENDIF
C
C
         QKEYS(1) = 'TO'
         QKEYS(2) = 'IN'
         QKEYS(3) = 'WHERE'
C
C
C  PARSE THE COMMAND
C
         SC = PARSE(QKEYS,QKEYL,QPTRS)

         JT = QPTRS(1,1)
         J = QPTRS(1,2)
         JW = QPTRS(1,3)
C
C     GET RELATION INFO
C
         IF (.NOT.SELREL(QPTRS(1,2),QPTRS(2,2))) GOTO 999
CCC   CALL BLKDSP('QUERY SELREL (TUPLER)',NAME,'ZZZZIIIII')
         CALL RELGET(STATUS)
         I = LOCPRM(NAME,2)
         IF (I.NE.0) THEN
            CALL WARN(8)
            GOTO 999
         ENDIF
C
C     GET ATTRIBUTE INFO
C
         CALL LXSREC(2,ANAME,ZC)
         I = LOCATT(ANAME,NAME)
         IF (I.NE.0) THEN
            CALL WARN(3,ANAME,NAME)
            GOTO 999
         ENDIF
         CALL ATTGET(ISTAT)
         IF (ISTAT.NE.0) GOTO 999
C
C  CALL CHANGE TO FINISH PROCESSING THE COMMAND.
C
         CALL BLKDEF(7,MAXCOL,1)
         CALL BLKDEF(8,MAXCOL,1)
         CALL BLKDEF(9,MAXCOL,1)
         KQ1 = BLKLOC(7)
         KQ11 = BLKLOC(8)
         KQ12 = BLKLOC(9)
         RDATE = RMDATE()
         CALL CHANGE(BUFFER(KQ1),BUFFER(KQ11),IFLAG,BUFFER(KQ12))
C
  999    RETURN 1
      END
