      SUBROUTINE XHIBIT(*)

         USE Globals, only : DFLAG, USERID, OWNER
         USE Message, only: WARN
         USE Text, only : BLANK, NONE

         INCLUDE 'syspar.inc'
C
C     LIST ALL RELATIONS HAVING SELECTED ATTRIBUTES.
C
         INCLUDE 'ascpar.inc'
         INCLUDE 'tokens.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'files.inc'
         INCLUDE 'buffer.inc'
C
         LOGICAL EQ
         LOGICAL FLAG
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
C  EDIT THE EXHIBIT COMMAND
C
         IF(ITEMS.EQ.1) THEN
            CALL MSG('E','EXHIBIT REQUIRES A LIST OF COLUMNS.',' ')
            GOTO 999
         ENDIF
         NUMBER = ITEMS - 1

C     ALLOCATE A BLOCK FOR THE ATTRIBUTE LIST
         CALL BLKDEF(11,NUMBER,Z)
         B = BLKLOC(11) - Z
C
C  COMMAND IS OKAY
C
         FLAG = .FALSE.
C
         DO 100 I=1,NUMBER
            L = B + I*Z
  100    CALL LXSREC(I+1,BUFFER(L),ZC)
         CALL MSG('R','TABLES CONTAINING ','+')
         DO 110 I = 1, NUMBER
            L = B + I*Z
            CALL MSG(' ',' ','+')
  110    CALL AMSG(BUFFER(L),-ZC,'+')
         CALL MSG(' ',' ',' ')
C
C  GO THROUGH EACH RELATION.
C
         I = LOCREL(BLANK)
  200    CALL RELGET(ISTAT)
         IF(ISTAT.NE.0) GO TO 500
C
C  SEE IF ALL THE ATTRIBUTES LISTED APPEAR IN THIS RELATION
C
         DO 300 I=1,NUMBER
            L = B + I*Z
            K = LOCATT(BUFFER(L),NAME)
  300    IF(K.NE.0) GO TO 200
C
C  CHECK USER READ SECURITY.
C
         IF(EQ(USERID,OWNER)) GO TO 400
         IF(EQ(RPW,NONE)) GO TO 400
         IF(EQ(RPW,USERID)) GO TO 400
         IF(EQ(MPW,USERID)) GO TO 400
C     RELATION IS NOT AVAILABLE TO THE USER.
         GO TO 200
C
C  ATTRIBUTES ARE IN THIS RELATION
C
  400    CALL MSG('R','   ','+')
         CALL AMSG(NAME,ZC,' ')
         FLAG = .TRUE.
         GO TO 200
C
C  SEE IF ANY RELATIONS HAD THE ATTRIBUTES
C
  500    IF(FLAG) GO TO 999
C
C  NONE OF THE RELATIONS HAD THE ATTRIBUTES
C
         CALL MSG('W','THOSE COLUMNS ARE NOT IN ANY TABLES.',' ')
         GO TO 999
C
C
C  DONE WITH EXHIBIT
C
  999    CALL BLKCLR(11)
         RETURN 1
      END
