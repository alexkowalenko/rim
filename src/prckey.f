      SUBROUTINE PRCKEY(IWORD,MAT,ATT)
         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE PROCESSES KEY ATTRIBUTES WHEN
C  LOADING (ADDING) OR DELETING OR REPLACING
C  A TUPLE TO/FROM A RELATION.
C
C  RM_Parameters:
C         IWORD---KEYWORD: ADD OR DELETE
C         MAT-----CONTAINS ONE TUPLE OF DATA
C         ATT-----ARRAY OF TUPLEA VALUES
C                 (ATTRIBUTE TABLE)
C
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'start.inc'
         INCLUDE 'rimptr.inc'
         CHARACTER*(*) IWORD
         INTEGER COLUMN
         INTEGER MAT(1),ATT(ZTUPAL,1)
C
C
C     FOR EACH ROW OF ATTRIBUTE TABLE
C
         DO 107 II = 1,NATT
C
C         MOVE THE ATT-TABLE ROW INTO 'TUPLEA'
C
            CALL BLKMOV(ATTNAM,ATT(1,II),ZTUPAL)
            IF(ATTKEY.EQ.0) GO TO 107
            START = ATTKEY
            COLUMN = ATTCOL
            IF(ATTWDS.EQ.0)COLUMN = MAT(ATTCOL) + 2
C
C         KEYS FOR TUPLE THAT IS BEING ADDED
C
            IF(IWORD.EQ.'ADD')THEN
               KSTART = ATTKEY
               IF(MAT(COLUMN).EQ.NULL) GO TO 107
               CALL BTADD(MAT(COLUMN),REND,ATTYPE)
C
C             IF THE STARTING NODE HAS CHANGED, UPDATE IT
C                IN BOTH THE DB ATT-TABLE AND THE IN-CORE
C                VERSION 'ATT'
C
               IF(START.NE.KSTART)THEN
                  ATT(ZTUPAL,II) = START
                  J = LOCATT(ATTNAM,NAME)
                  CALL ATTGET(ISTAT)
                  ATTKEY = START
                  CALL ATTPUT(ISTAT)
               ENDIF
C
               GOTO 107
            ENDIF
C
C         KEYS FOR TUPLE THAT IS BEING DELETED
C
            CALL BTREP(MAT(COLUMN),0,CID,ATTYPE)
C
  107    CONTINUE
         RETURN
      END
