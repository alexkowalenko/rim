      SUBROUTINE DBOPEN(NEWNAM,NEWOK)

         USE Globals, only : DFLAG, DBDATE, DBTIME
         USE Files, only : FILE1, F1OPN
         USE DateTime, only: RMTIME, RMDATE
         USE RandomFiles, only: RIOCLO
         USE System, only: SYSDBN, CHKFIL

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  OPEN A RIM DATABASE.
C
C     NEWNAM CONTAINS THE DB NAME
C     NEWOK = .TRUE. IF THE DB MAY BE CREATED
C
         CHARACTER*(*) NEWNAM
         LOGICAL NEWOK
         INCLUDE 'rimcom.inc'
         INCLUDE 'f2com.inc'
         INCLUDE 'f3com.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'ascpar.inc'
C
         LOGICAL RW
         CHARACTER*(ZFNAML) RIMDB1,RIMDB2,RIMDB3,RIMDBX
C
         RMSTAT = 0
         CALL RMCLOS
C
C  INITIALIZE AND SET DATE, AND TIME
C
         CALL RMINIT
         DBDATE = RMDATE()
         DBTIME = RMTIME()
C
C  FIX UP THE FILE NAMES.
C
         CALL SYSDBN(NEWNAM,RIMDB1,RIMDB2,RIMDB3,RIMDBX)
C
C  CHECK IF THE DB EXISTS
C
         IF (NEWOK) GOTO 100
         IF (CHKFIL(RIMDB1,RW) .AND. CHKFIL(RIMDB2,RW) .AND.
     X         CHKFIL(RIMDB3,RW)) GOTO 100
         CALL MSG('E','I CANNOT OPEN THE FILES.',' ')
         GOTO 999
C
C  OPEN FILE 1.
C
  100    CALL F1OPN(RIMDB1)
         IF((RMSTAT.NE.0).AND.(RMSTAT.NE.15)) GO TO 999
C
C  OPEN FILE 2.
C
         CALL F2OPN(RIMDB2)
         IF((RMSTAT.NE.0).AND.(RMSTAT.NE.12).AND.(RMSTAT.NE.15)) THEN
            GO TO 999
         END IF
C
C  OPEN FILE 3.
C
         CALL F3OPN(RIMDB3)
         IF((RMSTAT.NE.0).AND.(RMSTAT.NE.12).AND.(RMSTAT.NE.15)) THEN
            GO TO 999
         END IF
C
C  IF THIS IS A NEW DATABASE WE NEED TO SET UP THE FIRST BTREE.
C
         IF(DFLAG) DBDATE = RMDATE()
C
C  IF THERE IS A LOT OF DELETED SPACE ON FILE TWO THEN ADVISE THE USER
C  TO DO A RELOAD.
C
C     CALL PERDEL
         RETURN
C
C     AN ERROR SHOULD CLOSE ANY OPEN FILES WITHOUT UPDATE
C
  999    CALL RIOCLO(FILE1)
         CALL RIOCLO(FILE2)
         CALL RIOCLO(FILE3)
         RETURN
      END
