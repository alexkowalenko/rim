      SUBROUTINE UNPASS(ALL,IRCNTR,IDAY,MODE,LHASH,ATREL)

         USE Globals, only : DBNAME, USERID
         USE Text, only : BLANK, NONE
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     UNLOAD THE PASSWORDS OF A DATABASE.
C
C  INPUTS:
C          ALL------------TRUE IF ALL RELATIONS ARE SPECIFIED.
C          IRCNTR---------NUMBER OF RELATIONS IF SPECIFIED (ALL IS FALSE
C          IDAY-----------DAY CODE FOR HASH.
C          MODE -----------COMMAND SPECIFIED.
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'buffer.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplel.inc'
         INCLUDE 'msgcom.inc'
         INCLUDE 'dclar1.inc'
         INCLUDE 'dclar3.inc'
         LOGICAL EQ, NE
         LOGICAL ALL,PERM,LHASH
C
C
         CHARACTER*(*) MODE
         INTEGER IREL(Z,1),ATREL(Z,1)
         EQUIVALENCE (BUFFER(1),IREL(1,1))
         INTEGER FMTSTR(3)
C
C
C     MAX CHARACTERS PER LINE FOR UNLOAD
C
         PARAMETER (UMCPL=80)
C
C
         IACNTR = 0
C
    1    CALL MSG('R','DEFINE ','+')
         CALL AMSG(DBNAME,ZC,' ')
C
         CALL MSG('R','OWNER ','+')
CCC   IF (LHASH) THEN
CCC      CALL HASHIN (USERID,1,IREL(1,1),1)
CCC      CALL AMSG(IREL(1,1),ZC,' ')
CCC   ELSE
         CALL AMSG(USERID,ZC,' ')
CCC   ENDIF
C
C
   50    IF (IRCNTR .EQ. 0) GO TO 400
C
C  PRINT PASSWORDS
C
  220    CONTINUE
         CALL MSG('R','PASSWORDS',' ')
         J = LOCREL (BLANK)
         DO 300 I = 1,IRCNTR
            IF (ALL) GO TO 225
            J = LOCREL (IREL(1,I))
            CALL ZMOVE(RNAME,IREL(1,I))
            GO TO 240
C
  225       CALL CHKREL (PERM,MODE,ISTAT,USERID)
            IF (.NOT. PERM) GO TO 400
            CALL ZMOVE(RNAME,NAME)
C
  240       NUM = 31
            IF (LHASH) NUM = 39
            CALL ZMOVE(RPW1,RPW)
            DO 250 J = 1,2
               IF (NE(RPW1,NONE)) THEN
                  IF (J.EQ.1) CALL MSG('R','RPW FOR ','+')
                  IF (J.EQ.2) CALL MSG('R','MPW FOR ','+')
                  CALL AMSG(RNAME,-ZC,'+')
                  CALL MSG(' ',' IS ''','+')
CCCCCCCC IF (LHASH) CALL HASHIN (RPW1,IDAY,LINE,23)
                  IF (.NOT. LHASH) CALL AMSG(RPW1,-ZC,'+')
                  CALL MSG(' ','''',' ')
               ENDIF
               CALL ZMOVE(RPW1,MPW)
  250       CONTINUE
C
  300    CONTINUE
C
C
  400    CONTINUE
         CALL MSG('R','END',' ')
         RETURN
      END
