      FUNCTION LOCPRM(RNAME,JCODE)

         USE RM_Globals, only : USERID, OWNER, RMSTAT
         USE RM_Text, only : NONE
         INCLUDE 'syspar.inc'
C
C  CHECK PERMISSION FOR A USERID AGAINST A RELATION.
C
C  RM_Parameters:
C         RNAME---RELATION NAME
C         JCODE---READ/MODIFY CODE
C                 1 FOR READ
C                 2 FOR MODIFY
C         LOCPRM--O FOR OK, 1 FOR NO-WAY

         INCLUDE 'tupler.inc'
         LOGICAL EQ,NE
         INCLUDE 'dclar1.inc'
C
C  RETRIEVE THE PASSWORDS.
C
         IF(NE(RNAME,NAME)) GO TO 900
C
C  COMPARE THE PASSWORDS.
C
C
C        READ.
C
         IF(JCODE.EQ.1) THEN
            IF(EQ(RPW,NONE)) GO TO 1000
            IF(EQ(RPW,USERID)) GO TO 1000
            IF(NE(MPW,NONE).AND.EQ(MPW,USERID)) GO TO 1000
            IF(NE(OWNER,NONE).AND.EQ(OWNER,USERID)) GO TO 1000
C
C        MODIFY.
C
         ELSE IF(JCODE.EQ.2) THEN
            IF(EQ(MPW,NONE)) GO TO 1000
            IF(EQ(MPW,USERID)) GO TO 1000
            IF(NE(OWNER,NONE).AND.EQ(OWNER,USERID)) GO TO 1000
         ENDIF
C
C     NO WAY.
C
  900    LOCPRM = 1
         RMSTAT = 90
         RETURN
C
C     OK.
C
 1000    LOCPRM = 0
         RMSTAT = 0
         RETURN
      END
