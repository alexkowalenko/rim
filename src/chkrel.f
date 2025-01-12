      SUBROUTINE CHKREL (PERM,MODE,ISTAT,NAMUSR)

         USE RM_Globals, only : OWNER
         USE RM_Relations, only : RELGET
         USE RM_Text, only : NONE

         INCLUDE 'syspar.inc'
C
C     CHECKS PERMISSION TO SEE IF USER CAN UNLOAD THIS
C     RELATION.  PERM SET TO TRUE IF USER CAN.
C
C  INPUTS:
C          MODE-------COMMAND SPECIFIED (ALL,DATA,OR SCHEMA)
C          ISTAT------------WAS THE RELATION GET SUCCESSFUL?
C          NAMUSR-----------USER PASSWORD
C
C  OUTPUT:
C            PERM-------TRUE IF USER HAS PERMISSION TO UNLOAD
C                       FALSE OTHERWISE
C
         INTEGER ISTAT
         LOGICAL PERM
         CHARACTER*(*) MODE
C
         INCLUDE 'tupler.inc'
         INTEGER NAMUSR(Z)
         LOGICAL EQ

         PERM = .TRUE.
         CALL RELGET (ISTAT)
         IF (ISTAT .NE. 0) GO TO 10
C
C  CHECK FOR OWNER
C
         IF(EQ(OWNER,NAMUSR)) GO TO 20
C
C  CHECK FOR MODIFY PASSWORD
C
         IF (EQ(MPW,NONE) .OR. EQ(MPW,NAMUSR)) GO TO 20
C
   10    PERM = .FALSE.
C
   20    RETURN
      END
