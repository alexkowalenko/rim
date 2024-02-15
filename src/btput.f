      SUBROUTINE BTPUT(ID)

         USE Files, only : NUMIC, ICORE
         USE RM_Globals, only : IFMOD, RMSTAT

         implicit none

         INTEGER, intent(in) :: ID

         INTEGER :: NUMB
C
C  PURPOSE:    TURN ON THE WRITE FLAG ON THE INDICATED BLOCK
C
C  RM_Parameters
C     INPUT:   ID------RECORD NUMBER
C
C  LOOK FOR THIS BLOCK IN CORE.
C
         DO 100 NUMB=1,NUMIC
            IF(ID.EQ.ICORE(3,NUMB)) GO TO 200
  100    CONTINUE
C
C  DISASTER. WE CANNOT FIND THE BLOCK.
C
         RMSTAT = 1004
         RETURN
C
C  SET THE WRITE FLAG.
C
  200    CONTINUE
         ICORE(2,NUMB) = 1
         IFMOD = .TRUE.
         RETURN
      END
