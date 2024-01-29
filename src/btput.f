      SUBROUTINE BTPUT(ID)

         USE Globals, only : IFMOD

         INCLUDE 'syspar.inc'
C
C  PURPOSE:    TURN ON THE WRITE FLAG ON THE INDICATED BLOCK
C
C  PARAMETERS
C     INPUT:   ID------RECORD NUMBER
         INCLUDE 'f3com.inc'
         INCLUDE 'rimcom.inc'
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
