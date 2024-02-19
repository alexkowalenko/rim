      FUNCTION LOCVAR(ANAME)

         USE RM_BufferData, only: BUFFER
         USE RM_Text, only : BLANK
         USE Utils, only : ZMOVE, ITOH

         INCLUDE 'syspar.inc'
C
C     LOOK FOR VARIABLE
C
C         ANAME---NAME OF VARIABLE
C         LOCVAR--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

         INCLUDE 'tuplea.inc'
         INCLUDE 'pgmcom.inc'
         LOGICAL EQ
         LOGICAL NE
         INCLUDE 'dclar1.inc'
         LOCVAR = 0
C
C  LOOK FOR THE VARIABLE
C
         I = PGVBLK
         IF (I.LE.0) GOTO 9000

  300    IF(I.GE.PGVPTR) GO TO 9000
         IF(EQ(BUFFER(I),ANAME)) GOTO 500

         CALL ITOH(X1,X2,BUFFER(I+Z))
         I = I + Z + 2 + X2
         GO TO 300
C
C  WE FOUND THE VARIABLE
C
  500    CONTINUE
         CALL ZMOVE(ATTNAM,ANAME)
         ATTLEN = BUFFER(I+Z)
         ATTYPE = BUFFER(I+Z+1)
         ATTCOL = I+Z+2
         CALL ITOH(ATTCHA,ATTWDS,ATTLEN)
         ATTFOR = 0
         GO TO 9999
C
C  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
C
 9000    CONTINUE
         CALL ZMOVE(ATTNAM,BLANK)
         LOCVAR = 1
 9999    CONTINUE
         RETURN
      END
