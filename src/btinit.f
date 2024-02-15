      SUBROUTINE BTINIT(START)
         INCLUDE 'syspar.inc'
C
C  PURPOSE:   INITIALIZE FOR A NEW BTREE
C
C  RM_Parameters:
C         START---NEW RECORD USED FOR THIS BTREE
C
         INCLUDE 'btbuf.inc'
C
         INTEGER START
C
C  GET THE NEXT NODE.
C
         CALL BTGET(LF3REC,N1)
C
C  INSERT THE END-OF-LIST WORD.
C
         VALUE(1,N1) = ENDWRD
         VALUE(2,N1) = 1
         VALUE(3,N1) = 0
C
C  WRITE OUT THIS NODE.
C
         CALL BTPUT(LF3REC)
         START = LF3REC
         LF3REC = LF3REC + 1
         RETURN
      END
