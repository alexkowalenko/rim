      LOGICAL FUNCTION EQ(WORD1,WORD2)

         USE RM_Text, only : UPCASE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   COMPARE WORD1 AND WORD2 FOR EQ
C             BOTH ARE ASCII-RM_Text LENGTH ZC
C
C  RM_Parameters:
C         WORD1---A LONG WORD OF RM_Text
C         WORD2---ANOTHER LONG WORD OF RM_Text
C         EQ------.TRUE. IF WORD1.EQ.WORD2
C                 .FALSE. IF NOT EQ
C
         INTEGER WORD1(Z),WORD2(Z)
C
         EQ = .FALSE.
         DO 100 I = 1, ZC
            CALL GETT(WORD1,I,A1)
            CALL GETT(WORD2,I,A2)
  100    IF (UPCASE(A1).NE.UPCASE(A2)) RETURN
         EQ = .TRUE.
         RETURN
      END
