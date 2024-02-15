      LOGICAL FUNCTION NE(WORD1,WORD2)

         USE RM_Text, only : UPCASE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   COMPARE WORD1 AND WORD2 FOR NE
C             BOTH ARE ASCII-RM_Text LENGTH ZC
C
C  RM_Parameters:
C         WORD1---A LONG WORD OF RM_Text
C         WORD2---ANOTHER LONG WORD OF RM_Text
C         NE------.TRUE. IF WORD1.NE.WORD2
C                 .FALSE. IF EQ
C
         INTEGER WORD1(Z),WORD2(Z)
C
         NE = .TRUE.
         DO 100 I = 1, ZC
            CALL GETT(WORD1,I,A1)
            CALL GETT(WORD2,I,A2)
  100    IF (UPCASE(A1).NE.UPCASE(A2)) RETURN
         NE = .FALSE.
         RETURN
      END
