      SUBROUTINE KOMPXX(VALUE1,VALUE2,LEN,NK,OK,TYPE)
         INCLUDE 'syspar.inc'
C
C  THIS ROUTINE COMPARES VALUE1 AND VALUE2 TO SEE IF THEY MEET THE
C  DESIRED CONDITIONS.
C
C  RM_Parameters
C         VALUE1--FIRST VALUE
C         VALUE2--SECOND VALUE
C         LEN-----VALUE LENGTHS
C         NK------NUMBER FOR COMPARISON TYPE
C         OK------.FALSE. COMING IN, .TRUE. GOING OUT IF THE CONDITIONS
C                 ARE MET
C         TYPE----TYPE OF VALUES BEING COMPARED
C
         INCLUDE 'rmatts.inc'
C
         INTEGER VALUE1(1)
         INTEGER VALUE2(1)
         INTEGER TYPE
         LOGICAL OK
         IF(NK.EQ.-1) THEN
C         FAILS.
            IF(VALUE1(1).EQ.ZIMISS) OK = .TRUE.
            IF(VALUE1(1).EQ.ZINAPP) OK = .TRUE.
            GO TO 999
         ENDIF
C
         IF(VALUE1(1).EQ.ZIMISS) GO TO 999
         IF(VALUE1(1).EQ.ZINAPP) GO TO 999
         IF(NK.EQ.1) THEN
C       EXISTS
            OK = .TRUE.
            GO TO 999
         ENDIF
C
         IF(TYPE.EQ.KZINT .OR. TYPE.EQ.KZDATE .OR. TYPE.EQ.KZTIME)
     X      CALL KMPARI(VALUE1,VALUE2,LEN,NK,OK)
         IF(TYPE.EQ.KZREAL)
     X      CALL KMPARR(VALUE1,VALUE2,LEN,NK,OK)
         IF(TYPE.EQ.KZDOUB)
     X      CALL KMPARD(VALUE1,VALUE2,LEN/2,NK,OK)
         IF(TYPE.EQ.KZTEXT)
     X      CALL KMPART(VALUE1,VALUE2,LEN,NK,OK)
  999    CONTINUE
         RETURN
      END
