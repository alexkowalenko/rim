      SUBROUTINE MOTSCN(MOTID,IPTR)

         USE RM_BTree, only: BTGET
         USE Utils, only : ITOH

         INCLUDE 'syspar.inc'
C
C  PURPOSE:  SCAN THROUGH A MULTIPLE OCCURENCE TABLE (MOT)
C
C  RM_Parameters
C    INPUT:  MOTID---ID FOR THIS WORD
C    OUTPUT: MOTID---ID FOR MOT WORD NEXT TIME OR 0
C                    (0 IMPLIES THIS IS THE LAST VALUE)
C            IPTR----USER POINTER DESIRED
C
C  DECLARATIVES
         INCLUDE 'btbuf.inc'
C
C  CHECK FOR END OF MOT LIST.
C
  100    CONTINUE
         IF(MOTID.EQ.0) RETURN
C
C  GET THE MOT BLOCK THAT IS NEEDED.
C
         CALL ITOH(MOTIND,MOTIDP,MOTID)
         CALL BTGET(MOTIDP,IN)
         IND = 3 * IN - 3
         MOTIND = MOTIND + IND
C
C  RETRIEVE THE NEEDED WORD.
C
         MOTID = CORE(MOTIND)
         IPTR = CORE(MOTIND+1)
         IF(IPTR.EQ.0) GO TO 100
C
C  RETURN WITH THE VALUES.
C
         RETURN
      END
