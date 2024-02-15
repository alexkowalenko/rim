      SUBROUTINE LNKADD

         USE Files, only : LF1REC
         USE RM_Globals, only : IFMOD
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     ADD A NEW LINK ROW
C
         INCLUDE 'tuplel.inc'
         INCLUDE 'lnktbl.inc'
C
C  GET THE PAGE FOR ADDING NEW TUPLES.
C
         MRSTRT = NLROW
         IF (MRSTRT.EQ.0) THEN
            CALL MSG('E','MUST RELOAD DB TO USE LINKS',' ')
            RETURN
         ENDIF
         CALL LNKPAG(MRSTRT)
         I = MRSTRT
         NLROW = NLROW + 1
         IF(I.EQ.LPBUF) NLROW = (LPBUF * LF1REC) + 1
C
C  MOVE THE DATA FROM THE TUPLE TO THE BUFFER.
C
         LNKTBL(ZL1,I) = NLROW
         CALL ZMOVE(LNKTBL(ZL2,I),LNAME)
         CALL ZMOVE(LNKTBL(ZL3,I),R1NAME)
         CALL ZMOVE(LNKTBL(ZL4,I),A1NAME)
         CALL ZMOVE(LNKTBL(ZL5,I),R2NAME)
         CALL ZMOVE(LNKTBL(ZL6,I),A2NAME)
         LNKMOD = 1
         IFMOD = .TRUE.
         LLROW = 0
         IF(I.LT.LPBUF) RETURN
C
C  WE JUST FILLED A BUFFER. MAKE SURE LNKTBL GETS THE NEXT ONE.
C
         LNKBUF(1) = NLROW
         MRSTRT = NLROW
         CALL LNKPAG(MRSTRT)
         RETURN
      END
