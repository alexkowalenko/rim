      SUBROUTINE ATTADD

         USE Globals, only : IFMOD
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C  PURPOSE:   ADD A NEW TUPLE TO THE ATTRIBUTE RELATION
C
         INCLUDE 'tuplea.inc'
         INCLUDE 'attble.inc'
         INCLUDE 'f1com.inc'
C
C  GET THE PAGE FOR ADDING NEW TUPLES.
C
         MRSTRT = NAROW
         CALL ATTPAG(MRSTRT)
         I = MRSTRT
         NAROW = NAROW + 1
         IF(I.EQ.APBUF) NAROW = (APBUF * LF1REC) + 1
C
C  MOVE THE DATA FROM THE TUPLE TO THE BUFFER.
C
         L = 1
         ATTBLE(1,I) = NAROW
         CALL ZMOVE(ATTBLE(ZA2,I),ATTNAM)
         CALL ZMOVE(ATTBLE(ZA3,I),RELNAM)
         ATTBLE(ZA4,I) = ATTCOL
         ATTBLE(ZA5,I) = ATTLEN
         ATTBLE(ZA6,I) = ATTYPE
         ATTBLE(ZA7,I) = ATTKEY
         ATTBLE(ZA8,I) = ATTFOR
         ATTMOD = 1
         IFMOD = .TRUE.
         CROW = 0
         LROW = 0
         IF(I.LT.APBUF) RETURN
C
C  WE JUST FILLED A BUFFER. MAKE SURE ATTBLE GETS THE NEXT ONE.
C
         ATTBUF(1) = NAROW
         MRSTRT = NAROW
         CALL ATTPAG(MRSTRT)
         RETURN
      END
