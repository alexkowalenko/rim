      SUBROUTINE RMVARC(CTYP,TUPVAL)

         USE RM_Globals, only : RMSTAT
         USE RM_Attributes, only: ATTGET, LOCATT
         USE RM_Text, only : BLANK
C
C     FORTRAN INTERFACE SUBROUTINE
C
         INCLUDE 'syspar.inc'
C
C  PURPOSE: THIS ROUTINE CHANGES THE VARIABLE LENGTH ATTRIBUTE
C           TUPLE HEADERS FROM INTERNAL TO USER REPRESENTATION
C           OR VISA VERSA.
C
C                             USER                    INTERNAL
C           TYPE        WORD1       WORD2       WORD1       WORD2
C           ----------  ----------  ----------  ----------  ----------
C           RM_Text        CHARACTERS  0           WORDS       CHARACTERS
C           INT         ITEMS       0           WORDS       1
C           REAL        ITEMS       0           WORDS       1
C           DOUBLE      ITEMS       0           WORDS       1
C           VECTORS     ITEMS       0           WORDS       1
C           MATRICES    ROWS        COLS        WORDS       ROWS
C
C  RM_Parameters:
C           CTYP-----CONVERSION TYPE - -1 = INTERNAL TO USER
C                                      +1 = USER TO INTERNAL
C           TUPVAL---ARRAY CONTAINING THE TUPLE VALUES
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'vardat.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
C
         INTEGER CTYP
         INTEGER TUPVAL(1)
C
C  IF THE NUMBER OF VARIABLE ATTRIBUTES EXCEEDS MAX WE HAVE TO USE
C  ATTGET ETC TO DO THE CONVERSION ----
C
         LOOP = NUMVAR
         IF(NUMVAR.GT.ZPIVAT) THEN
C       TOO MANY VARIABLE LENGTH ATTRIBUTES
            I = LOCATT(BLANK,NAME)
            LOOP = NATT
         ENDIF
C
C  GET THE VALUES FOR EACH VARIABLE LENGTH ATTRIBUTE
C
         DO 500 K=1,LOOP
            IF(NUMVAR.GT.ZPIVAT) THEN
               CALL ATTGET(ISTATX)
               IF(ISTATX.NE.0) GO TO 999
               IF(ATTWDS.NE.0) GO TO 500
               IP = TUPVAL(ATTCOL)
               ITYPE = ATTYPE
            ELSE
               IP = TUPVAL(POSVAR(1,K))
               ITYPE = POSVAR(2,K)
            ENDIF
C
            IF((IP.LT.1).OR.(IP.GT.MAXCOL)) GO TO 998
            IW1 = TUPVAL(IP)
            IW2 = TUPVAL(IP+1)
            IF(CTYP.LT.0) GO TO 400
C
C  USER TO INTERNAL - RMPUT,RMLOAD
C
            IF(ITYPE.EQ.KZINT ) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZREAL) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZDOUB) TUPVAL(IP) = 2*IW1
            IF(ITYPE.EQ.KZTEXT) TUPVAL(IP) = (IW1-1)/ZCW + 1
            IF(ITYPE.EQ.KZIVEC) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZRVEC) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZDVEC) TUPVAL(IP) = 2*IW1
            IF(ITYPE.EQ.KZIMAT) TUPVAL(IP) = IW1*IW2
            IF(ITYPE.EQ.KZRMAT) TUPVAL(IP) = IW1*IW2
            IF(ITYPE.EQ.KZDMAT) TUPVAL(IP) = 2*IW1*IW2
            TUPVAL(IP+1) = 1
            IF(ITYPE.EQ.KZTEXT) TUPVAL(IP+1) = IW1
            IF(ITYPE.EQ.KZIMAT) TUPVAL(IP+1) = IW1
            IF(ITYPE.EQ.KZRMAT) TUPVAL(IP+1) = IW1
            IF(ITYPE.EQ.KZDMAT) TUPVAL(IP+1) = IW1
            IF((TUPVAL(IP).LT.1).OR.(TUPVAL(IP).GT.MAXCOL)) GO TO 998
            GO TO 500
C
C  INTERNAL TO USER - RMGET
C
  400       CONTINUE
            IF(ITYPE.EQ.KZINT ) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZREAL) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZDOUB) TUPVAL(IP) = IW1/2
            IF(ITYPE.EQ.KZTEXT) TUPVAL(IP) = IW2
            IF(ITYPE.EQ.KZIVEC) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZRVEC) TUPVAL(IP) = IW1
            IF(ITYPE.EQ.KZDVEC) TUPVAL(IP) = IW1/2
            IF(ITYPE.EQ.KZIMAT) TUPVAL(IP) = IW2
            IF(ITYPE.EQ.KZRMAT) TUPVAL(IP) = IW2
            IF(ITYPE.EQ.KZDMAT) TUPVAL(IP) = IW2
            TUPVAL(IP+1) = 0
            IF(ITYPE.EQ.KZIMAT) TUPVAL(IP+1) = IW1/IW2
            IF(ITYPE.EQ.KZRMAT) TUPVAL(IP+1) = IW1/IW2
            IF(ITYPE.EQ.KZDMAT) TUPVAL(IP+1) = (IW1/2)/IW2
  500    CONTINUE
         GO TO 999
C
  998    RMSTAT = 100
C
  999    CONTINUE
         RETURN
      END
