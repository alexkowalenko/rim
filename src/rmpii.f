      SUBROUTINE RMPII
      INCLUDE 'syspar.inc'
C
C     INITIALIZE THE PI DATA FOR CURRENT RELATION
C
C     INDCUR: MULTIPLE RELATION POSITION INDICATOR
C
C
      INCLUDE 'ascpar.inc'
      INCLUDE 'flags.inc'
      INCLUDE 'tupler.inc'
      INCLUDE 'tuplea.inc'
      INCLUDE 'vardat.inc'
      INCLUDE 'keydat.inc'
      INCLUDE 'rimcom.inc'
      INCLUDE 'rimptr.inc'
C
      LOGICAL EQ
C
      INCLUDE 'picom.inc'
C
C  SET PI VARS
C
      STAT2=0
      CID2=0
      NID2=0
      LEN2=0
      RST2=RSTART
      REN2=REND
C
C  CHECK FOR KEYED AND VARIABLE LENGTH ATTRIBUTES
C
      NUMVAR = 0
      NUMKEY = 0
      I = LOCATT(BLANK,NAME)
      DO 500 J=1,NATT
      CALL ATTGET(ISTATX)
      IF(ISTATX.NE.0) GO TO 999
      IF(ATTKEY.EQ.0) GO TO 400
       NUMKEY = NUMKEY + 1
       IF(NUMKEY.GT.ZPIKAT) GO TO 400
       KEYDAT(1,NUMKEY) = ATTKEY
       KEYDAT(2,NUMKEY) = ATTCOL
       KEYDAT(3,NUMKEY) = ATTWDS
       KEYDAT(4,NUMKEY) = ATTYPE
       CALL ZMOVE(KEYDAT(5,NUMKEY),ATTNAM)
C
400   IF(ATTWDS.NE.0) GO TO 500
      NUMVAR = NUMVAR + 1
      IF(NUMVAR.GT.ZPIVAT) GO TO 500
       POSVAR(1,NUMVAR) = ATTCOL
       POSVAR(2,NUMVAR) = ATTYPE
  500 CONTINUE
C
  999 CONTINUE
      STAT2=RMSTAT
      RETURN
      END
