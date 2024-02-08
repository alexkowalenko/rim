      SUBROUTINE GETLOC (INDPTR,NAMREL,LOCID)
            USE Globals, only : RMSTAT
      INCLUDE 'syspar.inc'
C     PURPOSE: GETS THE RSTART VALUE FOR THE INDICATED RELATION
C              EQUIVALENT TO A RMFIND FOR A RANDOM READ OF A RELATION
C
C     INPUT:   INDPTR --   INTERFACE POINTER NUMBER (1 THRU 5)
C              NAMREL -- RELATION NAME
C     OUTPUT:  LOCID --  LOCATION POINTER TO FIRST ROW OF THIS
C                        RELATION
C
C      ====>   RIC JOHNSTON ACC UW 85/01/04
C
C==================================================
      INCLUDE 'tupler.inc'
      CALL RMFIND(INDPTR,NAMREL)
      LOCID = 0
      IF(RMSTAT.NE.0)RETURN
      LOCID = RSTART
      RETURN
      END
