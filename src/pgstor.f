      LOGICAL FUNCTION PGSTOR(PGM,LPGM)

         USE RM_Globals, only : RMSTAT
         USE RM_Blocks, only: BLKCHG
         USE RM_BufferData, only: BUFFER

         INCLUDE 'syspar.inc'
C
C     STORE PART OF A PROGRAM
C
C     INPUT:   PGM-----PROGRAM TO STORE
C              LPGM----LENGTH OF PGM
C              IF LPGM<0 THEN ALLOCATE ENSURE SPACE ONLY
C
         INCLUDE 'pgmcom.inc'
C
         PGSTOR = .FALSE.
  100    IF (PGPPTR+IABS(LPGM).GT.PGPMAX) THEN
            PGPMAX = PGPMAX + 1000
            CALL BLKCHG(6,PGPMAX,1)
            IF (RMSTAT.NE.0) RETURN
            GOTO 100
         ENDIF
         IF (LPGM.GT.0) THEN
            CALL BLKMOV(BUFFER(PGPPTR),PGM,LPGM)
            PGPPTR = PGPPTR + LPGM
         ENDIF
         PGSTOR = .TRUE.
         RETURN
      END
