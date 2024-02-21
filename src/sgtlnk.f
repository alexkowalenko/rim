      LOGICAL FUNCTION SGTLNK(I,MP,LP)

         USE RM_Globals, only : RMSTAT
         USE RM_BufferData, only: BUFFER
         USE Extern, only: MSG
         USE RM_Links, only: LOCLNK
         USE RM_Relations, only: LOCREL
         USE Utils, only : ZMOVE

         INCLUDE 'syspar.inc'
C
C     GET A LINKED TUPLE USING INFO IN SELCOM
C
C     I = SELCOM INDEX
C     MP = POINTER TO MAIN TUPLE IN BUFFER
C     LP = POINTER TO LINKED TUPLE IN BUFFER
C     RETURNS TRUE IF LINKED TUPLE FOUND
C
         INCLUDE 'rmatts.inc'
         INCLUDE 'tupler.inc'
         INCLUDE 'tuplea.inc'
         INCLUDE 'tuplel.inc'
         INCLUDE 'rimptr.inc'
         INCLUDE 'selcom.inc'
         INCLUDE 'whcom.inc'

         INTEGER temp
C
         SGTLNK = .FALSE.

         IF (LNKFL(I).EQ.0) RETURN

C     GET LINK
         CALL ZMOVE(LNAME,SLNKNM(1,LNKFL(I)))
         IF (LOCLNK(LNAME).NE.0) THEN
            CALL MSG('E','SGTLNK: NO LINK ',' ')
            GOTO 900
         ENDIF

         CALL RMSAV(1)

C     SET UP WHERE CLAUSE
         CALL LOCATT(A1NAME,R1NAME)
         CALL ATTGET(STATUS)
         IF (STATUS.NE.0) THEN
            CALL MSG('E','LINK FR ATT NOT FOUND' ,' ')
            GOTO 800
         ENDIF
         DO 80 P = 1, ATTWDS
   80    WHRVAL(P) = BUFFER(MP+ATTCOL+P-2)
         WHRLEN(1) = ATTLEN

         temp = LOCREL(R2NAME)
         CALL LOCATT(A2NAME,R2NAME)
         CALL ATTGET(STATUS)
         IF (STATUS.NE.0) THEN
            CALL MSG('E','LINK TO ATT NOT FOUND' ,' ')
            GOTO 800
         ENDIF
         NBOO = 1
         BOO(1) = WHAND
         KATTP(1) = ATTCOL
         KATTL(1) = ATTLEN
         KATTY(1) = ATTYPE
         KOMTYP(1) = 2
         KOMPOS(1) = 1
         KOMLEN(1) = 1
         KOMPOT(1) = 1
         KSTRT = 0
         MAXTU = ALL9S
         LIMTU = ALL9S
         NS = 0
         KSTRT = ATTKEY
         IF(KSTRT.NE.0) NS = 2
   73    CALL RMLOOK(LP,2,1,NCOL)
         INDCUR = NULL
         IF (RMSTAT.NE.0) GOTO 800
         SGTLNK = .TRUE.
         GOTO 900
C
  800    CALL RMRES(1)
C
  900    RETURN
      END
