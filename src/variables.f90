MODULE RM_Variables

   implicit none
   private

   public VARADD
   public LOCVAR

contains

   LOGICAL FUNCTION VARADD()
      !!
      !! ADD A VARIABLE TO THE VAR LIST
      !!
      !!
      USE RM_Parameters, only: Z
      USE Extern, only: MSG
      USE RM_BufferData, only: BUFFER
      USE RM_Text, only : FILCH, ABLANK
      USE Utils, only : ZMOVE, HTOI

      INCLUDE 'incore.inc'
      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'pgmcom.inc'
      !
      IF (PGVPTR+2+Z+ATTWDS.GT.PGVMAX) THEN
         CALL MSG('E','VARIABLE SPACE IS EXHAUSTED.',' ')
         VARADD = .FALSE.
         RETURN
      ENDIF

      IF (ATTWDS.LE.0) ATTWDS = 1
      CALL HTOI(ATTCHA,ATTWDS,ATTLEN)
      CALL ZMOVE(BUFFER(PGVPTR),ATTNAM)
      BUFFER(PGVPTR+Z) = ATTLEN
      BUFFER(PGVPTR+Z+1) = ATTYPE
      IF (ATTYPE.NE.4) THEN
         BUFFER(PGVPTR+Z+2) = 0
      ELSE
         CALL FILCH(BUFFER(PGVPTR+Z+2),1,ATTCHA,ABLANK)
      ENDIF
      PGVPTR = PGVPTR + Z + 2 + ATTWDS
      VARADD = .TRUE.
      RETURN
   END FUNCTION VARADD


   INTEGER FUNCTION LOCVAR(ANAME)
      !!
      !! LOOK FOR VARIABLE
      !!
      !!     ANAME---NAME OF VARIABLE
      !!     LOCVAR--STATUS VARIABLE - 0 MEANS OK, 1 MEANS NO WAY

      USE RM_Parameters
      USE RM_BufferData, only: BUFFER
      USE RM_Text, only : BLANK
      USE Utils, only : ZMOVE, ITOH

      ! INTEGER, intent(in) :: ANAME(*)

      INCLUDE 'tuplea.inc.f90'
      INCLUDE 'pgmcom.inc'
      LOGICAL :: EQ
      LOGICAL :: NE
      INCLUDE 'dclar1.inc'

      INTEGER :: I, X1, X2

      LOCVAR = 0
      !
      !  LOOK FOR THE VARIABLE
      !
      I = PGVBLK
      IF (I.LE.0) GOTO 9000

300   IF(I.GE.PGVPTR) GO TO 9000
      IF(EQ(BUFFER(I),ANAME)) GOTO 500

      CALL ITOH(X1,X2,BUFFER(I+Z))
      I = I + Z + 2 + X2
      GO TO 300
      !
      !  WE FOUND THE VARIABLE
      !
500   CONTINUE
      CALL ZMOVE(ATTNAM,ANAME)
      ATTLEN = BUFFER(I+Z)
      ATTYPE = BUFFER(I+Z+1)
      ATTCOL = I+Z+2
      CALL ITOH(ATTCHA,ATTWDS,ATTLEN)
      ATTFOR = 0
      GO TO 9999
      !
      !  UNABLE TO FIND WHAT WE ARE LOOKING FOR.
      !
9000  CONTINUE
      CALL ZMOVE(ATTNAM,BLANK)
      LOCVAR = 1
9999  CONTINUE
      RETURN
   END FUNCTION LOCVAR

END MODULE RM_Variables
