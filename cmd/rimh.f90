PROGRAM RIMH

   USE RM_Parameters
   USE RM_Globals, only : RMSTAT
   USE RM_Text, only : STRASC

   IMPLICIT NONE

   !
   ! GENERATE THE HELP DATABASE
   !
   ! (UNIX version to open correct files)
   !
   !
   !  THE HELPDB SCHEMA IS DEFINED AS FOLLOWS:
   !
   !  DEFINE RIM_HELP           *(UWRIM HELP DATABASE)
   !  OWNER 'RIM'
   !  ATTRIBUTES
   !  COMKEY    TEXT          3   *(COMMAND KEYWORD)
   !  SUBKEY    TEXT          3   *(SUB-COMMAND KEYWORD)
   !  COMTXT    TEXT       VAR    *(TEXT LINE OF HELP TEXT)
   !
   !  *(THERE IS NO EXPLICIT SEQUENCING FOR THE TEXT RECORDS
   !    BELONGING TO A SPECIFIC COMMAND AND SUB-COMMAND.
   !    THEY ARE RETRIEVED AS THEY WERE LOADED.)
   !
   !  RELATIONS
   !  HELP_TEXT     WITH COMKEY SUBKEY COMTXT
   !  END
   !
   !
   !  INPUT RECORDS HAVE A COMMAND CHARACTER IN COLUMN 1 AND
   !  TEXT IN COLS 2 - 80
   !
   ! '*' = COMMENT
   ! '.' = BEGIN NEW COMMAND OR SUBCOMMAND
   ! '&' = BEGIN MACRO DEFINITION
   ! '=' = COPY MACRO TEXT
   !
   INCLUDE 'dclar1.inc'
   LOGICAL :: RIM
   INTEGER :: I, MACIDX, PTR, SUB
   INTEGER :: KEY(1)
   !
   INTEGER, PARAMETER :: NW = 80/ZCW
   COMMON /LINE/ UNIT, LINE(NW), LINEL, EOF
   INTEGER :: UNIT, LINE, LINEL, EOF
   CHARACTER(len=1) :: COM
   CHARACTER(len=4) :: COMK, SUBK

   ! DATA TO HOLD THE TEXT TUPLES
   INTEGER, PARAMETER :: TPL=3+NW+2
   INTEGER, PARAMETER :: TXTSRT=4
   COMMON /TUP/ TUPLE(TPL)
   INTEGER :: TUPLE
   !
   ! MACRO TABLE
   INTEGER, PARAMETER :: MACMXL=300,MACMRL=NW
   INTEGER :: MACKEY(MACMXL), MACBUF(MACMRL,MACMXL), MACLEN(MACMXL)
   !
   open (2,file='rim_help.data',status='old',iostat=RMSTAT)


   IF (.NOT.RIM(1,'OPEN rim_help')) THEN
      WRITE(6,1000) RMSTAT
1000  FORMAT(' OPEN ERROR:',I5)
      GOTO 900
   ENDIF
   IF (.NOT.RIM(1,'LOAD HELP_TEXT')) THEN
      WRITE(6,1100) RMSTAT
1100  FORMAT(' FIND ERROR:',I5)
      GOTO 900
   ENDIF

   UNIT = 2
   TUPLE(3) = TXTSRT
   TUPLE(TXTSRT+1) = 0
   MACIDX = 0
   !
10 IF (GETLIN(COM).NE.0) GOTO 800
20 IF (LINEL.EQ.0) GOTO 10
   IF (COM.EQ.'*') GOTO 10
   IF (COM.EQ.'.') GOTO 100
   IF (COM.EQ.'&') GOTO 200
   IF (COM.EQ.'=') GOTO 300
   GOTO 10
   !
   ! START NEW COMMAND
   !
100 TUPLE(1) = KEY3(TUPLE(2))
   CALL STRASC(COMK,TUPLE(1),4)
   CALL STRASC(SUBK,TUPLE(2),4)
   WRITE(6,1200) COMK, SUBK
1200 FORMAT (' COMMAND: ',A4,1X,A4)
   !
   ! FIRST ROW IS THE COMMAND
   !
110 CALL LODROW(LINE,LINEL)
   IF (GETLIN(COM).NE.0) GOTO 900
   IF (COM.NE.' ') GOTO 20
   GOTO 110
   !
   ! READ MACRO TEXT
   !
200 KEY = KEY3(SUB)
   CALL STRASC(COMK,KEY,4)
   WRITE(6,1300) COMK
1300 FORMAT (' MACRO: ',A4)
210 IF (GETLIN(COM).NE.0) GOTO 900
   IF (COM.NE.' ') GOTO 20
   MACIDX = MACIDX + 1
   IF (MACIDX.GT.MACMXL) THEN
      WRITE(6,1400)
1400  FORMAT ('TOO MANY MACRO TABLE ENTRIES')
      GOTO 900
   ENDIF
   MACKEY(MACIDX) = KEY(1)
   DO I = 1, NW
      MACBUF(I,MACIDX) = LINE(I)
   END DO
   MACLEN(MACIDX) = LINEL
   GOTO 210
   !
   ! COPY MACRO TEXT
   !
300 KEY = KEY3(SUB)
   DO I = 1, MACIDX
      PTR = I
      IF (MACKEY(I).EQ.KEY(1)) GOTO 320
   END DO
   CALL STRASC(COMK,KEY,4)
   WRITE(6,1500) COMK
1500 FORMAT(' MACRO NOT FOUND: ',A4)
   GOTO 10
   !
320 CALL LODROW(MACBUF(1,PTR),MACLEN(PTR))
   PTR = PTR + 1
   IF (PTR.GT.MACIDX) GOTO 10
   IF (MACKEY(PTR).NE.KEY(1)) GOTO 10
   GOTO 320
   !
   ! AT END - BUILD KEY FOR COMKEY
   !
800 WRITE(6,1800)
1800 FORMAT(' BUILDING KEY FOR COMKEY')
   IF (RIM(1,'BUILD KEY FOR COMKEY IN HELP_TEXT')) GOTO 900
   WRITE(6,1810) RMSTAT
1810 FORMAT(' ERROR: ',I5)
   !
900 IF (RIM(1,'CLOSE')) CALL EXIT(1)
   STOP

CONTAINS

   FUNCTION GETLIN(COM)

      USE RM_Text, only : ASCCHR, BLANK

      INCLUDE 'syspar.inc'
      !
      ! GET A LINE OF TEXT
      !
      CHARACTER*1 COM

      COMMON /LINE/ UNIT, LINE(NW), LINEL, EOF
      CHARACTER(len=80) :: CLINE
      !
      GETLIN = 0
      READ (2,10,END=900)CLINE
10    FORMAT(A80)
      DO I = 80,1,-1
         LINEL = I
         IF (CLINE(I:I).NE.' ') GOTO 110
100      CONTINUE
      END DO
110   COM = CLINE(1:1)
      DO I = 1,NW
         LINE(I) = BLANK(1)
200      CONTINUE
      END DO
      DO I = 2,LINEL
         CALL PUTT(LINE,I,ASCCHR(CLINE(I:I)))
300      CONTINUE
      END DO
      RETURN
      !
900   GETLIN = 1
      WRITE(6,1000)
1000  FORMAT(' EOF ON UNIT 2')
      RETURN
   END FUNCTION GETLIN


   FUNCTION KEY3(SUB)
      USE RM_Text, only : UPCASE, BLANK, ABLANK

      INCLUDE 'syspar.inc'
      !
      ! GET FIRST THREE CHARS OF FIRST WORD
      ! GET FIRST THREE CHARS OF SECOND WORD TO SUB
      !

      COMMON /LINE/ UNIT, LINE(NW), LINEL, EOF
      !
      KEY3 = BLANK(1)
      SUB  = BLANK(1)

      WN = 1
      CN = 1
      DO I = 2, LINEL
         CALL GETT(LINE,I,CH)
         IF (CH.EQ.ABLANK) THEN
            IF (CN.NE.1) WN = WN + 1
            CN = 1
            GOTO 100
         ENDIF

         IF (CN.LE.3) THEN
            IF (WN.EQ.1) CALL PUTT(KEY3,CN,UPCASE(CH))
            IF (WN.EQ.2) CALL PUTT(SUB ,CN,UPCASE(CH))
            CN = CN + 1
         ENDIF
100      CONTINUE
      END DO
900   RETURN
   END FUNCTION KEY3


   SUBROUTINE LODROW(LINE,NC)
      USE RM_Text, only : ABLANK
      !
      ! WRITE A LINE TO DB
      !
      LOGICAL :: RIMDM
      !
      INTEGER, intent(in) :: LINE(NW)
      INTEGER, intent(in) :: NC

      ! DATA TO HOLD THE TEXT TUPLES
      INTEGER, PARAMETER :: TPL=3+NW+2
      INTEGER, PARAMETER :: TXTSRT=4
      COMMON /TUP/ TUPLE(TPL)
      INTEGER :: TUPLE

      DO I = 1, NW
         TUPLE(TXTSRT+1+I) = LINE(I)
      END DO
      CALL PUTT(TUPLE(TXTSRT+1+I),1,ABLANK)
      TUPLE(TXTSRT) = NC
130   IF (.NOT.RIMDM(1,'LOAD',TUPLE)) THEN
         WRITE(6,1000) NC, RMSTAT
1000     FORMAT(' LOAD ERROR: ',2I5)
      ENDIF
      RETURN
   END SUBROUTINE LODROW

END PROGRAM RIMH
