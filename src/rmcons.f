      SUBROUTINE RMCONS
C
C     INITIALIZATION OF CONSTANTS
C     CALLED AT RIM STARTUP
C
C     MANY OF THESE ARE SYSTEM OR INSTALLATION DEPENDENT
C
C------------------------------------------------------------
C

         USE Parameters
         USE Globals, only: KMSSVL, KMSSVT, KNAPVL, KNAPVT, USERID
         USE Globals, only: ARBCHS, ARBCHM, USERID
         USE Globals, only : Globals_Initialise => Initialise
         USE DateTime, only : DateTime_Initialise, DTFENC
         USE Text, only : Text_Initialise, ASCTXT, ASCCHR, NONE
         USE Utils, only : ZMOVE

         INCLUDE 'ascpar.inc'
         INCLUDE 'files.inc'
         INCLUDE 'cards.inc'
         INCLUDE 'msgcom.inc'
         INCLUDE 'rmatts.inc'
         INCLUDE 'prom.inc'
         INCLUDE 'maccom.inc'
C
C     /ASCPAR/
         CALL Text_Initialise
C

         CALL ASCTXT(KDBHDR,ZC,'RIM DATABASE')
         CALL Globals_Initialise

         KMSSVL = 4
         CALL ASCTXT(KMSSVT,ZC,'-MV-')
         KNAPVL = 4
         CALL ASCTXT(KNAPVT,ZC,'-NA-')
         CALL ZMOVE(USERID,NONE)
         ARBCHS = ASCCHR('?')
         ARBCHM = ASCCHR('*')
C
         CALL DateTime_Initialise
C
C     HELP TEXT NAMES
         CALL ASCTXT(KZHPDB,ZC,'RIM_HELP')
         CALL ASCTXT(KZHPRL,ZC,'HELP_TEXT')
         CALL ASCTXT(KZHPKY,ZC,'COMKEY')
         CALL ASCTXT(KZHPSK,ZC,'SUBKEY')
         CALL ASCTXT(KZHPTX,ZC,'COMTXT')
C
C
C     /FILES/
         NINT = ZNINT
         NOUT = ZNOUT
         NOUTR = ZNOUT
         NOUTL = ZNOUT
         NOUTT = ZNOUT
         ECHO = .FALSE.
         CONNI = .TRUE.
         CONNO = .TRUE.
         BATCH = .FALSE.
         UTERML = 80
         UPRINL = 136
         ULPP = 0

C     /MSGCOM/
         MSUNIT = NOUT
         MSGPTR = 0

C     /CARDS/
         READCD = 0
         CRDIDX = 0
         DO 200 I = 1, ZCARDN
  200    CRDRLL(I) = 0
         LXEOC = 0

C     /LXLCOM/
         CALL LXINIT

C     /PROM/
         PRMPT = .TRUE.
         CALL PRMSET('INIT','RIM:')

C     /MACCOM/
         MACNUM = 0
         MACNTX = 1
         MACWPT = MACWPZ

         RETURN
      END
