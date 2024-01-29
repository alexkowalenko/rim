MODULE Globals

   USE Parameters, only : Z, ZFNAML

   implicit none
   private

   !  *** / F L A G S / ***
   !
   !   MISC. FLAG AND CONTROL VARIABLES
   !
   INTEGER, public :: DBNAME(Z), USERID(Z), OWNER(Z), DBDATE, DBTIME
   INTEGER, public :: KDBVER
   INTEGER, public :: TRACE, ARBCHS, ARBCHM
   INTEGER, public :: KRMDTF, KRMTMF, KRMINF, KRMRNF
   INTEGER, public :: KMSSVL, KMSSVT(Z), KNAPVL, KNAPVT(Z)
   INTEGER, public :: MRINDX, LIBFLG, PGVARS, INLINE
   INTEGER, public :: KRMRMF, KRMDMF

   LOGICAL, public :: DFLAG, DMFLAG, CASEIG, PGFLAG
   LOGICAL, public :: IFMOD
   REAL, public  :: TOL
   LOGICAL, public  :: PCENT, RUCK, PIFLAG

   !
   !  *** / R I M S T P / ***
   !
   !   A SEPERATE COMMON ALLOWS EASIER ACCESS BY FOREIGN LANGUAGES
   !
   INTEGER, public :: HXFLAG

   !
   !  VARIABLE DEFINITIONS:
   !         DFLAG---DEFINED DATA BASE FLAG
   !         DMFLAG--TRUE IF THE DATABASE MAY BE MODIFIED
   !         DBNAME--DATA BASE NAME
   !         USERID--CURRENT USER PASSWORD
   !         OWNER---PASSWORD FOR DATA BASE DEFINITION
   !         DBDATE--DATE THE DATA BASE WAS LAST CLOSED
   !         DBTIME--TIME THE DATA BASE WAS LAST CLOSED
   !         IFMOD---SET TO TRUE IFF DATA BASE HAS BEEN MODIFIED
   !         TOL-----TOLORANCE FOR REAL COMPARS
   !         PCENT---.TRUE. IF TOL IS PERCENT: .FALSE. IF ACTUAL VALUE
   !         RUCK----RULE CHECKING SWITCH - .TRUE. OR .FALSE.
   !         TRACE---FLAGS FOR TRACE OUTPUT
   !         CASEIG--TRUE TO IGNORE CASE DIFFERENCES IN STR MATCHES
   !         ARBCHS--SINGLE CHAR ARBITRARY CHAR FOR 'LIKE' MATCHES
   !         ARBCHM--MULTI- CHAR ARBITRARY CHAR FOR 'LIKE' MATCHES
   !         KRMDTF--DEFAULT DATE FORMAT
   !         KRMDTF--DEFAULT TIME FORMAT
   !         KRMINF--DEFAULT INTEGER FORMAT
   !         KRMRNF--DEFAULT REAL FORMAT
   !         KMSSVT--ASCII-TEXT FOR MISSING VALUES
   !         KMSSVL--LENGTH OF KMSSVT
   !         KNAPVT--ASCII-TEXT FOR NOT-APPLICABLE VALUES
   !         KNAPVL--LENGTH OF KNAPVT
   !         HXFLAG--HALT EXECUTION FLAG - SET ON TERMINAL INTERRUPTION
   !         PIFLAG--TRUE IF IN PROGRAMMABLE INTERFACE MODE
   !         MRINDX--MULTIPLE RELATION INDEX (FOR PI)
   !         LIBFLG--NON-ZERO IF CURRENT DATABASE IS A LIBRARY
   !         PGVARS--SPACE FOR PROGRAM VARIABLES
   !         PGFLAG--TRUE IF EXECUTING PROGRAM
   !         INLINE--INPUT LINE NUMBER IN CURRENT FILE
   !         KRMRMF--MAX PRECISION REAL FORMAT
   !         KRMDMF--MAX PRECISION DOUBLE FORMAT

   CHARACTER(len=ZFNAML), public :: DBFNAM
   !
   !     DBFNAM  - FILENAME OF ACTIVE DATABASE
   !

END MODULE Globals