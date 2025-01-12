!  ATTRIBUTE TYPES
!
!
!     MATVEC TYPES
!
!       KZSCA  - SCALER
!       KZVEC  - VECTOR
!       KZMAT  - MATRIX
!
      INTEGER, PARAMETER :: KZSCA=0, KZVEC=1, KZMAT=2
!
!
!     SCALER ATTRIBUTES (VECTOR TYPE = KZSCA)
!
!       KZINT -- INTEGER
!       KZREAL - REAL
!       KZDOUB - REAL(real64)
!       KZTEXT - TEXT
!       KZDATE - DATE
!       KZTIME - TIME
!       KZPROC - PROCEDURE  (USED ONLY IN REPORT WRITER )
!
      INTEGER, PARAMETER :: KZINT=1, KZREAL=2, KZDOUB=3, KZTEXT=4
      INTEGER, PARAMETER :: KZDATE=5, KZTIME=6
      INTEGER, PARAMETER :: KZPROC=9
!
!     VECTOR ATTRIBUTES (VECTOR TYPE = KZVEC)
!
!       KZIVEC - INTEGER VECTOR
!       KZRVEC - REAL VECTOR
!       KZDVEC - REAL(real64) VECTOR
!
      INTEGER, PARAMETER :: KZIVEC=KZVEC*ZHALF+KZINT
      INTEGER, PARAMETER :: KZRVEC=KZVEC*ZHALF+KZREAL
      INTEGER, PARAMETER :: KZDVEC=KZVEC*ZHALF+KZDOUB
!
!     MATRIX ATTRIBUTES (VECTOR TYPE = KZMAT)
!
!       KZIMAT - INTEGER MATRIX
!       KZRMAT - REAL MATRIX
!       KZDMAT - REAL(real64) MATRIX
!
      INTEGER, PARAMETER :: KZIMAT=KZMAT*ZHALF+KZINT
      INTEGER, PARAMETER :: KZRMAT=KZMAT*ZHALF+KZREAL
      INTEGER, PARAMETER :: KZDMAT=KZMAT*ZHALF+KZDOUB
