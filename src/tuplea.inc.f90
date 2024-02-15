!
!  *** / T U P L E A / ***
!
!  ONE TUPLE OF THE ATTRIBUTE RELATION
!
      COMMON /TUPLEA/ ATTNAM(Z),RELNAM(Z),ATTCOL,ATTLEN,ATTCHA,ATTWDS,ATTYPE,ATTKEY,ATTFOR
      INTEGER, PARAMETER :: ZTUPAL=2*Z+7

      INTEGER :: ATTNAM, RELNAM, ATTCOL, ATTLEN, ATTCHA, ATTWDS, ATTYPE,ATTKEY, ATTFOR
!
!  VARIABLE DEFINITIONS:
!         ATTNAM--NAME OF ATTRIBUTE
!         RELNAM--NAME OF RELATION
!         ATTCOL--STARTING COLUMN FOR ATTRIBUTE IN RELATION
!         ATTLEN--ATTRIBUTE LENGTH DATA - CALL ITOH(A,B,ATTLEN)
!                 TYPE    LENGTH   A        B
!                 ------  -------  -------  -------
!                 RM_Text    FIXED    NCHAR    NWORDS
!                 INT     FIXED    0        NWORDS
!                 REAL    FIXED    0        NWORDS
!                 DOUB    FIXED    0        NWORDS (2*ITEMS)
!                 IVEC    FIXED    ROWS     NWORDS
!                 RVEC    FIXED    ROWS     NWORDS
!                 DVEC    FIXED    ROWS     NWORDS (2*ITEMS)
!                 IMAT    FIXED    ROWS     NWORDS (ROWS*COLS)
!                 RMAT    FIXED    ROWS     NWORDS (ROWS*COLS)
!                 DMAT    FIXED    ROWS     NWORDS (2*ROWS*COLS)
!                 RM_Text    VAR      0        0
!                 INT     VAR      0        0
!                 REAL    VAR      0        0
!                 DOUB    VAR      0        0
!                 IVEC    VAR      0        0
!                 RVEC    VAR      0        0
!                 DVEC    VAR      0        0
!                 IMAT    FIX-VAR  ROWS     0
!                 RMAT    FIX-VAR  ROWS     0
!                 DMAT    FIX-VAR  ROWS     0
!                 IMAT    VAR-VAR  0        0
!                 RMAT    VAR-VAR  0        0
!                 DMAT    VAR-VAR  0        0
!         ATTCHA--THE "A" VALUE FROM ATTLEN
!         ATTWDS--THE "B" VALUE FROM ATTLEN
!         ATTYPE--VARIABLE TYPE (INT,REAL,RM_Text,DOUB,ETC.)
!         ATTKEY--0 FOR NON-KEY ATTRIBUTES
!                 BTREE START FOR KEY ATTRIBUTES
!         ATTFOR--FORMAT
