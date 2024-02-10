!
!     *** TMPA ***
!
!     TEMPORARY ATTRIBUTE ARRAY / DURING RELATION DEFINITION
!
!           1    = ATTRIBUTE NAME (LENGTH = Z)
!           2    = TYPE
!           3    = LENGTH
!           4    = KEY
!           5    = FORMAT
!
      INTEGER, PARAMETER :: TMPA1=1,TMPA2=TMPA1+Z,TMPA3=TMPA2+1
      INTEGER, PARAMETER :: TMPA4=TMPA3+1
      INTEGER, PARAMETER :: TMPA5=TMPA4+1
      INTEGER, PARAMETER :: TMPAL=TMPA5
!
