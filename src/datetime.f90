MODULE DateTime
  implicit none
  private

  public RMTIME
  public RMDATE

  contains 

    FUNCTION RMTIME()
      INTEGER RMTIME 
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      !  PURPOSE:   RETURN THE CURRENT TIME AS INTEGER (SEC FROM MIDNIGHT)
      !
      !  PARAMETERS:
      !     TIM-----THE CURRENT TIME
      !
      integer :: t(3)

      ! itime is a GNU Fortran instrinsic, a portable definition is in time_date.c
      CALL itime(T)
      RMTIME = t(1)*3600 + t(2)*60 + t(3)
      RETURN
    END FUNCTION RMTIME


    FUNCTION RMDATE()
      INTEGER RMDATE
      !
      ! ***UNIX SYSTEM DEPENDENT ROUTINE ***
      !
      !  PURPOSE:   RETURN THE CURRENT DATE AS INTEGER
      !
      !  PARAMETERS:
      !     DAT-----THE CURRENT DATE
      !
      integer :: d(3)
      
      ! idate is a GNU Fortran instrinsic, a portable definition is in time_date.c
      CALL idate(d)
      CALL JULDAT(d(1),d(2),d(3),RMDATE)
      RETURN
    END FUNCTION RMDATE

END MODULE DateTime
