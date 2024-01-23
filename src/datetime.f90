MODULE DateTime
  implicit none
  private

  public RMTIME
  public RMDATE
  public JULDAT
  public DATJUL

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
      LOGICAL X
      
      ! idate is a GNU Fortran instrinsic, a portable definition is in time_date.c
      CALL idate(d)
      X = JULDAT(d(1),d(2),d(3),RMDATE)
      RETURN
    END FUNCTION RMDATE

    
    LOGICAL FUNCTION JULDAT(DD,MM,YY,JUL)
      INTEGER, intent(in) :: DD, MM, YY
      INTEGER, intent(out) :: JUL
      !
      ! CONVERT D,M,Y TO JULIAN
      ! RETURN <TRUE> IF D,M,Y ARE VALID
      !
      ! FROM ACM-CA 199, BY R. TANTZEN
      !

      INTEGER :: D, M, Y, C, YA
      D = DD
      M = MM
      Y = YY
      IF (M.GT.2) THEN
        M = M - 3
      ELSE
        M = M + 9
        Y = Y - 1
      ENDIF
      C = Y/100
      YA = Y - 100*C
      JUL = (146097*C)/4 + (1461*YA)/4 + &
            (153*M + 2)/5 + D + 1721119

      ! TEST ARGS FOR VALIDITY

      CALL DATJUL(D,M,Y,JUL)
      IF (D.EQ.DD .AND. M.EQ.MM .AND. Y.EQ.YY) THEN
        JULDAT = .TRUE.
      ELSE
        JULDAT = .FALSE.
      ENDIF
      RETURN
    END FUNCTION JULDAT


    SUBROUTINE DATJUL(DD,MM,YY,JUL)
      INTEGER, intent(out) :: DD, MM, YY
      INTEGER, intent(in) :: JUL
      !
      ! CONVERT JULIAN TO D,M,Y
      !
      ! FROM ACM-CA 199, BY R. TANTZEN
      !

      INTEGER J, Y, M, D

      J = JUL
    
      J = J - 1721119
    
      Y = (4*J-1)/146097
      J = 4*J-1 - 146097*Y
      D = J/4
    
      J = (4*D+3)/1461
      D = 4*D+3 - 1461*J
      D = (D+4)/4
    
      M = (5*D-3)/153
      D = 5*D-3 - 153*M
      D = (D+5)/5
    
      Y = 100*Y + J
    
      IF (M.LT.10) THEN
         M = M + 3
      ELSE
         M = M - 9
         Y = Y + 1
      ENDIF
    
      DD = D
      MM = M
      YY = Y
      RETURN
    END SUBROUTINE DATJUL

END MODULE DateTime
