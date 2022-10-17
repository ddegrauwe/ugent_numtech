SUBROUTINE TIMELOOP

! perform time loop for shallow water equations

USE CONSTANTS

IMPLICIT NONE

! declare local variables
REAL, DIMENSION(:), ALLOCATABLE :: U0, H0       ! speed and height at current timestep
REAL, DIMENSION(:), ALLOCATABLE :: U9, H9       ! speed and height at previous timestep
REAL, DIMENSION(:), ALLOCATABLE :: U1, H1       ! speed and height at next timestep
REAL, DIMENSION(:), ALLOCATABLE :: DUDX0, DHDX0 ! derivatives at current timestep
INTEGER :: IT, IX   ! counters

! memory allocations
ALLOCATE(U0(NX),H0(NX),U1(NX),H1(NX),U9(NX),H9(NX))
ALLOCATE(DUDX0(NX),DHDX0(NX))

! initial conditions
DO IX=1,NX
  U0(IX) = 0.
  H0(IX) = 0.4*HM*EXP(-50.*(IX*DX/(NX*DX)-0.5)**2)           ! Bell-shape
ENDDO

! write result
CALL WRITE_RESULT(UM+U0,HM+H0)            ! Total height = HM + H0

! time loop: leapfrog scheme
DO IT=1, NT
  
  ! calculate derivatives
  CALL DERIVX(U0,DUDX0)
  CALL DERIVX(H0,DHDX0)
  
  IF (IT==1) THEN
    ! first timestep: forward scheme
    U1 = U0 - DT*( UM*DUDX0 +  G*DHDX0 )
    H1 = H0 - DT*( HM*DUDX0 + UM*DHDX0 )
  ELSE
    ! leapfrog
    U1 = U9 - 2.*DT*( UM*DUDX0 +  G*DHDX0 )
    H1 = H9 - 2.*DT*( HM*DUDX0 + UM*DHDX0 )
  ENDIF
  
  ! warning if values get too large
  IF (MAXVAL(ABS(H1)) > 10.) THEN
    WRITE (*,*) 'WARNING: instability detected!'
    EXIT
  ENDIF
  
  ! shift timesteps: t [0] becomes t-dt [9]; t+dt [1] becomes t [0]
  U9 = U0
  H9 = H0
  U0 = U1
  H0 = H1

  ! write output
  IF ( MODULO(IT,1) == 0 ) THEN
    CALL WRITE_RESULT(UM+U0,HM+H0)
  ENDIF
ENDDO

! release memory
DEALLOCATE(U0,H0,U1,H1,U9,H9)
DEALLOCATE(DUDX0,DHDX0)

END SUBROUTINE TIMELOOP
