SUBROUTINE TIMELOOP

! perform time integration, i.e. loop over timesteps

USE CONSTANTS

IMPLICIT NONE

! local variables
INTEGER :: IT           ! timestep counter
REAL    :: PHI0(NX)     ! numerical solution at current timestep
REAL    :: PHI1(NX)     ! solution at next timestep
REAL    :: PSI(NX)      ! exact solution

! initial condition
CALL EXACT_SOLUTION(0.,PSI)
PHI0=PSI
CALL WRITE_RESULT(PSI,PHI0)

! time loop
DO IT=1,NT

  ! exact solution
  CALL EXACT_SOLUTION(IT*DT,PSI)
  
  ! numerical solution
  CALL TIMESTEP(PHI0,PHI1)
  
  ! swap
  PHI0=PHI1
  
  ! write result
  CALL WRITE_RESULT(PSI,PHI0)
  
  ! check for instability
  WRITE (*,*) 'IT = ',IT,'; PHI_MAX = ',MAXVAL(ABS(PHI0))
  IF ( MAXVAL(ABS(PHI0)) > 10*MAXVAL(ABS(PSI)) ) THEN
    WRITE (*,*) 'instability detected -- aborting'
    EXIT
  ENDIF
  
ENDDO

END SUBROUTINE TIMELOOP
