SUBROUTINE TIMELOOP

  USE CONSTANTS

  IMPLICIT NONE

  ! declare local variables
  INTEGER              :: IT               ! current timestep
  COMPLEX, ALLOCATABLE :: PSI(:), PHI(:)   ! arrays of exact and numerical solution

  ! allocate memory for arrays
  ALLOCATE(PSI(0:NT),PHI(0:NT))

  ! set initial conditions
  IT=0
  PSI(IT)=PSI0
  PHI(IT)=PSI0

  ! start loop over timesteps
  DO IT = 1,NT

    ! exact solution
    PSI(IT) = EXP(II*KAPPA*DT)*PSI(IT-1)

    ! numerical solution: backward scheme
	PHI(IT) = 1./(1.-II*KAPPA*DT)*PHI(IT-1)

  ENDDO
  
  ! write the result
  CALL WRITE_RESULT(PSI,PHI)

END SUBROUTINE TIMELOOP

