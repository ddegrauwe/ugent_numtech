SUBROUTINE TIMELOOP

  USE CONSTANTS

  IMPLICIT NONE

  ! declare local variables
  INTEGER              :: IT               ! current timestep
  COMPLEX, ALLOCATABLE :: PSI(:), PHI(:), PHIF(:)   ! arrays of exact and numerical solution
  
  REAL :: GAMMA
  
  GAMMA=0.06

  ! allocate memory for arrays
  ALLOCATE(PSI(0:NT+1),PHI(0:NT+1),PHIF(0:NT+1))

  ! set initial conditions
  IT=0
  PSI(IT)=PSI0
  PHI(IT)=PSI0

  ! start loop over timesteps
  DO IT = 1,NT+1

    ! exact solution
    PSI(IT) = EXP(II*KAPPA*DT)*PSI(IT-1)

    ! numerical solution: leapfrog scheme
	IF (IT==1) THEN
	  ! ERROR during first timestep
      PHI(IT) = (1-II*KAPPA*DT)*PHI(IT-1)
	ELSE
	  ! leapfrog from second timestep on
	  PHI(IT)=PHIF(IT-2)+2*II*KAPPA*DT*PHI(IT-1)
	  ! filter
	  PHIF(IT-1) = PHI(IT-1) + GAMMA*(PHIF(IT-2)-2*PHI(IT-1)+PHI(IT))
	ENDIF

  ENDDO
  
  ! write the result
  CALL WRITE_RESULT(PSI(0:NT),PHIF(0:NT))

END SUBROUTINE TIMELOOP

