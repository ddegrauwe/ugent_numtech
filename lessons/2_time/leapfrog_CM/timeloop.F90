SUBROUTINE TIMELOOP
  
  ! perform loop over timesteps
  !
  
  USE CONSTANTS
  
  IMPLICIT NONE
  
  ! declare local variables
  INTEGER :: IT           ! timestep
  REAL    :: PSI(NX)      ! exact solution
  REAL    :: PHI9(NX)     ! solution at previous timestep
  REAL    :: PHI0(NX)     ! solution at current timestep
  REAL    :: PHI1(NX)     ! solution at next timestep
  
  ! initial condition
  IT=0
  CALL EXACT_SOLUTION(IT*DT,PSI)
  PHI0=PSI
  CALL WRITE_RESULT(PSI,PHI0)
  
  ! solution at IT=-1 to initialize leapfrog
  IT=-1
  CALL EXACT_SOLUTION(IT*DT,PHI9)
  ! suppose we make a mistake
  PHI9=PHI9+0.1
  
  ! actual time loop
  DO IT=1,NT
    ! exact solution
    CALL EXACT_SOLUTION(IT*DT,PSI)
    
    ! numerical solution
    CALL TIMESTEP(PHI9,PHI0,PHI1)
    
    ! swap results
    PHI9=PHI0
	PHI0=PHI1
    
    ! write result
    CALL WRITE_RESULT(PSI,PHI0)
    
    ! check maximum value to detect instability
    WRITE (*,*) 'IT = ',IT,'; PHI_MAX = ',MAXVAL(ABS(PHI0))
    IF ( MAXVAL(ABS(PHI0)) > 10*MAXVAL(ABS(PSI)) ) THEN
      WRITE (*,*) 'Instability detected! -- aborting'
      EXIT
    ENDIF
    
  ENDDO
  
END SUBROUTINE TIMELOOP
