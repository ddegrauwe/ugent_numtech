SUBROUTINE TIMESTEP(PHI9,PHI0,PHI1)
  
  ! calculates solution at next timestep (phi1) from solution at current timestep (phi0)
  
  USE CONSTANTS
  
  IMPLICIT NONE
  
  ! arguments
  REAL, INTENT(IN)  :: PHI9(NX)        ! solution at previous timestep
  REAL, INTENT(IN)  :: PHI0(NX)        ! solution at current timestep
  REAL, INTENT(OUT) :: PHI1(NX)        ! solution at next timestep
  
  ! local variables
  INTEGER :: IX                        ! space index
  REAL    :: MU
  
  ! CFL number
  MU=C*DT/DX
  
  ! upstream scheme
  DO IX=1,NX
    IF (IX>1 .AND. IX<NX) THEN
      PHI1(IX)=PHI9(IX)-MU*(PHI0(IX+1)-PHI0(IX-1))
    ELSEIF (IX==1) THEN
	  ! periodic boundary conditions
	  PHI1(IX)=PHI9(IX)-MU*(PHI0(IX+1)-PHI0(NX))
	ELSE
	  ! periodic boundary conditions
      PHI1(IX)=PHI9(IX)-MU*(PHI0(1)-PHI0(IX-1))
    ENDIF
  ENDDO
  
END SUBROUTINE TIMESTEP