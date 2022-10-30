SUBROUTINE TIMESTEP(PHI0,PHI1)
  
  ! calculates solution at next timestep (phi1) from solution at current timestep (phi0)
  
  USE CONSTANTS
  
  IMPLICIT NONE
  
  ! arguments
  REAL, INTENT(IN)  :: PHI0(NX)        ! solution at current timestep
  REAL, INTENT(OUT) :: PHI1(NX)        ! solution at next timestep
  
  ! local variables
  INTEGER :: IX                        ! space index
  
  ! upstream scheme
  DO IX=1,NX
    IF ( IX>1 .AND. IX<NX ) THEN
      PHI1(IX)=PHI0(IX)-C*DT/DX*(PHI0(IX+1)-PHI0(IX-1))/2.
    ELSEIF ( IX==1 ) THEN
      ! periodic boundary conditions at left boundary
      PHI1(1)=PHI0(1)-C*DT/DX*(PHI0(2)-PHI0(NX))/2.
    ELSE
      ! periodic boundary conditions at right boundary
      PHI1(NX)=PHI0(NX)-C*DT/DX*(PHI0(1)-PHI0(NX-1))/2.
    ENDIF
  ENDDO
  
END SUBROUTINE TIMESTEP
