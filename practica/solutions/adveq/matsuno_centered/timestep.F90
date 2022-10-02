SUBROUTINE TIMESTEP(PHI0,PHI1)
  
  ! calculates solution at next timestep (phi1) from solution at current timestep (phi0)
  
  USE CONSTANTS
  
  IMPLICIT NONE
  
  ! arguments
  REAL, INTENT(IN)  :: PHI0(NX)        ! solution at current timestep
  REAL, INTENT(OUT) :: PHI1(NX)        ! solution at next timestep
  
  ! local variables
  REAL :: PHI_TILDE(NX)                ! first guess for solution at next timestep
  INTEGER :: IX                        ! space index
  
  ! STAGE 1: upstream scheme for phi_tilde
  DO IX=1,NX
    IF ( IX>1 .AND. IX<NX ) THEN
      PHI_TILDE(IX)=PHI0(IX)-C*DT/DX*(PHI0(IX+1)-PHI0(IX-1))/2.
    ELSEIF ( IX==1 ) THEN
      ! periodic boundary conditions at left boundary
      PHI_TILDE(1)=PHI0(1)-C*DT/DX*(PHI0(2)-PHI0(NX))/2.
    ELSE
      ! periodic boundary conditions at right boundary
      PHI_TILDE(NX)=PHI0(NX)-C*DT/DX*(PHI0(1)-PHI0(NX-1))/2.
    ENDIF
  ENDDO

  ! STAGE 2: backward scheme for phi1 : (phi1-phi0)/dt+c*d/dx(phi_tilde)=0
  DO IX=1,NX
    IF ( IX>1 .AND. IX<NX ) THEN
      PHI1(IX)=PHI0(IX)-C*DT/DX*(PHI_TILDE(IX+1)-PHI_TILDE(IX-1))/2.
    ELSEIF ( IX==1 ) THEN
      ! periodic boundary conditions at left boundary
      PHI1(1)=PHI0(1)-C*DT/DX*(PHI_TILDE(2)-PHI_TILDE(NX))/2.
    ELSE
      ! periodic boundary conditions at right boundary
      PHI1(NX)=PHI0(NX)-C*DT/DX*(PHI_TILDE(1)-PHI_TILDE(NX-1))/2.
    ENDIF
  ENDDO
  
END SUBROUTINE TIMESTEP
