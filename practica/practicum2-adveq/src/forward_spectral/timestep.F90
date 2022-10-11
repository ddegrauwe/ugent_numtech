SUBROUTINE TIMESTEP(PHI0,PHI1)

! calculates the next solution from the current solution
! with forward timestepping and spectral spatial discretization

USE CONSTANTS
USE FFT_MOD

IMPLICIT NONE

! arguments
REAL, INTENT(IN)  :: PHI0(NX)
REAL, INTENT(OUT) :: PHI1(NX)

! local variables
COMPLEX           :: PHIS(-NX/2:NX/2)   ! spectral field
INTEGER           :: IX                 ! index
REAL              :: KX                 ! wavenumber

! fourier transform
CALL FFT(PHI0,PHIS)

! forward scheme in spectral space for each wave separately
DO IX=-NX/2,NX/2
  ! wavenumber, scaled to (0,pi/dx)
  KX=2*PI*IX/(NX*DX)
  ! time evolution with forward scheme
  PHIS(IX)=(1-II*C*KX*DT)*PHIS(IX)
ENDDO

! inverse transform
CALL IFFT(PHIS,PHI1)

END SUBROUTINE TIMESTEP
