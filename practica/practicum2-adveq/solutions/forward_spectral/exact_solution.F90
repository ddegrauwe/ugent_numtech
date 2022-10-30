SUBROUTINE EXACT_SOLUTION(T,PSI)

! gives the exact solution at a time T
!
! it must be periodic over the domain!
!

USE CONSTANTS

IMPLICIT NONE

! arguments
REAL, INTENT(IN)  :: T              ! time
REAL, INTENT(OUT) :: PSI(NX)        ! exact solution

! local variables
INTEGER :: IX     ! spatial index
INTEGER :: KX     ! wavenumber counter
REAL    :: LX     ! domain length

! calculate the domain length
LX=NX*DX

! harmonic function
KX=5              ! wavenumber: number of oscillations over the domain
DO IX=1,NX
  PSI(IX)=COS(2*PI*KX*(IX*DX-C*T)/LX)
ENDDO

! spike
!DO IX=1,NX
!  PSI(IX)=MAX(0.,1.-ABS(MODULO(IX*DX-C*T,LX)-LX/2)/DX)
!ENDDO

END SUBROUTINE EXACT_SOLUTION
