SUBROUTINE EXACT_SOLUTION(T,PSI)
  
  ! gives the exact solution at a time T
  !
  ! it must be periodic over the domain!
  !
  
  USE CONSTANTS
  
  IMPLICIT NONE
  
  ! arguments
  REAL, INTENT(IN)  :: T
  REAL, INTENT(OUT) :: PSI(NX)
  
  ! local variables
  INTEGER :: IX      ! spatial index
  INTEGER :: KX      ! wavenumber
  REAL    :: LX      ! domain length
  
  ! calculate domain length
  LX = NX*DX

  ! harmonic function
  KX = 20             ! wavenumber = number of oscillations over the complete domain
  DO IX=1,NX
    PSI(IX) = COS(2*PI*KX*(IX*DX-C*T)/LX)
  ENDDO
  
  ! spike
  !DO IX=1,NX
  !  PSI(IX)=MAX(0.,1.-ABS(MODULO(IX*DX-C*T,LX)-LX/2)/DX)
  !ENDDO

  
END SUBROUTINE EXACT_SOLUTION
  
