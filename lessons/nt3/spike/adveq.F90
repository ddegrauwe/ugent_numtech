PROGRAM ADVEQ

!=================================
! 1-Dimensional Advection Equation
!=================================

USE FFT_MOD

IMPLICIT NONE

! Declarations
REAL    :: DX, DT, C      ! grid distance, time output_spike, advection speed
INTEGER :: NX, NT         ! number of grid cells, number of time steps
INTEGER :: NEXP           ! number of experiments
REAL, ALLOCATABLE    :: PHI_GP(:,:)
COMPLEX, ALLOCATABLE :: PHI_SP(:,:)
REAL, ALLOCATABLE    :: K(:)

COMPLEX, PARAMETER :: II=(0.,1.)
REAL, PARAMETER    :: PI=ACOS(-1.)

COMPLEX, ALLOCATABLE :: OMEGA(:,:)

! Auxiliary variables
INTEGER :: IX, IT, IEXP

! Always start with a warm welcome
WRITE (*,*)
WRITE (*,*) '*** 1D Advection Equation ***'
WRITE (*,*)

! Parameter values
DX = 1.0
DT = 1.0
NX = 50   ! must be factorizable into 2, 3 and 5
NT = 10
C  = 1
NEXP=5    ! exact + 1st-4th order

! Allocate memory
ALLOCATE(K(-NX/2:NX/2),PHI_SP(-NX/2:NX/2,NEXP),PHI_GP(NX,NEXP))
ALLOCATE(OMEGA(-NX/2:NX/2,NEXP))

! Define wavenumbers and 
DO IX=-NX/2,NX/2
  K(IX)=IX*2*PI/(NX*DX)
ENDDO

! dispersion
OMEGA(:,1)=C*K
! 1st order decentered
OMEGA(:,2)=C/DX*(SIN(K*DX)+II*(COS(K*DX)-1.))
! 2nd order centered
OMEGA(:,3)=C*SIN(K*DX)/DX
! 3rd order decentered
OMEGA(:,4)=C/DX*(4*SIN(K*DX)/3.-SIN(2*K*DX)/6.-II*(1-COS(K*DX))**2/3.)
! 4th order centered
OMEGA(:,5)=C/DX*(4*SIN(K*DX)/3.-SIN(2*K*DX)/6.)

! Initial conditions
! spike
PHI_GP=0.
PHI_GP(NX/2,:)=1.

! transform to spectral space
DO IEXP=1,NEXP
  CALL FFT(PHI_GP(:,IEXP),PHI_SP(:,IEXP))
ENDDO

! open output file
OPEN(FILE='output.dat',UNIT=77)
! write general info
WRITE (77,*) NX, DX, NT, DT, NEXP
! write experiment names
WRITE (77,*) '"exact" "1st order" "2nd order" "3rd order" "4th order"'

! write initial state
CALL WRITEOUTPUT(NX,NEXP,PHI_SP)

! Time loop (with exact time integration)
DO IT=1,NT
  ! Monitor progress
  WRITE (*,'(A,1X,A,F9.2,A)',ADVANCE='NO') ACHAR(13),'Performing calculation ... ',IT*100./NT,' %'
  
  ! time evolution
  PHI_SP=EXP(-II*OMEGA*DT)*PHI_SP

  ! Write output to file
  CALL WRITEOUTPUT(NX,NEXP,PHI_SP)

ENDDO ! end of time loop
WRITE (*,*)

! Close output file
CLOSE(UNIT=77)

DEALLOCATE(K,PHI_GP,PHI_SP,OMEGA)

WRITE (*,*) 'Finished'
WRITE (*,*)

END PROGRAM ADVEQ

SUBROUTINE WRITEOUTPUT(NX,NEXP,PHI_SP)

! write output to file

USE FFT_MOD

IMPLICIT NONE

! arguments
INTEGER, INTENT(IN) :: NX, NEXP
COMPLEX, INTENT(IN) :: PHI_SP(-NX/2:NX/2,NEXP)
REAL :: PHI_GP(NX,NEXP)

! auxiliary variables
INTEGER :: IEXP
CHARACTER(LEN=32) :: FRMT

WRITE (FRMT,'(A,I0,A)') '(',NX,'F16.8)'

! inverse transform
DO IEXP=1,NEXP
  CALL IFFT(PHI_SP(:,IEXP),PHI_GP(:,IEXP))
ENDDO

! write to file
WRITE (UNIT=77,FMT=FRMT) PHI_GP

END SUBROUTINE WRITEOUTPUT
