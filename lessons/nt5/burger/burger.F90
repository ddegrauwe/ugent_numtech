PROGRAM BURGER

!=================================
! Burger's equation
!=================================

IMPLICIT NONE

! Declarations
REAL    :: DX, DT      ! grid distance, time step
INTEGER :: NX, NT         ! number of grid cells, number of time steps
REAL, ALLOCATABLE :: PHI0(:,:), PHI1(:,:)     ! Solution at time t and t+dt
REAL, ALLOCATABLE :: Q1(:,:), Q2(:,:), Q3(:,:), Q4(:,:)   ! for Runge Kutta integration
REAL, PARAMETER :: PI=ACOS(-1.)
REAL :: L2(3)

! Auxiliary variables
INTEGER :: IX, IT

! Always start with a warm welcome
WRITE (*,*)
WRITE (*,*) '*** Burger''s equation ***'
WRITE (*,*)
WRITE (*,'(A6,3A16)') 'time','advective','flux','conservative'
WRITE (*,*)

! Parameter values
DX = 1.0
DT = 0.1
NX = 100
NT = 500

! open output file
OPEN(FILE='output.dat',UNIT=77)
! write general info
WRITE (77,*) NX, DX, NT, DT, 3
! write experiment names
WRITE (77,*) '"advective" "flux" "conservative"'

! Allocate memory
ALLOCATE(PHI0(NX,3),PHI1(NX,3))
ALLOCATE(Q1(NX,3),Q2(NX,3),Q3(NX,3),Q4(NX,3))

! Initial conditions
DO IX=1,NX
  PHI0(IX,:)=SIN(2*(IX-1)*PI/REAL(NX))
ENDDO

L2=SQRT(SUM(PHI0**2))

! Write initial conditions to file
CALL WRITEOUTPUT(NX,PHI0)

! Time integration with Runge-Kutta-4
DO IT=1,NT
  
  Q1=DT*RHS(NX,DX,PHI0)
  Q2=DT*RHS(NX,DX,PHI0+Q1/2)
  Q3=DT*RHS(NX,DX,PHI0+Q2/2)
  Q4=DT*RHS(NX,DX,PHI0+Q3)
  
  PHI1=PHI0+(Q1+2*Q2+2*Q3+Q4)/6
      
  ! Shift arrays
  PHI0=PHI1
  
  ! print norm
  !WRITE (*,*) 'dL2 = ',L2-SQRT(SUM(PHI0**2))
  IF (MOD(IT,10)==0) THEN
    L2(1)=SQRT(SUM(PHI0(:,1)**2))
    L2(2)=SQRT(SUM(PHI0(:,2)**2))
    L2(3)=SQRT(SUM(PHI0(:,3)**2))
    WRITE (*,'(I6,3E16.6)') IT,L2
  ENDIF
  
  !IF (MAXVAL(ABS(PHI0))>1.e2) EXIT
  
  ! Write output to file
  CALL WRITEOUTPUT(NX,PHI0)

ENDDO ! end of time loop
WRITE (*,*)

! Write parameters to file for plotting
CLOSE(UNIT=77)

WRITE (*,*) 'Finished'
WRITE (*,*)

CONTAINS

FUNCTION RHS(NX, DX, PHI)

IMPLICIT NONE

! input arguments
INTEGER, INTENT(IN) :: NX
REAL, INTENT(IN) :: DX
REAL, INTENT(IN)  :: PHI(NX,3)

! output arguments
REAL :: RHS(NX,3)

! local variables
INTEGER :: IX

DO IX=1,NX
  ! advective form
  RHS(IX,1)=-PHI(IX,1)*(PHI(MODULO(IX+0,NX)+1,1)-PHI(MODULO(IX-2,NX)+1,1))/(2*DX)
  ! flux-conservative form
  RHS(IX,2)=-(PHI(MODULO(IX+0,NX)+1,2)**2-PHI(MODULO(IX-2,NX)+1,2)**2)/(4*DX)
  ! l2-conservative combination
  RHS(IX,3)=-PHI(IX,3)*(PHI(MODULO(IX+0,NX)+1,3)-PHI(MODULO(IX-2,NX)+1,3))/(6*DX) &
   & -(PHI(MODULO(IX+0,NX)+1,3)**2-PHI(MODULO(IX-2,NX)+1,3)**2)/(6*DX)
ENDDO

END FUNCTION RHS

SUBROUTINE WRITEOUTPUT(NX,PHI)

! write output to file

IMPLICIT NONE

! arguments
INTEGER, INTENT(IN) :: NX
REAL, INTENT(IN) :: PHI(NX,3)

! auxiliary variables
CHARACTER(LEN=32) :: FRMT

WRITE (FRMT,'(A,I0,A)') '(',NX,'E30.12)'

! write to file
WRITE (UNIT=77,FMT=FRMT) PHI

END SUBROUTINE WRITEOUTPUT

END PROGRAM BURGER

