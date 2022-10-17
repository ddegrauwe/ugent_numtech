PROGRAM DIFFEQ

!
! Diffusion equation:
!
! d(phi)/dt + K d^2(phi)/dx^2 = 0
!
! compile with
!   gfortran -O2 -fdefault-real-8 -o diffeq diffeq.F90
!

! safeguard
IMPLICIT NONE

! grid constants
INTEGER :: IX, IT, NX, NT
REAL    :: DX, DT
REAL    :: K
REAL, PARAMETER    :: PI=ACOS(-1.)

! solution
REAL, ALLOCATABLE :: PHI(:,:)

! other stuff
CHARACTER(LEN=128) :: MYFMT

! set parameters
NT = 1000
NX = 128
DT = 0.1
DX = 1.
K  = 1.

! Always start with a warm welcome
WRITE (*,*) 'Welcome to the DIFFEQ solver'

! allocate arrays
ALLOCATE(PHI(NX,0:NT))

! initial condition: just some harmonic function
DO IX=1,NX
  PHI(IX,0)=COS((2*PI*IX)/NX)+0.5*SIN(2*(2*PI*IX)/NX)-0.25*COS(6*(2*PI*IX)/NX)
ENDDO

! integrate with forward scheme
DO IT=1,NT
  DO IX=1,NX
    IF (IX==1) THEN
	  PHI(IX,IT)=PHI(IX,IT-1)+K*DT*(PHI(IX+1,IT-1)-2*PHI(IX,IT-1)+PHI(NX,IT-1))/DX**2
	ELSEIF (IX==NX) THEN
      PHI(IX,IT)=PHI(IX,IT-1)+K*DT*(PHI(1,IT-1)-2*PHI(IX,IT-1)+PHI(IX-1,IT-1))/DX**2
	ELSE
	  PHI(IX,IT)=PHI(IX,IT-1)+K*DT*(PHI(IX+1,IT-1)-2*PHI(IX,IT-1)+PHI(IX-1,IT-1))/DX**2
	ENDIF
  ENDDO
ENDDO

! write result
OPEN(FILE='output.dat',UNIT=7)
WRITE (7,*) NX,DX,NT,DT,1
WRITE (7,*) '"field"'
WRITE (MYFMT,'(A,I4,A)') '(',NX,'E16.8)'
WRITE (7,FMT=MYFMT) PHI
CLOSE(UNIT=7)

! deallocate
DEALLOCATE(PHI)

! that's all
WRITE (*,*) 'Diffeq finished'


END PROGRAM DIFFEQ
