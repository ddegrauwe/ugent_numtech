PROGRAM TEST_FFT

! small test program for FFT library
USE FFT_MOD

IMPLICIT NONE

! declarations
INTEGER           :: IX, NX, IY, NY
REAL, PARAMETER   :: PI=ACOS(-1.)
REAL, ALLOCATABLE :: Z(:,:), Z2(:,:)
COMPLEX, ALLOCATABLE :: ZS(:,:), ZS2(:,:)

INTEGER :: KX, KY

! initializations and allocations
NX=6
NY=6

! allocate
ALLOCATE(Z(NX,NY),Z2(NX,NY))
ALLOCATE(ZS(-NX/2:NX/2,-NY/2:NY/2), ZS2(-NX/2:NX/2,-NY/2:NY/2))

! fill with random numbers
CALL RANDOM_NUMBER(Z)
Z=100*Z
CALL RANDOM_NUMBER(Z2)
Z2=-100*Z2


WRITE (*,*) 'FFT test program'
WRITE (*,*) '  This program performs a forward and backward FFT, and '
WRITE (*,*) '  checks if the initial and final fields are the same'
WRITE (*,*)

! forward 1D fft
CALL FFT(Z(:,1),ZS(:,1))

! manual check
Z2=0.
DO IX=1,NX
  DO KX=-NX/2,NX/2
    Z2(IX,1)=Z2(IX,1)+REAL( ZS(KX,1)*EXP(COMPLEX(0.,1.)*2*PI*(KX*(IX-1)/REAL(NX))))
  ENDDO
ENDDO
WRITE (*,*) '  max diff after fft (1D) = ',MAXVAL(ABS(Z(:,1)-Z2(:,1)))

! inverse 1D fft
CALL IFFT(ZS(:,1),Z2(:,1))
WRITE (*,*) '  max diff after inverse fft (1D) = ',MAXVAL(ABS(Z(:,1)-Z2(:,1)))

! forward 2D fft
WRITE (*,*)
CALL FFT2(Z,ZS)

! manual check
Z2=0.
DO IX=1,NX
  DO IY=1,NY
    DO KX=-NX/2,NX/2
      DO KY=-NY/2,NY/2
        Z2(IX,IY)=Z2(IX,IY)+REAL((ZS(KX,KY))*EXP(COMPLEX(0.,1.)*2*PI*(KX*(IX-1)/REAL(NX)+KY*(IY-1)/REAL(NY))))
      ENDDO
    ENDDO
  ENDDO
ENDDO
WRITE (*,*) '  max diff after fft (2D) = ',MAXVAL(ABS(Z-Z2))

! inverse 2D fft
CALL IFFT2(ZS,Z2)
WRITE (*,*) '  max diff after inverse fft (2D) = ',MAXVAL(ABS(Z-Z2))

WRITE (*,*)
END PROGRAM TEST_FFT
