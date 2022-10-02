MODULE FFT_MOD

! module containing FFT subroutines in the following convention:
!   - complex coefficients
!   - wavenumbers range from -N/2 to N/2
!

IMPLICIT NONE

CONTAINS

SUBROUTINE CHECK235(N)
! subroutine that checks if N can be factorized as 2^a * 3^b * 5^c

  ! arguments
  INTEGER, INTENT(IN) :: N
  ! local variables
  INTEGER :: J
  ! initialize J
  J=N
  ! remove factors 2
  DO WHILE (MODULO(J,2)==0 .AND. J>0); J=J/2; ENDDO
  ! remove factors 3
  DO WHILE (MODULO(J,3)==0 .AND. J>0); J=J/3; ENDDO
  ! remove factors 5
  DO WHILE (MODULO(J,5)==0 .AND. J>0); J=J/5; ENDDO
  ! the result should be 1
  IF (J /= 1 ) THEN
    WRITE (0,*) 'Dimension array should be factorizable into powers of 2, 3 or 5 to apply Fourier transform (N = ',N,')'
    CALL ABORT()
  ENDIF

END SUBROUTINE CHECK235

! 1d forward
SUBROUTINE FFT(Y,YS)
  REAL, INTENT(IN) :: Y(:)
  COMPLEX, INTENT(OUT) :: YS(:)
  
  ! local variables
  INTEGER :: N
  INTEGER :: KFAX(10)
  INTEGER :: INC, JUMP, ISIGN, IX, NFLD
  REAL, ALLOCATABLE    :: ZWORK(:), TRIGS(:)
  
  ! number of gridpoints
  N=SIZE(Y,1)

  ! check if 2,3,5-factorizable
  CALL CHECK235(N)

  ! check size of YS
  IF ( SIZE(YS,1) /= 2*(N/2)+1 ) THEN
    WRITE (0,*) 'Dimensions of spectral array don''t match dimensions of gridpoint array:', N, SIZE(YS,1)
    CALL ABORT()
  ENDIF
  
  ! allocate local arrays
  ALLOCATE(ZWORK(N+2), TRIGS(N))

  ! put data in ZWORK
  ZWORK(1:N)=Y
  ZWORK(N+1:N+2)=0.

  ! set fft arguments
  INC=1
  JUMP=N+2
  ISIGN=-1
  NFLD=1

  ! calculate TRIGS and KFAX
  CALL SET99(TRIGS,KFAX,N)

  ! transform to sines and cosines
  CALL FFT992(ZWORK,TRIGS,KFAX,INC,JUMP,N,NFLD,ISIGN)
  
  ! put in complex format into YS
  DO IX=0,N/2
    YS(N/2+1+IX)=CMPLX(ZWORK(2*IX+1),ZWORK(2*IX+2))
    YS(N/2+1-IX)=CMPLX(ZWORK(2*IX+1),-ZWORK(2*IX+2))
  ENDDO
  ! for even N, the 2dx wave is distributed evenly over -k and k
  IF (MODULO(N,2)==0) THEN
    YS(1)=YS(1)/2.
    YS(N+1)=YS(N+1)/2.
  ENDIF
  
  ! clean up
  DEALLOCATE(ZWORK,TRIGS)
  
END SUBROUTINE FFT

! 1d inverse
SUBROUTINE IFFT(YS,Y)
  COMPLEX, INTENT(IN) :: YS(:)
  REAL, INTENT(OUT) :: Y(:)
  
  ! local variables
  INTEGER :: N
  INTEGER :: KFAX(10)
  INTEGER :: INC, JUMP, ISIGN, IX, NFLD
  REAL, ALLOCATABLE    :: ZWORK(:), TRIGS(:)
  
  
  ! number of gridpoints
  N=SIZE(Y,1)

  ! check if 2,3,5-factorizable
  CALL CHECK235(N)

  ! check size of YS
  IF (SIZE(YS,1) /= 2*(N/2)+1 ) THEN
    WRITE (0,*) 'Dimensions of spectral array don''t match dimensions of gridpoint array:', N, SIZE(YS,1)
    CALL ABORT()
  ENDIF
  
  ! allocate local arrays
  ALLOCATE(ZWORK(N+2), TRIGS(N))

  ! put data in ZWORK
  ZWORK(1)=REAL(YS(N/2+1))
  ZWORK(2)=0
  DO IX=1,(N-1)/2
    ZWORK(2*IX+1)=REAL(YS(N/2+1+IX))
    ZWORK(2*IX+2)=AIMAG(YS(N/2+1+IX))
  ENDDO
  IF (MODULO(N,2)==0) THEN
    ZWORK(N+1)=2*REAL(YS(N+1))
  ENDIF
  ZWORK(N+2)=0.

  ! set fft arguments
  NFLD=1
  INC=1
  JUMP=N+2
  ISIGN=1
  
  ! calculate TRIGS and KFAX
  CALL SET99(TRIGS,KFAX,N)

  ! transform to sines and cosines
  CALL FFT992(ZWORK,TRIGS,KFAX,INC,JUMP,N,NFLD,ISIGN)

  ! copy to Y
  Y(1:N)=ZWORK(1:N)

  ! clean up
  DEALLOCATE(ZWORK,TRIGS)
  
END SUBROUTINE IFFT

! 2d forward
SUBROUTINE FFT2(Z,ZS)
  REAL, INTENT(IN) :: Z(:,:)
  COMPLEX, INTENT(OUT) :: ZS(:,:)
  REAL, ALLOCATABLE :: ZWORK(:,:), TRIGSX(:), TRIGSY(:)
  INTEGER :: IX, IY, NX, NY
  INTEGER :: KFAXX(10), KFAXY(10)
  INTEGER :: INC, JUMP, ISIGN, NFLD

  ! get dimensions
  NX=SIZE(Z,1)
  NY=SIZE(Z,2)
  
  ! check dimensions of YS
  IF (SIZE(ZS,1) /= 2*(NX/2)+1 ) THEN
    WRITE (0,*) 'X-dimension of gridpoint and spectral arrays don''t match: ',NX,SIZE(ZS,1)
    CALL ABORT()
  ENDIF
  IF (SIZE(ZS,2) /= 2*(NY/2)+1 ) THEN
    WRITE (0,*) 'Y-dimension of gridpoint and spectral arrays don''t match: ',NY,SIZE(ZS,2)
    CALL ABORT()
  ENDIF
  
  ! check factors
  CALL CHECK235(NX)
  CALL CHECK235(NY)

  ! allocations
  ALLOCATE(ZWORK(NX+2,NY+2),TRIGSX(NX),TRIGSY(NY))
  
  ! calculate TRIGS and KFAX
  CALL SET99(TRIGSX,KFAXX,NX)
  CALL SET99(TRIGSY,KFAXY,NY)

  ! put data in ZWORK
  ZWORK(:,:)=0.
  ZWORK(1:NX,1:NY)=Z

  ! x-transform
  INC=1
  JUMP=NX+2
  ISIGN=-1
  NFLD=NY
  CALL FFT992(ZWORK,TRIGSX,KFAXX,INC,JUMP,NX,NFLD,ISIGN)
  ! y-transform
  INC=NX+2
  JUMP=1
  ISIGN=-1
  NFLD=NX+2
  CALL FFT992(ZWORK,TRIGSY,KFAXY,INC,JUMP,NY,NFLD,ISIGN)
  
  ! transform to complex convention
	! A_++ = 1/4*(A_cc-A_ss) + i/4*(-A_sc-A_cs)
	! A_-+ = 1/4*(A_cc+A_ss) + i/4*(A_sc-A_cs)
	! A_+- = 1/4*(A_cc+A_ss) + i/4*(-A_sc+A_cs)
	! A_-- = 1/4*(A_cc-A_ss) + i/4*(A_sc+A_cs)
  
  ZS(:,:)=0.
  DO IX=0,NX/2
    DO IY=0,NY/2
      ZS(NX/2+1+IX,NY/2+1+IY)=COMPLEX( (ZWORK(2*IX+1,2*IY+1)-ZWORK(2*IX+2,2*IY+2)), &
        & (ZWORK(2*IX+2,2*IY+1)+ZWORK(2*IX+1,2*IY+2)) )
      ZS(NX/2+1-IX,NY/2+1+IY)=COMPLEX( (ZWORK(2*IX+1,2*IY+1)+ZWORK(2*IX+2,2*IY+2)), &
        &  (-ZWORK(2*IX+2,2*IY+1)+ZWORK(2*IX+1,2*IY+2)) )
      ZS(NX/2+1+IX,NY/2+1-IY)=CONJG(ZS(NX/2+1-IX,NY/2+1+IY))
      ZS(NX/2+1-IX,NY/2+1-IY)=CONJG(ZS(NX/2+1+IX,NY/2+1+IY))
    ENDDO
  ENDDO
  
  ! for even NX, the 2dx is split evenly over -kx and kx
  IF ( MODULO(NX,2)==0 ) THEN
    ZS(1,:)=ZS(1,:)/2.
    ZS(NX+1,:)=ZS(NX+1,:)/2.
  ENDIF
  ! same for even NY
  IF ( MODULO(NY,2)==0 ) THEN
    ZS(:,1)=ZS(:,1)/2.
    ZS(:,NY+1)=ZS(:,NY+1)/2.
  ENDIF

  ! clean up
  DEALLOCATE(ZWORK,TRIGSX,TRIGSY)
  
END SUBROUTINE FFT2

! 2d inverse
SUBROUTINE IFFT2(ZS,Z)
  COMPLEX, INTENT(IN) :: ZS(:,:)
  REAL, INTENT(OUT) :: Z(:,:)
  REAL, ALLOCATABLE :: ZWORK(:,:), TRIGSX(:), TRIGSY(:)
  INTEGER :: IX, IY, NX, NY
  INTEGER :: KFAXX(10), KFAXY(10)
  INTEGER :: INC, JUMP, ISIGN, NFLD

  ! get dimensions
  NX=SIZE(Z,1)
  NY=SIZE(Z,2)
  
  ! check dimensions of YS
  IF (SIZE(ZS,1) /= 2*(NX/2)+1 ) THEN
    WRITE (0,*) 'X-dimension of gridpoint and spectral arrays don''t match: ',NX,SIZE(ZS,1)
    CALL ABORT()
  ENDIF
  IF (SIZE(ZS,2) /= 2*(NY/2)+1 ) THEN
    WRITE (0,*) 'Y-dimension of gridpoint and spectral arrays don''t match: ',NY,SIZE(ZS,2)
    CALL ABORT()
  ENDIF
  
  ! check factors
  CALL CHECK235(NX)
  CALL CHECK235(NY)

  ! allocations
  ALLOCATE(ZWORK(NX+2,NY+2),TRIGSX(NX),TRIGSY(NY))
  
  ! calculate TRIGS and KFAX
  CALL SET99(TRIGSX,KFAXX,NX)
  CALL SET99(TRIGSY,KFAXY,NY)

  ! transform to complex convention
	! A_cc =     A_++ + A_-+ + A_+- + A_--			= 2*RE{  A_++ + A_-+ }
	! A_sc = i*( A_++ - A_-+ + A_+- - A_--)     = 2*IM{ -A_++ + A_-+ }
	! A_cs = i*( A_++ + A_-+ - A_+- - A_--)     = 2*IM{ -A_++ - A_-+ }
	! A_ss =    -A_++ + A_-+ + A_+- - A_--      = 2*RE{ -A_++ + A_-+ }
  ZWORK(:,:)=0.
  DO IX=0,NX/2
    DO IY=0,NY/2
      ZWORK(2*IX+1,2*IY+1)=REAL(  ZS(NX/2+1+IX,NY/2+1+IY)+ZS(NX/2+1-IX,NY/2+1+IY))/2.
      ZWORK(2*IX+2,2*IY+1)=-AIMAG(-ZS(NX/2+1+IX,NY/2+1+IY)+ZS(NX/2+1-IX,NY/2+1+IY))/2.
      ZWORK(2*IX+1,2*IY+2)=-AIMAG(-ZS(NX/2+1+IX,NY/2+1+IY)-ZS(NX/2+1-IX,NY/2+1+IY))/2.
      ZWORK(2*IX+2,2*IY+2)=REAL( -ZS(NX/2+1+IX,NY/2+1+IY)+ZS(NX/2+1-IX,NY/2+1+IY))/2.
    ENDDO
  ENDDO
  
  
  ! for even nx, the 2dx wave is split over -k and k
  IF ( MODULO(NX,2)==0) THEN
    ZWORK(NX+1,:)=2.*ZWORK(NX+1,:)
  ENDIF
  ! same for even ny
  IF ( MODULO(NY,2)==0) THEN
    ZWORK(:,NY+1)=2.*ZWORK(:,NY+1)
  ENDIF

  ! y-transform
  INC=NX+2
  JUMP=1
  ISIGN=1
  NFLD=NX+2
  CALL FFT992(ZWORK,TRIGSY,KFAXY,INC,JUMP,NY,NFLD,ISIGN)
  
  ! x-transform
  INC=1
  JUMP=NX+2
  ISIGN=1
  NFLD=NY
  CALL FFT992(ZWORK,TRIGSX,KFAXX,INC,JUMP,NX,NFLD,ISIGN)

  ! put data in ZWORK
  Z=ZWORK(1:NX,1:NY)
    
  ! clean up
  DEALLOCATE(ZWORK,TRIGSX,TRIGSY)
  
END SUBROUTINE IFFT2

END MODULE FFT_MOD
