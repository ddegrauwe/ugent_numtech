SUBROUTINE DERIVX(Y,DYDX)

! calculates the derivative of a field

USE CONSTANTS

IMPLICIT NONE

REAL, INTENT(IN)  :: Y(NX)
REAL, INTENT(OUT) :: DYDX(NX)


! Centered finite difference
DYDX(2:NX-1) = Y(3:NX)-Y(1:NX-2)
DYDX(1)      = Y(2)-Y(NX)
DYDX(NX)     = Y(1)-Y(NX-1)

! Divide by gridpoint space
DYDX(:)=DYDX(:)/(2.*DX)

END SUBROUTINE DERIVX
