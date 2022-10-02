PROGRAM SWE1D

! 1D Linearized shallow water equations
!
!   du/dt + UM du/dx +  g dh/dx = 0
!   dh/dt + HM du/dx + UM dh/dx = 0
!
! where UM and HM are the (constant) main horizontal speed and the water height, 
! u and h are the perturbations on the horizontal speed and the water height.
! g is the gravitational acceleration.
!

IMPLICIT NONE

! setup constants
CALL SETUP()

! perform time loop
CALL TIMELOOP()

END PROGRAM SWE1D 


