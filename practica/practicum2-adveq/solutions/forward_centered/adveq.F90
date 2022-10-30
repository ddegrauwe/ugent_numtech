PROGRAM ADVEQ

! program to solve the 1D advection equation
!

! module containing constant parameters
USE CONSTANTS

IMPLICIT NONE

! initialize constant parameters
CALL SETUP()

! time integration
CALL TIMELOOP()

END PROGRAM ADVEQ