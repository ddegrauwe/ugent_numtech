PROGRAM OSCEQ
! a program to solve the oscillation equation

IMPLICIT NONE       ! safety to make sure all variables are declared

! initialize constant parameters
CALL SETUP_CONSTANTS()

! time loop
CALL TIMELOOP()

END PROGRAM OSCEQ

