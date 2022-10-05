MODULE CONSTANTS
  ! a module contains global variables. These are accessible from all subroutines and from the main program

  IMPLICIT NONE

  ! universal constants
  COMPLEX :: II       ! imaginary unit

  ! oscillation equation parameters
  REAL    :: KAPPA    ! parameter kappa in the oscillation equation (frequency)
  COMPLEX :: PSI0     ! initial condition

  ! discretization variables
  REAL    :: DT       ! timestep
  INTEGER :: NT       ! number of timesteps

END MODULE CONSTANTS

