SUBROUTINE SETUP_CONSTANTS
  ! subroutine to assign constants appropriate values

  USE CONSTANTS       ! all variables from this module are now known in this subroutine

  IMPLICIT NONE

  II    = COMPLEX(0.,1.)

  KAPPA = 0.5
  PSI0=COMPLEX(1.,0.)

  DT    = 1.0
  NT    = 100

END SUBROUTINE SETUP_CONSTANTS

