MODULE CONSTANTS

  IMPLICIT NONE
  
  ! universal constants
  REAL    :: PI
  
  ! discretization properties
  INTEGER :: NX, NT     ! number of gridpoints, number of timesteps
  REAL    :: DX, DT     ! space and time resolution
  
  ! problem parameters
  REAL    :: C          ! advection speed
  
  ! output file name
  CHARACTER(LEN=128) :: FILE_OUT
  
END MODULE CONSTANTS
