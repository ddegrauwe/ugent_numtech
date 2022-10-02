MODULE CONSTANTS

IMPLICIT NONE

! Physical constants
REAL    :: G                                    ! gravity constant

! System constants
REAL    :: UM                                   ! background advection speed
REAL    :: HM                                   ! background water height

! Discretization
INTEGER :: NX                                   ! number of gridpoints
INTEGER :: NT                                   ! number of timesteps
REAL    :: DX                                   ! gridpoint distance
REAL    :: DT                                   ! time step

! Output file
CHARACTER(LEN=128) :: FILE_OUT

END MODULE CONSTANTS

