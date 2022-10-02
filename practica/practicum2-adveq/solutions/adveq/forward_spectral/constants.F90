MODULE CONSTANTS

! constants for the advection model

IMPLICIT NONE

! universal constants
REAL    :: PI     
COMPLEX :: II     ! imaginary unit

! discretization properties
INTEGER :: NX     ! number of gridpoints
REAL    :: DX     ! spatial resolution

INTEGER :: NT     ! number of timesteps
REAL    :: DT     ! time resolution

! advection parameters
REAL    :: C      ! advection speed

! output file name
CHARACTER(LEN=128) :: FILE_OUT

END MODULE CONSTANTS
