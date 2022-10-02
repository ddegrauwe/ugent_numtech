SUBROUTINE SETUP

USE CONSTANTS

IMPLICIT NONE

! universal constants
PI=ACOS(-1.0)
II=COMPLEX(0.0,1.0)

! discretization properties
NX=100
DX=1.0
NT=50
DT=1000.0

! advection parameters
C=0.7

! output file name
FILE_OUT='output.dat'

! write some constants to the output file
OPEN(UNIT=77,FILE=FILE_OUT)
WRITE (77,*) NX, DX, NT, DT, 2                 ! discretization properties, number of experiments
WRITE (77,*) '"exact" "forward_spectral"'      ! experiment names
CLOSE(UNIT=77)

END SUBROUTINE SETUP
