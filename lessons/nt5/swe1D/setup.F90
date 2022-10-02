SUBROUTINE SETUP

! routine to setup constants

USE CONSTANTS

IMPLICIT NONE

! namelist parameters that are read from file fort.4
NAMELIST /NAMDOM/ NX, DX    ! Domain parameters
NAMELIST /NAMTIM/ NT, DT    ! Time parameters
NAMELIST /NAMCST/ G         ! Physical constants
NAMELIST /NAMSWE/ UM, HM    ! Shallow water equations parameters

! read parameters from fort.4 file
OPEN(UNIT=4)
READ(4, NAMDOM)
READ(4, NAMTIM)
READ(4, NAMCST)
READ(4, NAMSWE)
CLOSE(UNIT=4)

! open output file
FILE_OUT='output.dat'
OPEN(UNIT=77,FILE=FILE_OUT)

! write some constants to the output file
WRITE(77,*) NX, DX, NT, DT, UM
CLOSE(UNIT=77)

! show some relevant info
WRITE (*,*) 'Advection speed = ' , UM
WRITE (*,*) 'Sound speed     = ' , SQRT(G*HM)
WRITE (*,*) 'DX/DT           = ' , DX/DT

END SUBROUTINE SETUP
