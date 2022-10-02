SUBROUTINE SETUP
  
  ! setup constants of the advection equation
  
  USE CONSTANTS

  IMPLICIT NONE
  
  ! universal constants
  PI=ACOS(-1.0)

  ! discretization properties
  NX = 100
  DX = 1.0
  
  NT = 50
  DT = 1.0
  
  ! system parameters
  C =  0.8
  
  ! print info to screen
  WRITE (*,*) ''
  WRITE (*,*) 'Program to solve the Advection equation'
  WRITE (*,*) ''
  WRITE (*,*) 'Parameters:'
  WRITE (*,*) '  C   = ',C
  WRITE (*,*) '  DT  = ',DT
  WRITE (*,*) '  DX  = ',DX
  WRITE (*,*) '  CFL = ',C*DT/DX
  WRITE (*,*) ''
  
  
  ! output file
  FILE_OUT='output.dat'
  
  ! write some constants to the output file -- to be read by R
  OPEN(UNIT=77,FILE=FILE_OUT)
  WRITE (77,*) NX, DX, NT, DT, 2         ! discretization properties, number of experiments
  WRITE (77,*) '"exact" "matsuno_centered"'      ! experiment names
  CLOSE(UNIT=77)

END SUBROUTINE SETUP
