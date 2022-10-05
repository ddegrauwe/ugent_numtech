SUBROUTINE WRITE_RESULT(PSI,PHI)

  ! write the result to a file
  USE CONSTANTS

  IMPLICIT NONE

  ! arguments
  COMPLEX, INTENT(IN) :: PSI(0:NT), PHI(0:NT)
  
  ! auxiliary results
  INTEGER :: IT     ! timestep

  ! open a file
  OPEN(FILE='output.dat',UNIT=20)
  
  ! write heading
  WRITE (20,'(A6,A12,A12)') 'time','exact','numerical'
  
  ! write solutions at all timesteps to the file
  DO IT=0,NT
    WRITE (20,'(F6.2,E18.10,E18.10)') IT*DT, REAL(PSI(IT)), REAL(PHI(IT))
  ENDDO
  
  ! close the file
  CLOSE(UNIT=20)

END SUBROUTINE WRITE_RESULT

