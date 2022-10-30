SUBROUTINE WRITE_RESULT(PSI,PHI)

  ! write results (exact and numerical) to file
  
  USE CONSTANTS
  
  IMPLICIT NONE
  
  ! arguments
  REAL, INTENT(IN) :: PSI(NX), PHI(NX) ! exact and numerical solution
  
  ! local variables
  CHARACTER(LEN=32) :: FRMT            ! numerical format

  ! set output format
  WRITE (FRMT,'(A,I0,A)') '(',NX,'E16.8)'    ! store a field in one row with NX columns 
                                             ! of numbers in exponential format

  ! write to file
  OPEN(UNIT=77,FILE=FILE_OUT,ACCESS='APPEND')   ! append to the end of the file
  WRITE (UNIT=77,FMT=FRMT) PSI, PHI             ! write the two fields
  CLOSE(UNIT=77)                                ! close the file

END SUBROUTINE WRITE_RESULT
