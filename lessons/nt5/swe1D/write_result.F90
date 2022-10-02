SUBROUTINE WRITE_RESULT(U,H)

! write result to file

USE CONSTANTS

IMPLICIT NONE

! arguments
REAL, INTENT(IN) :: U(NX)
REAL, INTENT(IN) :: H(NX)

! local variables
CHARACTER(LEN=128) :: FRMT

! set the format (NX numbers in exponential format)
WRITE (FRMT,'(A,I0,A)') '(',NX,'E16.8)'

! Open file
OPEN(UNIT=77,FILE=FILE_OUT,ACCESS='APPEND')

! Write data
WRITE (UNIT=77,FMT=FRMT) U
WRITE (UNIT=77,FMT=FRMT) H

! Close file
CLOSE(UNIT=77)

END SUBROUTINE WRITE_RESULT

