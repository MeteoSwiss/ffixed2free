MODULE m_stringpattern

  ! Description:

  ! This module contains the procedure *stringpattern*, which calls the 
  ! C-routine *stringpatternc*. *stringpattern* checks the match of the
  ! string <str> with the regular expression <pattern>. In case of
  ! a match, it returns the  start and end indices <spos> and <epos> of 
  ! the first match and zero, zero otherwise.

  ! NB: TO BE COMPILED WITH gfortran!

  ! M. Schaner, MeteoSwiss, 27 October 2011


  IMPLICIT NONE

  ! External subroutine:
  EXTERNAL :: stringpatternc
 
CONTAINS

  SUBROUTINE stringpattern (str, pattern, spos, epos)

    ! Dummy variables:
    CHARACTER(*), INTENT(in) :: str, pattern
    INTEGER, INTENT(out) :: spos, epos

    ! Executable statements:

    ! Call C-routine:
    CALL stringpatternc(TRIM(str) // char(0), TRIM(pattern) // char(0), &
         spos, epos)

  END SUBROUTINE stringpattern

END MODULE m_stringpattern
