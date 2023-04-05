! Copyright (c) 2023 MeteoSwiss, contributors listed in AUTHORS
! 
! Distributed under the terms of the BSD 3-Clause License.
!  
! SPDX-License-Identifier: BSD-3-Clause

MODULE m_type_declaration

  ! Description

  ! Insert :: at variable type_declarations.

  ! Martin Schraner, MeteoSwiss, 4 November 2011

  USE m_stringpattern, ONLY: stringpattern
 
  IMPLICIT NONE

  PRIVATE

  !--- Module variables:

  INTEGER, PARAMETER :: maxlinelen = 250 
  INTEGER, PARAMETER :: ilun = 11, olun = 12    ! Logical file units

  ! Input and output lines:
  CHARACTER (len=maxlinelen) :: iline  = '', iline_up = ' ', oline = ''

  ! Patterns:
  CHARACTER (len=maxlinelen), PARAMETER :: &
       pattern_avoid = '[,(:]', &
       pattern_number = '[0-9]+', &
       pattern_star = '[(][*][)]', &
       pattern_function = &
       '^F[ ]*U[ ]*N[ ]*C[ ]*T[ ]*I[ ]*O[ ]*N', &
       pattern_integer = '^[ ]*INTEGER', &
       pattern_integerdec1 = &
       TRIM(pattern_integer)//'[ ]*[*][ ]*[0-9]+[ ]+', &
       pattern_integerdec2 = TRIM(pattern_integer)//'[ ]+', &
       pattern_real = '^[ ]*REAL', &
       pattern_realdec1 = &
       TRIM(pattern_real)//'[ ]*[*][ ]*[0-9]+[ ]+', &
       pattern_realdec2 = TRIM(pattern_real)//'[ ]+', &
       pattern_character = '^[ ]*CHARACTER', &
       pattern_characterdec1 = &
       TRIM(pattern_character)//'[ ]*[*][ (]*[0-9*]+[)]*[ ]+', &
       pattern_characterdec2 = TRIM(pattern_character)//'[ ]+', &
       pattern_logical = '^[ ]*LOGICAL', &
       pattern_logicaldec = TRIM(pattern_logical)//'[ ]+', &
       pattern_doubleprecision = '^[ ]*DOUBLE[ ]+PRECISION', &
       pattern_doubleprecisiondec = TRIM(pattern_doubleprecision)//'[ ]+'

  INTEGER :: ios, linenumber, spos, epos, spos_var, spos_n, epos_n
  LOGICAL :: lpattern_found

  ! Functions:
  CHARACTER (len=maxlinelen), EXTERNAL :: toupper

  ! Public entities:
  PUBLIC :: type_declaration

CONTAINS

  SUBROUTINE type_declaration (infile, outfile)

    ! Dummy arguments:
    CHARACTER (LEN=*), INTENT(in) :: infile, outfile
   
    ! Executable statements:

    ! Initialize:
    linenumber = 0              ! Line number

    ! Open input and ouput files:
    OPEN(unit=ilun,file=infile,action='read',position='rewind',status='old')
    OPEN(unit=olun,file=outfile,action='write',position='rewind',status='replace')

    ! Loop over all input lines:
    DO

       lpattern_found = .FALSE.
       linenumber = linenumber + 1
       
       ! Read an input line
       READ (ilun,'(a)',iostat=ios) iline
       
       ! Exit at the end of file:
       IF (ios /= 0) THEN
          EXIT
       END IF

       ! Convert iline to UPPERCASE:
       iline_up = toupper(iline)

       ! Check for INTEGER:
       CALL check_integer
       IF (lpattern_found) CYCLE

       ! Check for REAL:
       CALL check_real
       IF (lpattern_found) CYCLE

       ! Check for CHARACTER:
       CALL check_character
       IF (lpattern_found) CYCLE

       ! Check for LOGICAL:
       CALL check_logical
       IF (lpattern_found) CYCLE

       ! Check for DOUBLE PRECISION:
       CALL check_doubleprecision
       IF (lpattern_found) CYCLE

       ! Rewrite line:
       oline = TRIM(iline)
       CALL write_output

    END DO

    ! Close files:
    CLOSE(ilun)
    CLOSE(olun)

  END SUBROUTINE type_declaration


  SUBROUTINE check_integer

    ! Local variables:
    LOGICAL :: lstar, luppercase

    oline = ' '
    CALL stringpattern(TRIM(iline_up), pattern_integerdec1, spos, spos_var)
    IF (spos_var == 0) THEN
       CALL stringpattern &
            (TRIM(iline_up), pattern_integerdec2, spos, spos_var)
       IF (spos_var > 0) lstar = .FALSE.
    ELSE 
       lstar = .TRUE.
    END IF
    IF (spos_var == 0) RETURN   ! Exit subroutine if pattern not found

    ! Column where variables start:
    spos_var = spos_var + 1

    ! Check if follows ',', '(' or ':':
    CALL stringpattern(iline_up(spos_var:spos_var), pattern_avoid, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Check if follows FUNCTION:
    CALL stringpattern &
         (TRIM(iline_up(spos_var:)), pattern_function, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Uppercase keyword?
    CALL stringpattern(TRIM(iline), pattern_real, spos, epos)    
    IF (spos > 0) THEN
       luppercase = .TRUE.
    ELSE
       luppercase = .FALSE.
    END IF
    
    ! Column where INTEGER ends:
    CALL stringpattern(TRIM(iline_up), pattern_integer, spos, epos)
    oline(1:epos) = iline(1:epos)

    IF (lstar) THEN    ! integer with kind
       IF (luppercase) THEN
          oline(epos+1:epos+6) = '(KIND='
       ELSE
          oline(epos+1:epos+6) = '(kind='
       END IF
       CALL stringpattern(TRIM(iline_up), pattern_number, spos_n, epos_n)
       oline(epos+7:epos+7+epos_n-spos_n) = iline(spos_n:epos_n)
       oline(epos+7+epos_n-spos_n+1:epos+7+epos_n-spos_n+5) = ') :: '
       oline(epos+7+epos_n-spos_n+6:) = TRIM(iline(spos_var:))
    ELSE   ! without kind
       oline(epos+1:epos+4) = ' :: '
       oline(epos+5:) = TRIM(iline(spos_var:))
    END IF

    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_integer


  SUBROUTINE check_real

    ! Local variables:
    LOGICAL :: lstar, luppercase

    oline = ' '
    CALL stringpattern(TRIM(iline_up), pattern_realdec1, spos, spos_var)
    IF (spos_var == 0) THEN
       CALL stringpattern &
            (TRIM(iline_up), pattern_realdec2, spos, spos_var)
       IF (spos_var > 0) lstar = .FALSE.
    ELSE 
       lstar = .TRUE.
    END IF
    IF (spos_var == 0) RETURN   ! Exit subroutine if pattern not found

    ! Column where variables start:
    spos_var = spos_var + 1

    ! Check if follows ',', '(' or ':':
    CALL stringpattern(iline_up(spos_var:spos_var), pattern_avoid, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Check if follows FUNCTION:
    CALL stringpattern &
         (TRIM(iline_up(spos_var:)), pattern_function, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Uppercase keyword?
    CALL stringpattern(TRIM(iline), pattern_real, spos, epos)    
    IF (spos > 0) THEN
       luppercase = .TRUE.
    ELSE
       luppercase = .FALSE.
    END IF
    
    ! Column where REAL ends:
    CALL stringpattern(TRIM(iline_up), pattern_real, spos, epos)
    oline(1:epos) = iline(1:epos)

    IF (lstar) THEN    ! real with kind
       IF (luppercase) THEN
          oline(epos+1:epos+6) = '(KIND='
       ELSE
          oline(epos+1:epos+6) = '(kind='
       END IF
       CALL stringpattern(TRIM(iline_up), pattern_number, spos_n, epos_n)
       oline(epos+7:epos+7+epos_n-spos_n) = iline(spos_n:epos_n)
       oline(epos+7+epos_n-spos_n+1:epos+7+epos_n-spos_n+5) = ') :: '
       oline(epos+7+epos_n-spos_n+6:) = TRIM(iline(spos_var:))
    ELSE   ! without kind
       oline(epos+1:epos+4) = ' :: '
       oline(epos+5:) = TRIM(iline(spos_var:))
    END IF

    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_real


  SUBROUTINE check_character

    ! Local variables:
    LOGICAL :: lstar, luppercase

    oline = ' '
    CALL stringpattern(TRIM(iline_up), pattern_characterdec1, spos, spos_var)
    IF (spos_var == 0) THEN
       CALL stringpattern &
            (TRIM(iline_up), pattern_characterdec2, spos, spos_var)
       IF (spos_var > 0) lstar = .FALSE.
    ELSE 
       lstar = .TRUE.
    END IF
    IF (spos_var == 0) RETURN   ! Exit subroutine if pattern not found

    ! Column where variables start:
    spos_var = spos_var + 1

    ! Check if follows ',', '(' or ':':
    CALL stringpattern(iline_up(spos_var:spos_var), pattern_avoid, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Check if follows FUNCTION:
    CALL stringpattern &
         (TRIM(iline_up(spos_var:)), pattern_function, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Uppercase keyword?
    CALL stringpattern(TRIM(iline), pattern_real, spos, epos)    
    IF (spos > 0) THEN
       luppercase = .TRUE.
    ELSE
       luppercase = .FALSE.
    END IF
    
    ! Column where CHARACTER ends:
    CALL stringpattern(TRIM(iline_up), pattern_character, spos, epos)
    oline(1:epos) = iline(1:epos)

    IF (lstar) THEN    ! character with kind
       IF (luppercase) THEN
          oline(epos+1:epos+5) = '(LEN='
       ELSE
          oline(epos+1:epos+5) = '(len='
       END IF
       CALL stringpattern(TRIM(iline_up), pattern_number, spos_n, epos_n)
       if (spos_n == 0) then
          CALL stringpattern(TRIM(iline_up), pattern_star, spos_n, epos_n)
          spos_n = spos_n+1
          epos_n = epos_n-1
       end if
       oline(epos+6:epos+6+epos_n-spos_n) = iline(spos_n:epos_n)
       oline(epos+6+epos_n-spos_n+1:epos+6+epos_n-spos_n+5) = ') :: '
       oline(epos+6+epos_n-spos_n+6:) = TRIM(iline(spos_var:))
    ELSE   ! without kind
       oline(epos+1:epos+4) = ' :: '
       oline(epos+5:) = TRIM(iline(spos_var:))
    END IF

    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_character


  SUBROUTINE check_logical

    oline = ' '
    CALL stringpattern(TRIM(iline_up), pattern_logicaldec, spos, spos_var)
    IF (spos_var == 0) RETURN   ! Exit subroutine if pattern not found

    ! Column where variables start:
    spos_var = spos_var + 1

    ! Check if follows ',', '(' or ':':
    CALL stringpattern(iline_up(spos_var:spos_var), pattern_avoid, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Check if follows FUNCTION:
    CALL stringpattern &
         (TRIM(iline_up(spos_var:)), pattern_function, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Column where LOGICAL ends:
    CALL stringpattern(TRIM(iline_up), pattern_logical, spos, epos)
    oline(1:epos) = iline(1:epos)
    oline(epos+1:epos+4) = ' :: '
    oline(epos+5:) = TRIM(iline(spos_var:))

    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_logical


  SUBROUTINE check_doubleprecision

    oline = ' '
    CALL stringpattern &
         (TRIM(iline_up), pattern_doubleprecisiondec, spos, spos_var)
    IF (spos_var == 0) RETURN   ! Exit subroutine if pattern not found

    ! Column where variables start:
    spos_var = spos_var + 1

    ! Check if follows ',', '(' or ':':
    CALL stringpattern(iline_up(spos_var:spos_var), pattern_avoid, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found

    ! Check if follows FUNCTION:
    CALL stringpattern &
         (TRIM(iline_up(spos_var:)), pattern_function, spos, epos)
    IF (spos > 0) RETURN   ! Exit subroutine if pattern found
    
    ! Column where DOUBLEPRECISION ends:
    CALL stringpattern(TRIM(iline_up), pattern_doubleprecision, spos, epos)
    oline(1:epos) = iline(1:epos)
    oline(epos+1:epos+4) = ' :: '
    oline(epos+5:) = TRIM(iline(spos_var:))

    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_doubleprecision


  SUBROUTINE write_output

    WRITE(olun,'(a)') TRIM(oline)

  END SUBROUTINE write_output

END MODULE m_type_declaration
