MODULE m_indent_subroutines

  ! Description:

  ! Module to indent bodies of subroutines, functions, programs and modules
  ! according to FORTRAN 90 style. Furthermore, the end of a subroutine,
  ! function, program or module is changed to 
  ! - end subroutine <name_of_subroutine>,
  ! - end function <name_of_function>,
  ! - end program <name_of_program>, or
  ! - end module <name_of_module>, 
  ! respectively.

  ! Martin Schraner, MeteoSwiss, 1 November 2011

  USE m_linked_list_indent_subroutines
  USE m_stringpattern, ONLY: stringpattern
 
  IMPLICIT NONE

  PRIVATE

  !--- Module variables:

  INTEGER, PARAMETER :: maxlinelen = 250 
  INTEGER, PARAMETER :: description_linelen =80
  INTEGER, PARAMETER :: ilun = 11, olun = 12    ! Logical file units
  INTEGER, PARAMETER :: tab = 2 

  ! Input and output lines:
  CHARACTER (len=maxlinelen) :: iline  = '', iline_up = ' ', oline = '', &
       name = '', keyword = '' 

  ! Patterns:
  CHARACTER (len=maxlinelen), PARAMETER :: &
       comment_pattern = '^[ ]*!', &
       omp_pattern = '^[ ]*[!][$]OMP[ ]+', &
       cpreprocessor_pattern = '^[ ]*[#]', &
       label_pattern = '^[ ]*[0-9]+[ ]*[A-Z_]+', &
       labelw_pattern = '^[ ]*[0-9]+[ ]*', &
       named_do_block_pattern = '^[ ]*[A-Z0-9_]+:[ ]*D[ ]*O[!A-Z0-9_]', &
       named_if_block_pattern = '^[ ]*[A-Z0-9_]+:[ ]*I[ ]*F[!A-Z0-9_]', &
       named_case_block_pattern = &
       '^[ ]*[A-Z0-9_]+:[ ]*S[ ]*E[ ]*L[ ]*E[ ]*C[ ]*T[ ]**C[ ]*A[ ]*S[ ]*E[!A-Z0-9_]', &
       named_blockw_pattern = '^[ ]*[A-Z0-9_]+:[ ]*', &
       contains_pattern = '^[ ]*C[ ]*O[ ]*N[ ]*T[ ]*A[ ]*I[ ]*N[ ]*S[ ]*$', &
       containsw_pattern = 'C[ ]*O[ ]*N[ ]*T[ ]*A[ ]*I[ ]*N[ ]*S', &
       end_pattern = '^[ ]*E[ ]*N[ ]*D[ ]*', &
       subroutine_pattern = &
       'S[ ]*U[ ]*B[ ]*R[ ]*O[ ]*U[ ]*T[ ]*I[ ]*N[ ]*E[ ]*', &
       begin_subroutine_pattern = '^[ ]*'//TRIM(subroutine_pattern), &
       subroutine_name_pattern = TRIM(begin_subroutine_pattern)//'[A-Z0-9_]+', &
       end_subroutine_pattern = TRIM(end_pattern)//TRIM(subroutine_pattern), &
       function_pattern = &
       'F[ ]*U[ ]*N[ ]*C[ ]*T[ ]*I[ ]*O[ ]*N[ ]*', &
       begin_function_pattern = '^[ ]*'//TRIM(function_pattern), &
       begin_function_pattern2 = '[ ]+'//TRIM(function_pattern), &
       function_name_pattern = &
       '^.*'//TRIM(function_pattern)//'[A-Z0-9_]+', &
       end_function_pattern = TRIM(end_pattern)//TRIM(function_pattern), &
       module_pattern = &
       'M[ ]*O[ ]*D[ ]*U[ ]*L[ ]*E[ ]*', &
       begin_module_pattern = '^[ ]*'//TRIM(module_pattern), &
       module_name_pattern = TRIM(begin_module_pattern)//'[A-Z0-9_]+', &
       end_module_pattern = TRIM(end_pattern)//TRIM(module_pattern), &
       program_pattern = &
       'P[ ]*R[ ]*O[ ]*G[ ]*R[ ]*A[ ]*M[ ]*', &
       begin_program_pattern = '^[ ]*'//TRIM(program_pattern), &
       program_name_pattern = TRIM(begin_program_pattern)//'[A-Z0-9_]+', &
       end_program_pattern = TRIM(end_pattern)//TRIM(program_pattern), &
       blockdata_pattern = &
       'B[ ]*L[ ]*O[ ]*C[ ]*K[ ]*D[ ]*A[ ]*T[ ]*A[ ]*', &
       begin_blockdata_pattern = '^[ ]*'//TRIM(blockdata_pattern), &
       blockdata_name_pattern = TRIM(begin_blockdata_pattern)//'[A-Z0-9_]+', &
       end_blockdata_pattern = TRIM(end_pattern)//TRIM(blockdata_pattern)
  
  ! Item of linked list:
  TYPE (linked_list_item), POINTER :: item

  INTEGER :: ios, linenumber  
  INTEGER :: bl, spos, epos, spos_name, epos_name
  LOGICAL :: lpattern_found, luppercase, ldescription

  ! Functions:
  CHARACTER (len=maxlinelen), EXTERNAL :: toupper, tolower
  
  ! Public entities:
  PUBLIC :: indent_subroutines

CONTAINS

  SUBROUTINE indent_subroutines (infile, outfile)

    ! Dummy arguments:
    CHARACTER (LEN=*), INTENT(in) :: infile, outfile

    ! Executable statements:

    ! Initialize:
    bl = 0                      ! Number of blanks at beginning of line
    linenumber = 0              ! Line number
    ldescription = .FALSE.      ! Flag if current line is part of subroutine
                                ! description block

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

       ! Test if bl is negative:
       IF (bl < 0) THEN
          PRINT *, 'bl < 0. ERROR'
          STOP
       END IF

       ! Convert iline to UPPERCASE:
       iline_up = toupper(iline)

       ! Check for OMP:
       CALL check_omp
       IF (lpattern_found) CYCLE

       ! Check for C preprocessor directives:
       CALL check_cpreprocessor
       IF (lpattern_found) CYCLE

       ! Check for beginning of subroutine, function, module or program:
       CALL check_beginning
       IF (lpattern_found) CYCLE

       ! Check for end of subroutine, function, module or program:
       CALL check_end
       IF (lpattern_found) CYCLE

       ! Check for line beginning with a label:
       CALL check_label
       IF (lpattern_found) CYCLE

       ! Check for line beginning with a named block:
       CALL check_named_block
       IF (lpattern_found) CYCLE

       ! Check for CONTAINS:
       CALL check_contains
       IF (lpattern_found) CYCLE

       ! Check for subroutine description block:
       CALL check_description
       IF (ldescription) CYCLE       

       ! Rewrite line:
       IF (bl >= 1) oline(1:bl) = ' '
       oline(bl+1:) = TRIM(iline)
       CALL write_output

    END DO

    ! Close files:
    CLOSE(ilun)
    CLOSE(olun)

  END SUBROUTINE indent_subroutines


  SUBROUTINE check_beginning

    ! Local variables
    integer :: i_bang, i_quote
   
    keyword = ''

    ! Check for beginning of SUBROUTINE:
    CALL stringpattern(TRIM(iline_up), begin_subroutine_pattern, spos, epos)
    IF (spos > 0) THEN
       keyword = 'SUBROUTINE'

       ! Get name:
       CALL stringpattern &
            (TRIM(iline_up), subroutine_name_pattern, spos, epos_name)
       spos_name = epos + 1
       name = TRIM(iline(spos_name:epos_name))

    ELSE
       
       ! Check for beginning of FUNCTION (check 2 types of pattern):
       CALL stringpattern &
            (TRIM(iline_up), begin_function_pattern, spos, epos)
       IF (spos == 0) CALL stringpattern &
            (TRIM(iline_up), begin_function_pattern2, spos, epos)

       IF (spos > 0) THEN

          ! Extra checks for FUNCTION, since FUNCTION does not have to
          ! be the first word of a line (e.g. REAL FUNCTION has to be valid,
          ! but ! FUNCTION and END FUNCTION not).

          ! Check if function is not after a comment sign:
          i_bang = index(iline_up,'!')
          if (i_bang > 0 .and. i_bang < spos) return 

          ! Check if function is not after a quote:
          i_quote = max(index(iline_up,"'"),index(iline_up,'"'))
          if (i_quote > 0 .and. i_quote < spos) return 

          ! Check if line is not an END FUNCTION:
          CALL stringpattern(TRIM(iline_up), end_pattern, spos, epos)

          IF (spos > 0) RETURN
          keyword = 'FUNCTION'

          ! Get name:
          CALL stringpattern &
            (TRIM(iline_up), function_pattern, spos, epos)
          CALL stringpattern &
               (TRIM(iline_up), function_name_pattern, spos, epos_name)
          spos_name = epos + 1
          name = TRIM(iline(spos_name:epos_name))

       ELSE
          
          ! Check for beginning of MODULE:
          CALL stringpattern(TRIM(iline_up), begin_module_pattern, spos, epos)
          IF (spos > 0) THEN
             keyword = 'MODULE'

             ! Get name:
             CALL stringpattern &
                  (TRIM(iline_up), module_name_pattern, spos, epos_name)
             spos_name = epos + 1
             name = TRIM(iline(spos_name:epos_name))
             
          ELSE

             ! Check for beginning of PROGRAM:
             CALL stringpattern &
                  (TRIM(iline_up), begin_program_pattern, spos, epos)
             IF (spos > 0) THEN
                keyword = 'PROGRAM'

                CALL stringpattern &
                     (TRIM(iline_up), program_name_pattern, spos, epos_name)
                spos_name = epos + 1
                name = TRIM(iline(spos_name:epos_name))

             ELSE

                ! Check for beginning of BLOCKDATA:
                CALL stringpattern &
                     (TRIM(iline_up), begin_blockdata_pattern, spos, epos)
                IF (spos > 0) THEN
                   keyword = 'BLOCK DATA'

                   CALL stringpattern &
                        (TRIM(iline_up), blockdata_name_pattern, spos, epos_name)
                   spos_name = epos + 1
                   name = TRIM(iline(spos_name:epos_name))

                END IF
             END IF
          END IF
       END IF
    END IF

    ! Exit subroutine if pattern not found:
    IF (keyword == '') RETURN

    ! Create new item:
    CALL new_item(TRIM(keyword), TRIM(name), item)
    CALL add_at_tail(item)

    IF (bl >= 1) oline(1:bl) = ' '
    oline(bl+1:) = TRIM(iline)
    CALL write_output

    bl = bl + tab

    lpattern_found = .TRUE.

  END SUBROUTINE check_beginning

  SUBROUTINE check_end

    keyword = ''
    
    ! Check for END:
    CALL stringpattern(TRIM(iline_up), end_pattern, spos, epos)
    
    IF (spos == 0) THEN   ! Pattern END not found
       RETURN             ! Exit subroutine

    ELSE     ! Pattern END found

       ! Was END originally written in uppercase?
       CALL stringpattern(TRIM(iline), end_pattern, spos, epos)
       IF (spos == 0) THEN
          luppercase = .FALSE.
       ELSE
          luppercase = .TRUE.
       END IF
       
       ! Is there another word after END?
       CALL stringpattern &
            (TRIM(iline_up), TRIM(end_pattern)//'[A-Z0-9_=]+', spos, epos)
       
       IF (spos > 0) THEN    ! Pattern found

          keyword = 'unknown'
          
          ! Is it a END SUBROUTINE?
          CALL stringpattern &
               (TRIM(iline_up), end_subroutine_pattern, spos, epos)
          IF (spos > 0) THEN
             keyword = 'SUBROUTINE'             

          ELSE
          
             ! Is it a END FUNCTION?
             CALL stringpattern &
                  (TRIM(iline_up), end_function_pattern, spos, epos)
             IF (spos > 0) THEN
                keyword = 'FUNCTION'
                
             ELSE

                ! Is it a END MODULE?
                CALL stringpattern &
                     (TRIM(iline_up), end_module_pattern, spos, epos)
                IF (spos > 0) THEN
                   keyword = 'MODULE'
                   
                ELSE
                         
                   ! Is it a END PROGRAM?
                   CALL stringpattern &
                        (TRIM(iline_up), end_program_pattern, spos, epos)
                   IF (spos > 0) THEN
                      keyword = 'PROGRAM'
                   
                   ELSE
                         
                      ! Is it a END BLOCKDATA?
                      CALL stringpattern &
                           (TRIM(iline_up), end_blockdata_pattern, spos, epos)
                      IF (spos > 0) THEN
                         keyword = 'BLOCK DATA'
                   
                      END IF
                   END IF
                END IF
             END IF
          END IF
       END IF

    END IF

    IF (keyword == 'unknown') RETURN

    ! Get and delete item at tail:
    CALL get_and_delete_at_tail(item)

    IF (.NOT.(ASSOCIATED(item))) THEN
       PRINT *, 'Linked list is empty. ERROR!'
       STOP
    END IF

    bl = bl - tab

    IF (keyword /= '') THEN    ! END SUBROUTINE, END MODULE,...

       IF (TRIM(keyword) /= TRIM(item%keyword)) THEN
          PRINT *, 'Mismatch of keywords'
          STOP
       END IF
       IF (bl >= 1) oline(1:bl) = ' '
       oline(bl+1:) = TRIM(iline)

    ELSE     ! END without another keyword

       IF (bl >= 1) oline(1:bl) = ' '
       oline(bl+1:) = 'END '//TRIM(item%keyword)//' '//TRIM(item%name)
       IF (.NOT.(luppercase)) oline = tolower(oline)

    END IF   ! END 
          
    CALL write_output
    
    lpattern_found = .TRUE.

  END SUBROUTINE check_end


  SUBROUTINE check_label

    ! Check if current line begins with a label:
    CALL stringpattern(TRIM(iline_up), label_pattern, spos, epos)

    ! Exit subroutine if pattern not found:
    IF (spos == 0) RETURN 

    CALL stringpattern(TRIM(iline_up), labelw_pattern, spos, epos)

    oline(1:epos) = iline(1:epos)
    IF (bl >= 1) oline(epos+1:epos+bl) = ' '
    oline(epos+bl+1:) = TRIM(iline(epos+1:)) 
    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_label


  SUBROUTINE check_named_block

    ! Check for named DO block:
    CALL stringpattern(TRIM(iline_up), named_do_block_pattern, spos, epos)

    IF (spos > 0) THEN  ! Pattern found
       lpattern_found = .TRUE.

    ELSE

       ! Check for named DO block:
       CALL stringpattern(TRIM(iline_up), named_if_block_pattern, spos, epos)

       IF (spos > 0) THEN  ! Pattern found
          lpattern_found = .TRUE.

       ELSE

          ! Check for named CASE block:
          CALL stringpattern &
               (TRIM(iline_up), named_case_block_pattern, spos, epos)

          IF (spos > 0) lpattern_found = .TRUE.  ! Pattern found

       END IF
    END IF

    ! Exit subroutine if pattern not found:
    IF (.NOT.(lpattern_found)) RETURN

    CALL stringpattern(TRIM(iline_up), named_blockw_pattern, spos, epos)

    oline(1:epos) = iline(1:epos)
    IF (bl >= 1) oline(epos+1:epos+bl) = ' '
    oline(epos+bl+1:) = TRIM(iline(epos+1:)) 
    CALL write_output

  END SUBROUTINE check_named_block


  SUBROUTINE check_omp

    ! Check if current line is OMP:
    CALL stringpattern(TRIM(iline_up), omp_pattern, spos, epos)

    ! Exit subroutine if pattern not found:
    IF (spos == 0) RETURN

    ! Remove leading blanks:
    CALL stringpattern(TRIM(iline_up), '!', spos, epos)
    oline(1:) = TRIM(iline(spos:))
    CALL write_output
    
    lpattern_found = .TRUE.

  END SUBROUTINE check_omp


  SUBROUTINE check_cpreprocessor

    ! Check if current line is CPREPROCESSOR:
    CALL stringpattern(TRIM(iline_up), cpreprocessor_pattern, spos, epos)

    ! Exit subroutine if pattern not found:
    IF (spos == 0) RETURN

    ! Remove leading blanks:
    CALL stringpattern(TRIM(iline_up), '#', spos, epos)
    oline(1:) = TRIM(iline(spos:))
    CALL write_output
    
    lpattern_found = .TRUE.

  END SUBROUTINE check_cpreprocessor


  SUBROUTINE check_contains

    ! Check if current line is OMP:
    CALL stringpattern(TRIM(iline_up), contains_pattern, spos, epos)

    ! Exit subroutine if pattern not found:
    IF (spos == 0) RETURN

    CALL stringpattern(TRIM(iline_up), containsw_pattern, spos, epos)

    ! Do not modify current line:
    IF (bl >= 3) oline(:bl-2) = ' '
    IF (bl >= 2) THEN
       oline(bl-1:) = iline(spos:epos)
    ELSE
       oline(:) = iline(spos:epos)
    END IF
    oline = TRIM(iline)
    CALL write_output
    
    lpattern_found = .TRUE.

  END SUBROUTINE check_contains


  SUBROUTINE check_description

    ! Local variable:
    INTEGER :: linelen

    ! Check if there current line starts a subroutine description block:
    spos = INDEX(TRIM(iline), '!******************************************&
         &*************************************')
    IF (spos > 0) ldescription = .TRUE.

    IF (ldescription) THEN

       ! Check if line is a comment:
       CALL stringpattern(TRIM(iline_up), comment_pattern, spos, epos)

       ! If current line is not a comment line, set ldescription back 
       ! to .FALSE. and exit subroutine:
       IF (spos == 0) THEN
          ldescription = .FALSE.
          RETURN
       END IF

       ! Remove bl blanks immediately at the end of the line if possible:
       IF (bl >= 1) oline(1:bl) = ' '
       oline(bl+1:) = TRIM(iline) 
       IF (bl >= 1) THEN
          DO
             linelen = LEN_TRIM(oline)
             IF (linelen <= description_linelen) EXIT

             ! Check if there is a blank immediately before the end of line:
             IF (oline(linelen-1:linelen-1) /= ' ' .AND. &
                  oline(linelen-1:linelen-1) /= '*') EXIT  ! Not a blank or *
    
             ! Remove this blank:
             oline(linelen-1:) = TRIM(oline(linelen:))
          END DO
       END IF
       
       CALL write_output

    END IF

  END SUBROUTINE check_description      


  SUBROUTINE write_output

    WRITE(olun,'(a)') TRIM(oline)

  END SUBROUTINE write_output

END MODULE m_indent_subroutines
