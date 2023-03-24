MODULE m_do_loops

  ! Description:

  ! Module to rewrite the DO-Loops in f90 style (i.e. END DO at the end of
  ! a loop instead of LABEL + CONTINUE).

  ! M. Schraner, MeteoSwiss, 28 October 2011

  USE m_linked_list_do_loops
  USE m_stringpattern, ONLY: stringpattern

  IMPLICIT NONE

  PRIVATE

  !--- Module variables:

  INTEGER, PARAMETER :: maxlinelen = 250
  INTEGER, PARAMETER :: maxngotos = 1000    ! Maximum allowed number of GO-TOs
  INTEGER, PARAMETER :: ilun = 11, olun = 12    ! Logical file units
  INTEGER, PARAMETER :: indentdo = 2        ! Indent of the body of a DO-loop
  INTEGER, PARAMETER :: indentcont = 5      ! Indent of continuation line

  ! Input and output lines:
  CHARACTER (len=maxlinelen) :: iline  = '', iline_up = ' ', oline = ''

  ! Patterns:
  CHARACTER (len=maxlinelen), PARAMETER :: &
       goto_label_pattern = '[ )]+G[ ]*O[ ]*T[ ]*O[ ]+[0-9]+', &
       goto_pattern = '[ )]+G[ ]*O[ ]*T[ ]*O[ ]+', &
       begin_do_pattern = '^[ ]*D[ ]*O[, ]+[0-9]+[ ]' , &
       begin_do_do_pattern = 'D[ ]*O' , &
       begin_do_label_pattern = '[0-9]+[ ]', &
       end_do_label_pattern = '^[ ]*[0-9]+[ ]', &
       end_do_label_pattern2 = '^[ ]*[0-9]+[ ]*', &
       end_do_continue_pattern = &
       '^[ ]*[0-9]+[ ]+C[ ]*O[ ]*N[ ]*T[ ]*I[ ]*N[ ]*U[ ]*E[ ]*', &
       labeled_enddo_pattern =  '^[ ]*[0-9]+[ ]+END[ ]*DO', &
       ampersand_pattern = '[A-Z]+.*&'

  ! Item of linked list:
  TYPE (linked_list_item), POINTER :: item

  INTEGER, DIMENSION(maxngotos) :: goto_labels
  INTEGER :: ios, linenumber  
  INTEGER :: spos, epos, spos_do, epos_do, spos_label, epos_label, spos_cont, &
       epos_cont, spos_conti, epos_conti, label, lenlabel
  LOGICAL :: lpattern_found, lwrite_end_do, lline_cont

  ! Functions:
  CHARACTER (len=maxlinelen), EXTERNAL :: toupper

  ! Public entities:
  PUBLIC :: do_loops

CONTAINS

  SUBROUTINE do_loops (infile, outfile)

    ! Dummy arguments:
    CHARACTER (LEN=*), INTENT(in) :: infile, outfile
   
    ! Executable statements:

    ! Open input and ouput files:
    OPEN(unit=ilun,file=infile,action='read',position='rewind',status='old')
    OPEN(unit=olun,file=outfile,action='write',position='rewind',status='replace')
   
    ! Check for GO-TO labels:
    CALL check_gotos

    ! Check for DO loops:
    CALL check_do_loops

    ! Close files:
    CLOSE(ilun)
    CLOSE(olun)

  END SUBROUTINE do_loops


  SUBROUTINE check_gotos

    ! Check for GO-TOs and save labels. These labels must not be deleted
    ! when modifying the DO-loops.

    ! Local variable:
    INTEGER :: ind

    ! Executable statements:

    ! Initialize:
    ind = 1              ! Label index
    goto_labels(:) = -1      ! No GOTO-labels available

    ! Loop over all input lines:
    DO

       ! Read an input line
       READ (ilun,'(a)',iostat=ios) iline

       ! Exit at the end of file:
       IF (ios /= 0) THEN
          REWIND ilun    ! Jump back to the beginning of the input file
          EXIT
       END IF

       ! Convert iline to UPPERCASE:
       iline_up = toupper(iline)

       ! Check for GO-TO at current line:
       CALL stringpattern &
            (TRIM(iline_up), goto_label_pattern, spos, epos_label)

       IF (spos > 0) THEN

          ! Get GO-TO label:
          CALL stringpattern (TRIM(iline_up), goto_pattern, spos, spos_label)

          ! Label as integer:
          READ(iline(spos_label+1:epos_label),*) goto_labels(ind)

          ind = ind + 1

          IF (ind > maxngotos) THEN
             PRINT *, 'Maximum number of GOTOs per file exeeded'
             STOP
          END IF

       END IF

    END DO

  END SUBROUTINE check_gotos


  SUBROUTINE check_do_loops

    ! Check for DO-loops and modify them to FORTRAN 90 style.

    ! Initialize:
    linenumber = 0              ! Line number
    lwrite_end_do = .FALSE.     ! Flag if END DO(s) has/have to be written
    lline_cont = .FALSE.        ! Line is continuation of previous line
                                ! (can only be set to .TRUE. at the end of 
                                ! a DO-loop)

    ! Loop over all input lines:
    DO

       lpattern_found = .FALSE.
       linenumber = linenumber + 1

       ! Read an input line
       READ (ilun,'(a)',iostat=ios) iline
       
       ! Exit at the end of file:
       IF (ios /= 0) THEN
          IF (lwrite_end_do) CALL write_end_do
          EXIT
       END IF

       ! Convert iline to UPPERCASE:
       iline_up = toupper(iline)

       ! Check if END DO(s) has/have to be written:
       IF (lwrite_end_do) THEN 
          IF (.NOT.(lline_cont)) THEN
             ! Line is not continuation of previous line -> write END DO(s)
             CALL write_end_do
             lwrite_end_do = .FALSE.
          ELSE
             ! Line is continuation of previous line -> do not write END DO(s)

             ! Check for trailing ampersand at current line:
             CALL stringpattern (TRIM(iline_up), ampersand_pattern, spos, epos)
             IF (spos == 0) lline_cont = .FALSE.  ! Write output at next 
                                                  ! iteration
             ! Re-write continuation line with indent:
             CALL stringpattern (TRIM(iline), '^[ ]*', spos, epos)
             oline(1:item%spos+indentdo+indentcont-1) = ' '
             oline(item%spos+indentdo+indentcont:) = TRIM(iline(epos+1:))
             CALL write_output
             CYCLE
          END IF
       END IF

       ! Check if line contains beginning of a DO-loop and modify line if 
       ! succesful:
       CALL check_begin_do
       IF (lpattern_found) CYCLE

       ! Check if line contains end of a DO-loop:         
       CALL check_end_do
       IF (lpattern_found) CYCLE 

       ! Rewrite line:
       oline = TRIM(iline)
       CALL write_output

    END DO

  END SUBROUTINE check_do_loops


  SUBROUTINE check_begin_do
      
    ! Check and modify beginning of DO-loop.

    ! Local variable:
    LOGICAL :: luppercase

    ! Check for DO with label:
    CALL stringpattern(TRIM(iline_up), begin_do_pattern, spos, epos)
    
    ! Exit subroutine if pattern not found:
    IF (spos == 0) RETURN

    ! Was DO originally written in uppercase?
    CALL stringpattern(TRIM(iline), begin_do_pattern, spos, epos)
    IF (spos == 0) THEN
       luppercase = .FALSE.
    ELSE
       luppercase = .TRUE.
    END IF
           
    ! Determine position where DO starts / ends:
    CALL stringpattern &
         (TRIM(iline_up), begin_do_do_pattern, spos_do, epos_do)
    
    ! Determine position where label starts / ends:
    CALL stringpattern &
         (TRIM(iline_up), begin_do_label_pattern, spos_label, epos_label)
    
    ! Label as integer:
    READ(iline(spos_label:epos_label-1),*) label
    
    ! Save label and start position of DO in new linked list item:
    CALL new_item(label, linenumber, spos_do, luppercase, item)
    CALL add_at_tail(item)
    
    ! Remove label in oline:
    oline(1:epos_do) = iline(1:epos_do)
    oline(epos_do+1:) = TRIM(iline(epos_label:))

    ! Write line into output file:
    CALL write_output

    lpattern_found = .TRUE.

  END SUBROUTINE check_begin_do

  
  SUBROUTINE check_end_do

    ! Check and modify end of DO-loop.

    ! Local variables:
    CHARACTER (LEN=5) :: label_st
    INTEGER :: spos_label2, epos_label2
    LOGICAL :: luppercase

    ! Check for label:
    CALL stringpattern &
         (TRIM(iline_up), end_do_label_pattern, spos_label, epos_label)

    ! Exit subroutine if pattern not found:
    IF (spos_label == 0) RETURN

    ! Label as integer:
    READ(iline(spos_label:epos_label-1),*) label

    ! Get item at tail of linked list:
    CALL get_at_tail(item)
    
    ! Exit if label does not fit:
    IF (ASSOCIATED(item)) THEN
       IF (item%label /= label) RETURN
    ELSE
       RETURN
    END IF

    ! Check if it is a labeled CONTINUE, a labeled END DO or simply a labeled
    ! command line. In the latter case, re-write line without the label, 
    ! in the former two cases, do not write any output: 
    CALL stringpattern &
         (TRIM(iline_up), end_do_continue_pattern, spos_cont, epos_cont)

    IF (spos_cont == 0) THEN    ! No CONTINUE found

       ! Check if it is a labeled END DO:
       CALL stringpattern (TRIM(iline_up), labeled_enddo_pattern, spos, epos)

       IF (spos == 0) THEN ! No CONTINUE or END DO found -> labeled command line

          CALL stringpattern &
               (TRIM(iline_up), end_do_label_pattern2, spos_label2, epos_label2)

          ! Replace label with blanks and write output:
          oline(:item%spos+indentdo-1) = ' '
          oline(item%spos+indentdo:) = TRIM(iline(epos_label2+1:))
          CALL write_output
          
          ! Check for trailing ampersand (-> line continuation):
          CALL stringpattern (TRIM(iline_up), ampersand_pattern, spos, epos)
          IF (spos > 0) lline_cont = .TRUE.

       END IF

    END IF

    ! Check if there is a GOTO pointing to the current label and insert a
    ! labeled CONTINUE if it is the case:
    IF (ANY(label == goto_labels)) THEN
       lenlabel = epos_label - spos_label   ! number of digits of the label

       spos_cont = item%spos + indentdo
       luppercase = item%luppercase

       ! Insert labeled CONTINUE:
       WRITE(label_st, '(i5)') label
       spos_cont = MAX(spos_cont, lenlabel + 2)

       oline = ' '
       oline(:lenlabel) = label_st(5-lenlabel+1:5)
       IF (luppercase) THEN
          oline(spos_cont:spos_cont+7) = 'CONTINUE'
       ELSE
          oline(spos_cont:spos_cont+7) = 'continue'
       END IF
       CALL write_output
    END IF

    lpattern_found = .TRUE.
    lwrite_end_do = .TRUE.

  END SUBROUTINE check_end_do


  SUBROUTINE write_end_do

    ! Write END DO(s).

    ! Local variables:
    LOGICAL :: luppercase

    ! Backwards loop over linked list:
    DO

       ! Get item at tail of linked list:
       CALL get_at_tail(item)
       
       ! Exit loop if label does not fit:
       IF (ASSOCIATED(item)) THEN
          IF (item%label /= label) EXIT
       ELSE    ! List is empty
          EXIT
       END IF

       ! Write END DO:
       oline = ''
       oline(:item%spos-1) = ' '
       IF (item%luppercase) THEN
          oline(item%spos:item%spos+5) = 'END DO'
       ELSE
          oline(item%spos:item%spos+5) = 'end do'
       END IF
       CALL write_output

       ! Delete item:
       CALL get_and_delete_at_tail(item)

    END DO
      
  END SUBROUTINE write_end_do


  SUBROUTINE write_output

    WRITE(olun,'(a)') TRIM(oline)

  END SUBROUTINE write_output

END MODULE m_do_loops

