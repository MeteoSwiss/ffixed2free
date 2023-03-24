module m_ffixed2free

use m_stringpattern, ONLY: stringpattern

implicit none

integer, parameter :: maxlinelen = 250

! Comment block type
type comment_line
   character(len=maxlinelen)  :: line = ''
   type(comment_line),pointer :: next => NULL()
end type comment_line

! Output line type
type out_line
   character(len=maxlinelen)  :: line = ''
   type(comment_line),pointer :: comment_head => NULL()
   type(comment_line),pointer :: comment_tail => NULL()
end type out_line

! Comment characters
character(len=*),parameter :: comment(6) = &
     (/ 'C', 'c', 'D', 'd', '*', '!' /)

! Compiler directive markers
character(len=*),parameter :: directive(1) = &
     (/ '$OMP' /)

! Input and output lines
character(len=maxlinelen) :: iline  = ''
type(out_line), SAVE      :: oline(2)        ! Output line buffer 
                                             ! SAVE attribute needed for gfortran
                                             ! -> ompiler bug?
integer                   :: curr_src, prev_src
integer                   :: indentcomm      ! Current indent im comment
character(len=1)          :: end_c           ! Last character of comment line
integer                   :: len_c           ! Length of comment line

integer, parameter        :: indentcont = 5  ! Indent of continuation line

logical                   :: lcont           ! Flag for continuation line

integer :: ios, i_line

! Functions:
integer, external :: count_substring


contains
!------------------------------------------------------------------------------
  subroutine ffixed2free_file(infile,outfile)

    implicit none
    ! Dummy arguments
    character(len=*), intent(in) :: infile, outfile

    ! Local variables
    integer :: n_src
    integer, parameter :: ilun = 11, olun = 12
    logical :: is_src

    ! Init
    n_src  = 0

    ! Init module variables
    i_line = 0
    len_c  = 0
    prev_src = 1
    curr_src = 2

    ! Open input and ouput files:
    open(unit=ilun,file=infile,action='read',position='rewind',status='old')
    open(unit=olun,file=outfile,action='write',position='rewind',status='replace')

    ! Loop over lines: 
    do
       ! Read an input line
       read(ilun,'(a)',iostat=ios) iline
       if (ios /= 0) then
          ! No more input lines,
          ! write last line to output and exit loop
          write(olun,'(a)') trim(oline(prev_src)%line)
          call write_comment(olun,oline(prev_src))
          exit
       end if
       i_line = i_line + 1

       call ffixed2free_line(is_src)

       ! Write previous line if new source code line
       if (is_src) then
          n_src = n_src + 1
          if (n_src > 1) then
             write(olun,'(a)') trim(oline(prev_src)%line)
          end if
          call write_comment(olun,oline(prev_src))

          ! Switch line index
          prev_src = curr_src
          curr_src = 3 - prev_src
       end if

    end do

   ! Close files:
   close(ilun)
   close(olun)

  end subroutine ffixed2free_file
!------------------------------------------------------------------------------
  subroutine ffixed2free_line(is_src)
    implicit none

    ! Dummy arguments
    logical :: is_src

    ! Local variables
    integer :: i_bang

    i_bang = index(iline,'!')
    if (any(iline(1:1) == comment)) then
       if (any(iline(2:1+LEN(directive)) == directive)) then
          ! Compiler directive
          is_src = .true.
          call ffixed2free_directive
       else
          ! Comment line
          is_src = .false.
          call ffixed2free_comment
       end if
    else if (iline(1:1) == '#') then
       ! C-preprocessor
       is_src = .true.
       call ffixed2free_cpreprocessor
    else if (iline == '') then
       ! Empty line is comment line by definition
       is_src = .false.
       call ffixed2free_comment
    else if (i_bang > 0 .and. iline(:i_bang-1) == '') then
       ! Comment line with blanks followed by end-of-line comment
       is_src = .false.
       call ffixed2free_comment
    else
       ! Source code line
       is_src = .true.
       call ffixed2free_sourcecode
    end if
  end subroutine ffixed2free_line
!------------------------------------------------------------------------------
  subroutine ffixed2free_comment
    implicit none

    ! Local variables
    integer :: i_bang, i_noblank

    ! Append comment line to previous source code line
    if (associated(oline(prev_src)%comment_tail)) then
       ! Add a comment line
       allocate(oline(prev_src)%comment_tail%next)
       oline(prev_src)%comment_tail => oline(prev_src)%comment_tail%next
    else
       ! Add first comment line
       allocate(oline(prev_src)%comment_head)
       oline(prev_src)%comment_tail => oline(prev_src)%comment_head
       ! Initialize indentcomm for this comment block
       indentcomm = maxlinelen
    end if

    if (iline == ' ') then
       ! Empty line is comment line by definition
       oline(prev_src)%comment_tail%line = ' '
    else if (iline(2:2) == ACHAR(9)) then
       ! Second character is tab sign, remove tab
       ! Move indent ahead of comment sign, but only up to the position
       ! in previous comment
       i_noblank = max(1,min(indentcomm,verify(iline(3:),' ')))
       indentcomm = i_noblank
       oline(prev_src)%comment_tail%line(:i_noblank-1) = ' '
       oline(prev_src)%comment_tail%line(i_noblank:) = '! '//iline(i_noblank+2:)
    else if (iline(2:6) == ' ' .and. .not. &
         (len_trim(iline) == len_c .and. &
         iline(len_trim(iline):len_trim(iline)) == end_c)) then
       ! Comment with indent and not a formatted comment box with
       ! length and last character equal to previous comment
       ! Reduce indent, move indent ahead of comment sign, but only up to the position
       ! in previous comment
       i_noblank = max(1,min(indentcomm,verify(iline(7:),' ')))
       indentcomm = i_noblank
       oline(prev_src)%comment_tail%line(:i_noblank-1) = ' '
       oline(prev_src)%comment_tail%line(i_noblank:) = '! '//iline(i_noblank+6:)
    else
       ! Formatted comment, copy as is
       oline(prev_src)%comment_tail%line = '!'//iline(2:)
    end if
    ! Store length of comment and last character
    len_c = len_trim(oline(prev_src)%comment_tail%line)
    end_c = oline(prev_src)%comment_tail%line(len_c:len_c)

  end subroutine ffixed2free_comment
!------------------------------------------------------------------------------
  subroutine ffixed2free_sourcecode
    implicit none

    ! Local variables
    integer len_lab, len_indent, i_comment, i_bang, i_tab, i_shifted, ipos
    integer :: ipos_cont = 1, n_quote = 0
    character(len=2) :: amp

    ! Remove tabs
    oline(curr_src)%line = iline
    do
      i_bang = index(oline(curr_src)%line,'!')
      if (i_bang == 0) i_bang = len_trim(oline(curr_src)%line) + 1
      i_tab  = index(oline(curr_src)%line(1:i_bang-1),ACHAR(9))
      if (i_tab == 0) EXIT
      i_shifted = ((i_tab - 1) / 8 + 1) * 8 + 1
      oline(curr_src)%line(i_shifted:) = oline(curr_src)%line(i_tab+1:)
      oline(curr_src)%line(i_tab:i_shifted-1) = ''
    end do

    ! Flag for continuation line:
    lcont = (oline(curr_src)%line(1:5) == '' .and. oline(curr_src)%line(6:6) /= '')

    if (oline(curr_src)%line(1:5) == '') then

       ! Normal FORTRAN indent, reduce indent
       oline(curr_src)%line(1:) = oline(curr_src)%line(7:)
       ! Check for continuation line
       if (lcont) then
          ! Continuation line
          ! Check number of string delimiters but only before comment
          i_comment = index(oline(prev_src)%line,'!')
          if (i_comment > 0) then
             n_quote = mod(n_quote + &
                  mod(count_substring(oline(prev_src)%line(1:i_comment-1),"'"),2) + &
                  mod(count_substring(oline(prev_src)%line(1:i_comment-1),'"'),2),2)
             ! Exclamation mark is part of a string, ignore
             if (n_quote /= 0) i_comment = 0
          end if
          if (i_comment == 0) then
             n_quote = mod(n_quote + &
                  mod(count_substring(oline(prev_src)%line,"'"),2) + &
                  mod(count_substring(oline(prev_src)%line,'"'),2),2)
          end if
          ! Add ampersand at end of previous line
          ! No blank before ampersand if it is a string
          if (n_quote /= 0) then
             amp = '&'
          else
             amp = ' &'
          end if
          if (i_comment > 0) then
             oline(prev_src)%line = trim(oline(prev_src)%line(:i_comment-1)) &
                  // amp // ' ' // oline(prev_src)%line(i_comment:)
          else
             oline(prev_src)%line = trim(oline(prev_src)%line) // amp
          end if
          ! If the current line is the continuation of a string, add an
          ! ampersand immediately at the very beginning of the string,
          ! including leading blanks
          if (n_quote /= 0) then
             oline(curr_src)%line(2:) = oline(curr_src)%line(1:)
             oline(curr_src)%line(1:1) = '&'
          end if
          ipos = max(1,verify(oline(curr_src)%line,' '))
          oline(curr_src)%line(ipos_cont+indentcont+1:) = &
               oline(curr_src)%line(ipos:)
          oline(curr_src)%line(1:ipos_cont+indentcont) = ' '
       end if

    else if (verify(oline(curr_src)%line(1:5),' 01234567890') == 0) then

       ! Label present, reduce indent after label
       len_lab = len_trim(oline(curr_src)%line(1:5))
       len_indent = verify(oline(curr_src)%line(7:7+len_lab),' ') - 1
       if (len_indent < 0) len_indent = len_lab
       oline(curr_src)%line = oline(curr_src)%line(1:len_lab)//' '//&
            oline(curr_src)%line(7+len_indent:)

    else

       ! Unrecognized line type, copy without change
       write(*,'(a,i0)') &
            'WARNING: Unrecognized line type,&
            & copied as is, line ',i_line
       oline(curr_src)%line = iline

    end if

    ! Determine indent before continuation line, reset quote count
    if (.not. lcont) then
       ipos_cont = max(1,verify(oline(curr_src)%line,' 0123456789'))
       n_quote = 0
    end if

  end subroutine ffixed2free_sourcecode
!------------------------------------------------------------------------------
  subroutine ffixed2free_directive
    implicit none

    oline(curr_src)%line(1:) = '!'//iline(2:5)//' '//iline(7:)
    if (iline(6:6) /= '') then
       ! Continuation line,
       ! add ampersand at end of previous line
       oline(prev_src)%line = trim(oline(prev_src)%line) // ' &'
    end if

  end subroutine ffixed2free_directive
!------------------------------------------------------------------------------
  subroutine ffixed2free_cpreprocessor
    implicit none

    oline(curr_src)%line(1:) = iline(1:)

  end subroutine ffixed2free_cpreprocessor
!------------------------------------------------------------------------------
  subroutine write_comment(lun,outline)
    implicit none
    integer        :: lun
    type(out_line) :: outline

    type(comment_line),pointer :: comline, comline_prev
    
    if (associated(outline%comment_head)) then
       comline => outline%comment_head
       nullify(outline%comment_head)
       nullify(outline%comment_tail)
 
       do
          write(lun,'(a)') trim(comline%line)
          if (associated(comline%next)) then
             comline_prev => comline
             comline => comline%next
             deallocate(comline_prev)
          else
             exit
          end if
       end do
    end if

  end subroutine write_comment
!------------------------------------------------------------------------------

end module m_ffixed2free
