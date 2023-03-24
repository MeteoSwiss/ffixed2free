program ffixed2free

use m_ffixed2free,           ONLY: ffixed2free_file
use m_type_declaration,       ONLY: type_declaration
use m_do_loops,               ONLY: do_loops
use m_indent_subroutines,     ONLY: indent_subroutines

implicit none

integer, parameter :: maxstrlen = 1024, tmplun = 11

character(len=maxstrlen) :: infile, outfile
character(len=8), parameter :: tmpfile1 = 'tmp1.f90', tmpfile2 = 'tmp2.f90', &
     tmpfile3 = 'tmp3.f90'
integer             :: len_infilename  ! length of name in variable file

! Command line args
integer :: iarg  ! index of command line argument
integer :: istat ! status
integer :: len_name ! length of file name without extension


! Loop over command line arguments:

do iarg = 1,max(1,command_argument_count())

   ! Input / output file names:

   if (command_argument_count() < 1) then
      print *,'Usage: ffixed2free file [files ...]'
      stop
   else
      call get_command_argument(iarg,infile,len_infilename,istat)
   endif

   len_name = len_trim(infile) - 2
   if (infile(len_name+1:) == '.f') then
      outfile = infile(:len_name)//'.f90'
   else if (infile(len_name+1:) == '.F') then
      outfile = infile(:len_name)//'.F90'
   else
      outfile = trim(infile)//'.f90'
   end if

   ! Call ffixed2free_file:
   write(*,'(a,x,a)') 'Processing',trim(infile)
   call ffixed2free_file(infile, tmpfile1)

   ! Call indent_subroutines:
   call indent_subroutines(tmpfile1, tmpfile2)

   ! Call type_declaration:
   call type_declaration(tmpfile2, tmpfile3)

   ! Call do_loops:
   write(*,'(a,x,a)') 'Writing',trim(outfile)
   call do_loops(tmpfile3, outfile)

end do

! Remove tmpfile1 and tmpfile2:
open(unit=tmplun,file=trim(tmpfile1))
close(tmplun,status='delete')
open(unit=tmplun,file=trim(tmpfile2))
close(tmplun,status='delete')
open(unit=tmplun,file=trim(tmpfile3))
close(tmplun,status='delete')

end program ffixed2free
