PROGRAM test

  implicit none

  character (250) :: toupper
  CHARACTER (250) :: iline = '', outline = ''

  iline = '      subroutine advance(pj,itime,ldt,up,vp,wp,usigold,'
  outline = toupper(iline)
  print *, trim(outline)

END PROGRAM test
