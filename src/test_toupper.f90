! Copyright (c) 2023 MeteoSwiss, contributors listed in AUTHORS
! 
! Distributed under the terms of the BSD 3-Clause License.
!  
! SPDX-License-Identifier: BSD-3-Clause

PROGRAM test

  implicit none

  character (250) :: toupper
  CHARACTER (250) :: iline = '', outline = ''

  iline = '      subroutine advance(pj,itime,ldt,up,vp,wp,usigold,'
  outline = toupper(iline)
  print *, trim(outline)

END PROGRAM test
