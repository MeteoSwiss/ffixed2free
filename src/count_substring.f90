! Copyright (c) 2023 MeteoSwiss, contributors listed in AUTHORS
! 
! Distributed under the terms of the BSD 3-Clause License.
!  
! SPDX-License-Identifier: BSD-3-Clause

FUNCTION count_substring(string, substring)

  ! Description:

  ! Returns the number a <substring> occurs as a substring of <string>. If it
  ! does not occur, zero is returned.

  ! Martin Schraner, MeteoSwiss, 1 November 2011

  IMPLICIT NONE

  ! Dummy arguments:
  CHARACTER(LEN=*), INTENT(in) :: string, substring
  INTEGER :: count_substring

  ! Local variables:
  INTEGER :: ind, inds


  ! Executable statements:

  count_substring = 0
  inds = 0
  ind = 0

  DO
     IF (ind > LEN_TRIM(string)) RETURN
     ind = INDEX (TRIM(string(inds+1:)), TRIM(substring))
     inds = inds + ind
     IF (ind == 0) RETURN
     count_substring = count_substring + 1
  END DO

  ! This line is never called, but it is necessary if the string contains a '.
  ! Compiler bug of gfortran.
  PRINT *, count_substring

END FUNCTION count_substring

  

  
  

     

  
